;;; udisksctl.el --- Interface to udisksctl -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Toni Schmidbauer

;; Author: Toni Schmidbauer <toni@stderr.at>
;; Created: 11 June 2013
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/tosmi/emacs-udisksctl
;; Package-Requires: ((emacs "27.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; This is a major mode to interact with udisksctl.  It allows you to
;; see devices, unlock/lock encrypted device and mounting/unmounting
;; devices via udisksctl

;; Operating Systems:
;; Developped under Linux.  Should work on all OS'es
;; that support the udisksctl command.


;;; Code:

;;; Todo
;; - maybe the functions to mount/unmount/lock/unlock could be
;;   defined via defmacro?

(require 'text-property-search)
(require 'ert)
(require 'auth-source)

(defgroup udisksctl nil
  "The udisksctl interface."
  :group 'applications)

(defvar udisksctl-buffer-name "*udisksctl*")

(defcustom udisksctl-status-cmd "status"
  "Parameter for udisksctl to get the status information."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-mount-cmd "mount"
  "Parameter for udisksctl to mount a device."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-unmount-cmd "unmount"
  "Parameter for udisksctl to unmount a device."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-unlock-cmd "unlock"
  "Parameter for udiskctl to unlock a device."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-lock-cmd "lock"
  "Parameter for udisksctl to lock a device."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-info-cmd "info"
  "Parameter for udisksctl to lock a device."
  :type 'string
  :group 'udisksctl)

(defcustom udisksctl-dump-cmd "dump"
  "Parameter for udisksctl to dump all device information."
  :type 'string
  :group 'udisksctl)

(defvar udisksctl-process-buffer-name "*udisksctl-process*")
(defconst udisksctl-list-buffer-name "*udisksctl-list*"
  "Name of the list device buffer.")
(defvar udisksctl-process nil)
(defvar udisksctl--process-device nil
  "The device associated with the current process.")
(defvar udisksctl-device nil)

(defvar udisksctl-device-alist nil
  "An alist for each device and their properties.
This variable is used as a temporal data to avoid calling the dump
command many times during processing.

The format is as follows:

  (((\"name\" UDISK-NAME)
    (\"sections\" ((\"type\" \"org.freedesktop.UDisk2.Block\") ...)
                  ((\"type\" \"org.freedesktop.UDisk2.Filename\") ...)
                  ...))
   ...)

Use `udisksctl-update-device-alist' function to update this variable.")

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "u" #'udisksctl-unlock-at-point)
    (define-key map "l" #'udisksctl-lock-at-point)
    (define-key map "m" #'udisksctl-mount-at-point)
    (define-key map "U" #'udisksctl-unmount-at-point)
    (define-key map "i" #'udisksctl-info-at-point)
    (define-key map "f" #'udisksctl-find-file-at-point)
    (define-key map "g" #'udisksctl-list)
    map)
  "Keymap for `udisksctl-mode'.")

(define-derived-mode udisksctl-mode special-mode
  "Udisksctl"
  "Major mode for udisksctl.
Shows status information about disks via udisksctl.

Keybindings:
\\{udisksctl-mode-map}"
  (kill-all-local-variables)
  (use-local-map udisksctl-mode-map)
  (setq major-mode 'udisksctl-mode
	buffer-read-only t))

;; ;; For debugging purposes only!
;;(setq debug-on-error nil)

(defun udisksctl-elt-devices (elt-data)
  "Return all device names from an element in ELT-DATA.
ELT-DATA is an alist element retrieved from the list in
`udisksctl-device-alist'."
  (flatten-list
   (mapcar (lambda (section)
             (alist-get "Device" section nil nil #'string-equal))
           (car (alist-get "sections" elt-data nil nil #'string-equal)))))

(defun udisksctl-device-search-data (block-path)
  "Search on `udisksctl-device-alist' a device data from the BLOCK-PATH."
  (cl-find-if (lambda (elt)
                (member block-path (udisksctl-elt-devices elt)))
              udisksctl-device-alist))

(defun udisksctl-block-to-uuid (block-path)
  "Return the UUID from the BLOCK-PATH.
Search in `udisksctl-device-alist' the given BLOCK-PATH and return the UUID."
  (car
   (mapcar (lambda (section)
             (car (alist-get "IdUUID" section nil nil #'string-equal)))
           (car (alist-get "sections" (udisksctl-device-search-data block-path)
                           nil nil #'string-equal)))))

(defun udisksctl-execute-cmd (cmd device &optional noerase)
  "Execute CMD on DEVICE, does not require user input.
If NOERASE is specified the output buffer will not be erased."
  (setq udisksctl--process-device device)
  (let ((process-connection-type t))
    (get-buffer-create  udisksctl-process-buffer-name)
    (with-current-buffer udisksctl-process-buffer-name
      (if noerase
	  (goto-char (point-max))
	(erase-buffer))
      (setq udisksctl-process
	    (start-process "udisksctl" udisksctl-process-buffer-name "udisksctl" cmd "-b" device)))
    (set-process-filter udisksctl-process 'udisksctl-process-filter)
    (set-process-sentinel udisksctl-process 'udisksctl-process-sentinel)
    (while (equal (process-status udisksctl-process) 'run)
      (sit-for 0.1 t))
    (if (not (equal (process-exit-status udisksctl-process) 0))
	(udisksctl-error-message)
      (progn
	(udisksctl-success-message)
	(udisksctl-remember-mounts-and-mappings)))
    (udisksctl-refresh-buffer)))

(defun udisksctl-error-message ()
  "Signal an elisp error when the udiskctl process does."
  (error "%s" (or (with-current-buffer (get-buffer udisksctl-process-buffer-name)
		    (goto-char (point-min))
		    (or
		     (re-search-forward "^\\([^[:space:]].*?\\)\\.$" nil t))
		    (match-string 1))
		  "udiskctl failed")))

(defun udisksctl-success-message()
  "Show a success message when the udiskctl process does."
  (message "%s" (with-current-buffer (get-buffer udisksctl-process-buffer-name)
		  (goto-char (point-min))
		  (re-search-forward "^\\(.*\\)$" nil t)
		  (match-string 1))))

(defvar udisksctl-status-list nil
  "An alist of devices and ids.")

(defun udisksctl-remember-mounts-and-mappings ()
  "Find a device name from the process output buffer and store it.
If found, save the device name to `udisksctl-status-list'."
  (with-current-buffer (get-buffer udisksctl-process-buffer-name)
    (goto-char (point-min))
    (cond ((or
	        (re-search-forward "Unlocked \\([^[:space:]]+\\) as \\([^[:space:]]+\\)\." nil t)
	        (re-search-forward "Mounted \\([^[:space:]]+\\) at \\([^[:space:]]+\\)\.?" nil t))
	       (add-to-list 'udisksctl-status-list (cons (match-string 1) (match-string 2))))
          ((or
	        (re-search-forward "Unmounted \\([^[:space:]]+\\)\." nil t)
	        (re-search-forward "Locked \\([^[:space:]]+\\)\." nil t))
	       (udisksctl-remove-mapping (match-string 1))))))

(defun udisksctl-remove-mapping (searchkey)
  "Search for SEARCHKEY in the status list."
  (let ((key (assoc searchkey udisksctl-status-list)))
    (setq udisksctl-status-list (assq-delete-all (car key) udisksctl-status-list))))

(defun udisksctl-print-alist (list format)
  "Print the assoc LIST with the FORMAT specified."
  (when list
    (let ((device (car (car list)))
	  (dmdevice (cdr (car list))))
      (insert (format format device dmdevice))
      (udisksctl-print-alist (cdr list) format))))

(defun udisksctl-get-password-from-auth-source (uuid)
  "Get the password from auth-source.
This is used to search the password for LUKS devices in KWallet or other
programs.  A special auth-source backend should be configured in Emacs.

UUID is the disk identifier to search on auth-source."
  (let ((secret (auth-source-search :label uuid :folder "SolidLuks")))
    (when secret
      (plist-get (car secret) :secret))))
      
(defun udisksctl--get-password (uuid)
  "Retrieve password to unlock UUID disk from different sources.
Use:

1.  auth-source (KWallet and others).

Lastly, ask the user."
  (or (udisksctl-get-password-from-auth-source uuid)
      (read-passwd "Passphrase: " nil)))

(defun udisksctl-process-filter (proc string)
  "Filter udisksctl output for a password prompt.
PROC is the process object.
STRING is the process output to filter."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (if (string-match "^Passphrase: " string)
	(process-send-string proc
                             (concat
                              (udisksctl--get-password (udisksctl-block-to-uuid udisksctl--process-device))
                              "\n"))
      (save-excursion
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point))
	(goto-char (process-mark proc))))))

(defun udisksctl-process-sentinel (proc event)
  "Sentinel for the udiskctl process.
PROC is the process being watched.
EVENT is the event that triggered the sentinel."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert event "\n"))))

;; ;; Not used!
;; (defun udisksctl-parse-output (udisksctl-proc)
;;   (save-current-buffer
;;     (set-buffer (process-buffer udisksctl-proc))))

;; (defmacro udiskctl-cmd (cmd &optional device)
;;   "Run the given udisksctl CMD on DEVICE."
;;   `(interactive)
;;   `(if (not device)
;;        `(setq udisksctl-device (udisksctl-read-device "Enter device name to lock: "))
;;      `(setq udisksctl-device 'device))
;;   `(udisksctl-execute-cmd udisksctl-(cmd)-cmd udisksctl-device))

;; (macroexpand '(udiskctl-cmd "unlock" "/dev/sde1"))

;; (udiskctl-cmd )

(defun udisksctl-unlock (&optional device)
  "Call the udiskctl unlock command.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to unlock: "))))
    (udisksctl-execute-cmd udisksctl-unlock-cmd udisksctl-device)))

(defun udisksctl-lock (&optional device)
  "Call the udiskctl lock command.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to lock: "))))
    (udisksctl-execute-cmd udisksctl-lock-cmd udisksctl-device)))

(defun udisksctl-mount (&optional device)
  "Mount a device using udisksctl.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to mount: "))))
    (udisksctl-execute-cmd udisksctl-mount-cmd udisksctl-device)))

(defun udisksctl-unmount (&optional device)
  "Umount a device using udiskctl.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to unmount: "))))
    (udisksctl-execute-cmd udisksctl-unmount-cmd udisksctl-device)))

(defun udisksctl-info (&optional device)
  "Call udisksctl info command on the given DEVICE.
Ask for the device if not provided.  DEVICE must be a block file name,
for example: /dev/sda1."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to unmount: "))))
    (with-current-buffer (get-buffer-create udisksctl-buffer-name)
      (erase-buffer)
      (call-process "udisksctl" nil udisksctl-buffer-name nil
                    udisksctl-info-cmd "-b" udisksctl-device)
      (switch-to-buffer (current-buffer)))))

(defun udisksctl-read-device (&optional message)
  "Read a device name from minibufer to work on (mount, unlock ...).
MESSAGE is the string prompt to use.  Device name are files under
 /dev directory.  For example: /dev/sda."
  (if (boundp 'message)
      (read-string message)
    (read-string "Enter device name: ")))

(defun udisksctl--call-status-cmd ()
  "Call the status command and print it at the udiskctl buffer."
  (call-process "udisksctl" nil udisksctl-buffer-name nil
		udisksctl-status-cmd)
  (udisksctl-print-status))

(defun udisksctl-print-status ()
  "Print the status at the udiskctl buffer.
Print the status stored at `udisksctl-status-list' at the udiskctl
buffer."
  (with-current-buffer (get-buffer udisksctl-buffer-name)
    (goto-char (point-max))
    (unless (equal udisksctl-status-list nil)
	  (insert "\nDevice mappings\n---------------\n")
	  (udisksctl-print-alist udisksctl-status-list "%s mapped to %s\n"))))

(defun udisksctl-refresh-buffer ()
  "Update the udiskctl buffer."
  (interactive)
  (with-current-buffer (get-buffer udisksctl-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (udisksctl--call-status-cmd))))

(defun udisksctl-create-buffer ()
  "Create the udisksctl buffer."
  (unless (buffer-live-p (get-buffer udisksctl-buffer-name))
    (get-buffer-create udisksctl-buffer-name)
    (udisksctl-refresh-buffer))
  (switch-to-buffer udisksctl-buffer-name)
  (udisksctl-mode))

;;;###autoload
(defun udisksctl-status()
  "Run udiskctl status and show it in a new buffer."
  (interactive)
  (udisksctl-create-buffer))

;; --------------------------------------------------
;; Parsing the dump output
;; --------------------------------------------------

(defun udisksctl--append-lists (alist)
  "Append unitary lists in ALIST with the cons found before.

This is to fix a problem when parsing dump output lists.  For
instance, the following text:

    Symlinks:          /dev/disk/by-id/abcd1
                       /dev/disk/by-id/abcd2
                       /dev/mapper/luks-abcd1

Would generate the following parsed alist:

  (...
     (\"Symlinks\" \"/dev/disk/by-id/abcd\")
    (\"/dev/disk/by-id/abcd2\")
    (\"/dev/mapper/luks-abcd1\")
   ...)

This function appends the unitary lists with the first cons:

  (...
     (\"Symlinks\" (\"/dev/disk/by-id/abcd\"
                    \"/dev/disk/by-id/abcd2\"
                    \"/dev/mapper/luks-abcd1\"))
   ...)

Return an alist properly formatted."
  (let ((result nil)
        (prev nil))
    (seq-do (lambda (c)
              (if (eq (length c) 1)
                  (setq prev (list (car prev)
                                   (append (if (listp (cadr prev))
                                               (cadr prev)
                                             (cdr prev))
                                           c)))
                (progn (when prev
                         (setq result (push prev result)))
                       (setq prev c))))
            alist)
    (when prev
      (setq result (push prev result)))
    (seq-reverse result)))

(ert-deftest udisksctl--append-lists-test ()
  (should (equal '(("test1" "test2")
                   ("hello" ("world1" "world2" "world3"))
                   ("a1" "a2"))
                 (udisksctl--append-lists '(("test1" "test2")
                                           ("hello" "world1") ("world2") ("world3")
                                           ("a1" "a2"))))))

(defun udisksctl--parse-block (string)
  "Return an alist for the parsed information in STRING .
STRING should be a block from the dump output."
  (let ((alist (udisksctl--append-lists
                (mapcar (lambda (s)
                          (split-string s ":" nil "[[:space:]]*"))
                        (split-string string "\n" t)))))
    (append (list (list "type" (concat "org.freedesktop" (caar alist))))
            (seq-drop alist 1))))

(defun udisksctl--parse-section (string)
  "Parse the STRING data insde a section selecting the proper type.
Determine the type from STRING, and call the proper parse function
for it."
  (cond ((string-prefix-p ".UDisks2.Block" string)
         (udisksctl--parse-block string))
        ((string-prefix-p ".UDisks2.Filesystem" string)
         ;; The function udisksctl--parse-filesystem would be the same
         ;; as this one:
         (udisksctl--parse-block string))
        ;; ((string-prefix-p ".UDisks2.Management" string)
        ;; (udisksctl--parse-management string))
        (t nil)))

(defun udisksctl--parse-udisk (string)
  "Parse a dump section.
A dump section contains several Management, Block or Filesystem type
of data inside.
STRING is the dumped section."
  (let ((lst-data (split-string string "  org.freedesktop" nil)))
    (append (list (list "name" (car lst-data)))
            (list (list "sections"
                        (mapcar #'udisksctl--parse-section
                                (seq-drop lst-data 1)))))))

(defun udisksctl--parse-dump-string (string)
  "Parse a complete dump output.
Parse the whole dump output string into an alist.
STRING is the complete udisksctl dump output."
  (mapcar #'udisksctl--parse-udisk (split-string string "\n\n" t "[[:space:]]")))
  
(defun udisksctl--parse-dump ()
  "Parse the dump information from the current buffer."
  (udisksctl--parse-dump-string
   (buffer-substring-no-properties (point-min) (point-max))))
  
(defun udisksctl--call-dump ()
  "Call the dump command and parse it."
  (with-current-buffer (get-buffer-create udisksctl-buffer-name)
    (erase-buffer)
    (call-process "udisksctl" nil udisksctl-buffer-name nil
                  udisksctl-dump-cmd)
    (udisksctl--parse-dump)))

(defun udisksctl-device-block-p (device-data)
  "Return t if DEVICE-DATA is a device block type of information."
  (string-suffix-p "Block"
                   (car (alist-get "type" device-data nil nil #'string=))))

(defun udisksctl-device-filesystem-p (device-data)
  "Return t if DEVICE-DATA is a device block type of information."
  (string-suffix-p "Filesystem"
                   (car (alist-get "type" device-data nil nil #'string=))))

;; --------------------------------------------------
;; Main function
;; --------------------------------------------------

(defface udisksctl-device-face
  '((t (:height 1.2 :inherit (bold))))
  "Face used for device names."
  :group 'udisksctl)

(defface udisksctl-readonly-face
  '((t (:box (:line-width (2 . 2)
              :color "grey75"
              :style flat-button))))
  "Face used for device names."
  :group 'udisksctl)

(defface udisksctl-label-face
  '((t (:inherit (italic))))
  "Face used for device names."
  :group 'udisksctl)

(defun udisksctl--add-string-property (property-type data string)
  "Add a set of properties to the given STRING.
There are several sets of properties with names.  Given the
PROPERTY-TYPE name, assign that set of properties.
Possible values of PROPERTY-TYPE are symbols such as: device,
readonly, label."
  
  (cond ((eq property-type 'device)
         (propertize string
                     'face 'udisksctl-device-face
                     'udisks-type 'device
                     'udisks-data data))
        ((eq property-type 'readonly)
         (propertize string
                     'face 'udisksctl-readonly-face
                     'udisks-type 'readonly
                     'udisks-data data))
        ((eq property-type 'readonly)
         (propertize string
                     'face 'udisksctl-label-face
                     'udisks-type 'label
                     'udisks-data data))
        (t string)))

(defun udisksctl--insert-block (device-data &optional udisk-data)
  "Insert device information for user consumption.
Format a string and insert the information from DEVICE-DATA to the
current buffer."
  (insert (format "%s %s %s 🏷️\"%s\""
                  (udisksctl--add-string-property
                   'device
                   udisk-data
                   (car (alist-get "Device" device-data nil nil #'string=)))
                  (car (alist-get "IdType" device-data nil nil #'string=))
                  (if (string= "true" (car (alist-get "ReadOnly" device-data nil nil #'string=)))
                      "🔒"
                    "✍️")
                  (car (alist-get "IdLabel" device-data nil nil #'string=)))))

(defun udisksctl--insert-filesystem (device-data &optional udisk-data)
  "Insert filesystem information for user consumption.
DEVICE-DATA is the filesystem parsed information."
  (insert (format " | %s "
                  (propertize                   
                   (car (alist-get "MountPoints" device-data nil nil #'string=))
                   'udisk-data udisk-data))))

(defun udisksctl--insert-section (section-data &optional udisk-data)
  "Insert the section data for user consumption.
Call the proper function depending on the type of the section.
SECTION-DATA is a parsed section from the dump.
Add UDISK-DATA to the string property udisk-data at the inserted
string."
  (cond ((udisksctl-device-block-p section-data)
         (udisksctl--insert-block section-data udisk-data))
        ((udisksctl-device-filesystem-p section-data)
         (udisksctl--insert-filesystem section-data udisk-data))))

(defun udisksctl--insert-udisk (udisk-data)
  "Insert udisk and their sections data into current buffer.
Format a string and insert it for user consumption.
UDISK-DATA is a parsed udisk information."
  (mapc (lambda (section-data)
          (udisksctl--insert-section section-data udisk-data))
        (car (alist-get "sections" udisk-data nil nil #'string=)))
  (insert "\n"))
                         
(defun udisksctl-update-device-alist ()
  "Update variables with new device information from udisksctl."
  (setq udisksctl-device-alist (udisksctl--call-dump)))

(defun udisksctl-list (&optional no-update)
  "Show an interactive buffer with a list of devices.
If NO-UPDATE is t, the do not update the device list (do not call
udisksctl dump)."
  (interactive "P")
  (unless no-update
    (udisksctl-update-device-alist))
  (with-current-buffer (get-buffer-create udisksctl-list-buffer-name)
    (let ((inhibit-read-only t))
      (udisksctl-mode)
      (erase-buffer)
      (mapc #'udisksctl--insert-udisk udisksctl-device-alist))
    (switch-to-buffer (current-buffer))))

(defun udisksctl-mount-at-point ()
  "Mount the device at the current line.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Mounting %s" device-name)
               (udisksctl-mount device-name))
      (message "No device name found at point."))))

(defun udisksctl-unmount-at-point ()
  "Unmount the device at the current line.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Unmounting %s" device-name)
               (udisksctl-unmount device-name))
      (message "No device name found at point."))))

(defun udisksctl-lock-at-point ()
  "Lock the device at the current line.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Locking %s" device-name)
               (udisksctl-lock device-name))
      (message "No device name found at point."))))

(defun udisksctl-unlock-at-point ()
  "Mount the device at the current line.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Unlocking %s" device-name)
               (udisksctl-unlock device-name))
      (message "No device name found at point."))))

(defun udisksctl-info-at-point ()
  "Mount the device at the current line.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Info %s" device-name)
               (udisksctl-info device-name))
      (message "No device name found at point."))))

(defun udisksctl--find-section (section-type udisk-data)
  "Search a specific section type and return its data.
Search for the section with SECTION-TYPE type inside UDISK-DATA
sections and return its data.

SECTION-TYPE can be \"org.freedesktop.UDisks2.Filesystem\" for
filesystems section, or \"org.freedesktop.UDisks2.Block\" for block
information."
  (seq-find (lambda (section-data)
              (string= (car (alist-get "type" 
                                       section-data
                                       nil nil #'string=))
                       section-type))
          (car (alist-get "sections" udisk-data nil nil #'string=))))

(defun udisksctl-find-file-at-point ()
  "Open Dired with the mounted directory at current position."
  (interactive)
  (let ((udisks-data (udisksctl--find-device-data-at-point)))
    (when udisks-data
      (find-file 
       (car (alist-get "MountPoints"
                       (udisksctl--find-section "org.freedesktop.UDisks2.Filesystem"
                                               udisks-data)
                       nil nil #'string=))))))
  
(defun udisksctl--find-device-name ()
  "Find the device name near the current point."
  (save-excursion
    (goto-char (line-end-position))
    (let ((pmatch (text-property-search-backward 'udisks-type 'device t)))
      (when (and pmatch
                 (<= (line-beginning-position) (prop-match-beginning pmatch))
                 (<= (prop-match-end pmatch) (line-end-position)))
        (buffer-substring-no-properties (prop-match-beginning pmatch)
                                        (prop-match-end pmatch))))))

(defun udisksctl--find-device-data-at-point ()
  "Find device data properties at current point."
  (save-excursion
    (let ((pmatch (text-property-search-backward 'udisks-type 'device t)))
      (when (and pmatch
                 (<= (line-beginning-position) (prop-match-beginning pmatch))
                 (<= (prop-match-end pmatch) (line-end-position)))
        (get-text-property (prop-match-beginning pmatch) 'udisks-data)))))
    

(provide 'udisksctl)
;;; udisksctl.el ends here
