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

(defcustom udisksctl-poweroff-cmd "power-off"
  "Parameter for udisksctl to power off a device."
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

(defcustom udisksctl-use-auth-source-passwords t
  "Use auth-source library and `auth-source-search' to retrieve disks passwords."
  :type 'boolean
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
    (define-key map "P" #'udisksctl-poweroff-at-point)
    (define-key map "u" #'udisksctl-unlock-at-point)
    (define-key map "l" #'udisksctl-lock-at-point)
    (define-key map "m" #'udisksctl-mount-at-point)
    (define-key map "U" #'udisksctl-unmount-at-point)
    (define-key map "i" #'udisksctl-info-at-point)
    (define-key map "f" #'udisksctl-find-file-at-point)
    (define-key map "g" #'udisksctl-list)
    (define-key map "\r" #'udisksctl-find-file-at-point)
    (define-key map "\n" #'udisksctl-find-file-at-point)
    map)
  "Keymap for `udisksctl-mode'.")

(define-derived-mode udisksctl-mode tabulated-list-mode
  "Udisksctl"
  "Major mode for udisksctl.
Shows status information about disks via udisksctl.

Keybindings:
\\{udisksctl-mode-map}"
  (kill-all-local-variables)
  (use-local-map udisksctl-mode-map)
  (setq tabulated-list-format [(" " 2 nil)
                               ("dev" 15 t)
                               ("type" 10 t)
                               ("rw" 2 nil)
                               ("size" 15 nil)
                               ("tag" 15 t)
                               ("mount" 15 t)])
  (setq tabulated-list-sort-key nil)
  (setq tabulated-list-use-header-line t)
  (tabulated-list-init-header)
  (visual-line-mode -1) ;; visual line mode make mountpoints look below the line.
  (add-hook 'tabulated-list-revert-hook #'udisksctl-list nil t))

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
      (let ((pass (plist-get (car secret) :secret)))
        (if (functionp pass)
            (funcall pass)
          pass)))))
      
(defun udisksctl--get-password (uuid)
  "Retrieve password to unlock UUID disk from different sources.
Use:

1.  auth-source (KWallet and others) see `udisksctl-use-auth-source-passwords'.

Lastly, ask the user."
  (or (and udisksctl-use-auth-source-passwords (udisksctl-get-password-from-auth-source uuid))
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

(defun udisksctl-poweroff (&optional device)
  "Call the udisksctl power-off command.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (let ((udisksctl-device (or device
                              (udisksctl-read-device "Enter device name to power-off: "))))
    (udisksctl-execute-cmd udisksctl-poweroff-cmd udisksctl-device)))

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
                              (udisksctl-read-device "Enter device name to info: "))))
    (with-current-buffer (get-buffer-create udisksctl-buffer-name)
      (erase-buffer)
      (call-process "udisksctl" nil udisksctl-buffer-name nil
                    udisksctl-info-cmd "-b" udisksctl-device)
      (switch-to-buffer (current-buffer)))))

(defun udisksctl-read-device (&optional message)
  "Read a device name from minibufer to work on (mount, unlock ...).
MESSAGE is the string prompt to use.  Device name are files under
 /dev directory.  For example: /dev/sda."
  (read-string (or message "Enter device name: ")))

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

(defun udisksctl--file-size (section-data)
  "Return the size in human readable string if exists.
SECTION-DATA is an udiskstl parsed section."
  (let ((size-str (car (alist-get "Size" section-data nil nil #'string=))))
    (if size-str
        (file-size-human-readable (string-to-number size-str))
      "no info")))

(defun udisksctl-section-data-to-entries (section-data)
  "From an udisksctl section, generate a tabulated-list enry.
SECTION-DATA is a parsed section from the udisksctl-cli command."
  (when (cdr section-data)
    (list section-data (vector
                        (if (equal (car (alist-get 'type section-data)) 'mount)
                            "├m"
                          "┌d")
                        (or (car (alist-get "Device" section-data nil nil #'string=)) "")
                        (or (car (alist-get "IdType" section-data nil nil #'string=)) "")
                        (if (string= "true" (car (alist-get "ReadOnly" section-data nil nil #'string=)))
                            "🔒"
                          "✍️")
                        (udisksctl--file-size section-data)
                        (or (car (alist-get "IdLabel" section-data nil nil #'string=))
                            "")
                        (or (car (alist-get "MountPoints" section-data nil nil #'string=))
                            "")))))

(defun udisksctl-udisk-data-to-entries (udisk-data)
  "Generate a list of tabulated-list entries from UDISKS-DATA.
UDISK-DATA must be one of the items in the list data parsed from udisksctl-cli.
It must have a \"sections\" element.

See `udisksctl-device-alist' for an example."
  (let ((previous-section nil))
    (delq nil
          (mapcar
           (lambda (section-data)
             "Use SECTION-DATA if it has Device key or PREVIOUS-SECTION otherwise."
             (udisksctl-section-data-to-entries (if (alist-get "Device" section-data nil nil #'string=)
                                                    (progn (setq previous-section section-data)
                                                           (append '((type dev)) section-data))
                                                  (append '((type mount)) previous-section section-data))))
           (cl-delete-if ;; We filter the list of sections to show only interesting ones.
            (lambda (section-data)
              "True if SECTION-DATA has no IdType ar no MountPoints.
                          In other words, delete sections without type format
                          nor mount points."
              (and (string= ""
                            (car (alist-get "IdType" section-data '("") nil #'string=)))
                   (not (alist-get "MountPoints" section-data nil nil #'string=))))
            (car (alist-get "sections" udisk-data nil nil #'string=)))))))
                        
(defun udisksctl-update-device-alist ()
  "Update variables with new device information from udisksctl."
  (setq udisksctl-device-alist (udisksctl--call-dump)))

(defun udisksctl-device-alist-to-entries (&optional device-alist)
  "Transform a parsed output from udisksctl-cli to tabulated-list entries.
DEVICE-ALIST is a parsed output from udisksctl-cli.  If nil or not provided,
the default `udisksctl-device-alist' is used.  See
`udisksctl-update-device-alist' to update the default device alist."
  (delq nil (mapcan #'udisksctl-udisk-data-to-entries
                    (or device-alist udisksctl-device-alist))))

(defun udisksctl-list (&optional no-update)
  "Show an interactive buffer with a list of devices.
If NO-UPDATE is t, the do not update the device list (do not call
udisksctl dump)."
  (interactive "P")
  (with-current-buffer (get-buffer-create udisksctl-list-buffer-name)
    (unless no-update
      (udisksctl-update-device-alist)
      (setq tabulated-list-entries (udisksctl-device-alist-to-entries)))
    (let ((curpoint (point))) ;; save-excursion did not work because of erase-buffer!
      (udisksctl-mode)
      (tabulated-list-print)
      (goto-char curpoint))
    (switch-to-buffer (current-buffer)))
  (message "udisksctl-list reloaded!"))

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

(defun udisksctl-poweroff-at-point ()
  "Power off the device at the current line.
This function is designed for the udisksctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (progn (message "Powering off %s" device-name)
               (udisksctl-poweroff device-name))
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
  (let ((file-path (car (alist-get "MountPoints"
                                   (tabulated-list-get-id)
                                   nil nil #'string=))))
    (if file-path
        (find-file file-path)
      (message "Cannot find mount-point at current position."))))
  
(defun udisksctl--find-device-name ()
  "Find the device name near the current point."
  (car (alist-get "Device" (tabulated-list-get-id) nil nil #'string=)))
    

(provide 'udisksctl)
;;; udisksctl.el ends here
