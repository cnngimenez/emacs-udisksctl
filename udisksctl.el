;;; udisksctl.el --- Interface to udisksctl -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Toni Schmidbauer

;; Author: Toni Schmidbauer <toni@stderr.at>
;; Created: 11 June 2013
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/tosmi/emacs-udisksctl
;; Package-Requires: ((emacs "25.1"))

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

(defcustom udisksctl-dump-cmd "dump"
  "Parameter for udisksctl to dump all device information."
  :type 'string
  :group 'udisksctl)

(defvar udisksctl-process-buffer-name "*udisksctl-process*")
(defconst udisksctl-list-buffer-name "*udisksctl-list*"
  "Name of the list device buffer.")
(defvar udisksctl-process nil)
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
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'udisksctl-unlock)
    (define-key map "l" 'udisksctl-lock)
    (define-key map "m" 'udisksctl-mount-at-point)
    (define-key map "U" 'udisksctl-unmount)
    (define-key map "g" 'udisksctl-refresh-buffer)
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

(defun udisksctl-execute-cmd (cmd device &optional noerase)
  "Execute CMD on DEVICE, does not require user input.
If NOERASE is specified the output buffer will not be erased."
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

(defun udisksctl-process-filter (proc string)
  "Filter udisksctl output for a password prompt.
PROC is the process object.
STRING is the process output to filter."
  (save-current-buffer
    (set-buffer (process-buffer proc))
    (if (string-match "^Passphrase: " string)
	(process-send-string proc (concat (read-passwd "Passphrase: " nil) "\n"))
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
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to unlock: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-unlock-cmd udisksctl-device))

(defun udisksctl-lock (&optional device)
  "Call the udiskctl lock command.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to lock: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-lock-cmd udisksctl-device))

(defun udisksctl-mount (&optional device)
  "Mount a device using udisksctl.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to mount: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-mount-cmd udisksctl-device))

(defun udisksctl-unmount (&optional device)
  "Umount a device using udiskctl.
DEVICE is the device name (for example: /dev/sda)."
  (interactive)
  (if (not device)
      (setq udisksctl-device (udisksctl-read-device "Enter device name to unmount: "))
    (setq udisksctl-device 'device))
  (udisksctl-execute-cmd udisksctl-unmount-cmd udisksctl-device))

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

(defconst udisksctl-status-device-regexp "[[:space:]]\\([^[:space:]]+\\)[[:space:]]*$"
  "Regexp to find the device name on the status buffer.
It find the \"sda\" device name.  Observe that it can find the
\"DEVICE\" string and the separator line too.")

(defun udisksctl--find-device-name ()
  "Find the device name near the current point."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-forward udisksctl-status-device-regexp (line-end-position) t)
      (let ((str (match-string 1)))
        (unless (or (string= "DEVICE" str)
                    (string-prefix-p "-" str))
          str)))))

(defun udisksctl-mount-at-point ()
  "Mount the device at the current point.
This function is designed for the udiskctl buffer."
  (interactive)
  (let ((device-name (udisksctl--find-device-name)))
    (if device-name
        (udisksctl-mount (concat "/dev/" device-name))
      (message "No device name found at point."))))

;; --------------------------------------------------
;; Parsing the dump output
;; --------------------------------------------------

(defun udisksctl--parse-block (string)
  "Return an alist for the parsed information in STRING .
STRING should be a block from the dump output."
  (let ((alist (mapcar (lambda (s)
                         (split-string s ":" nil "[[:space:]]*"))
                       (split-string string "\n" t))))
    (append (list (list "type" (concat "org.freedesktop" (caar alist))))
            (seq-drop alist 2))))

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

(defun udisksctl--insert-block (device-data)
  "Insert device information for user consumption.
Format a string and insert the information from DEVICE-DATA to the
current buffer."
  (insert (format "%s %s"
                  (alist-get "Device" device-data nil nil #'string=)
                  (if (string= "true" (car (alist-get "ReadOnly" device-data nil nil #'string=)))
                      "🔓"
                    "🔒"))))
                               

(defun udisksctl--insert-filesystem (device-data)
  "Insert filesystem information for user consumption.
DEVICE-DATA is the filesystem parsed information."
  (insert (format "%s  "
                  (alist-get "MountPoints" device-data nil nil #'string=))))

(defun udisksctl--insert-section (section-data)
  "Insert the section data for user consumption.
Call the proper function depending on the type of the section.
SECTION-DATA is a parsed section from the dump."
  (cond ((udisksctl-device-block-p section-data)
         (udisksctl--insert-block section-data))
        ((udisksctl-device-filesystem-p section-data)
         (udisksctl--insert-filesystem section-data))))

(defun udisksctl--insert-udisk (udisk-data)
  "Insert udisk and their sections data into current buffer.
Format a string and insert it for user consumption.
UDISK-DATA is a parsed udisk information."
  (mapc #'udisksctl--insert-section (car (alist-get "sections" udisk-data nil nil #'string=)))
  (insert "\n"))
                         
(defun udisksctl-update-device-alist ()
  "Update variables with new device information from udisksctl."
  (setq udisksctl-device-alist (udisksctl--call-dump)))

(defun udisksctl-list ()
  "Show an interactive buffer with a list of devices."
  (interactive)
  (udisksctl-update-device-alist)
  (with-current-buffer (get-buffer-create udisksctl-list-buffer-name)
    (erase-buffer)
    (mapc #'udisksctl--insert-udisk udisksctl-device-alist)
    (switch-to-buffer (current-buffer))))
  
(provide 'udisksctl)
;;; udisksctl.el ends here
