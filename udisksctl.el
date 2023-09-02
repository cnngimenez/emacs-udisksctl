;;; udisksctl.el --- Interface to udisksctl -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Toni Schmidbauer

;; Author: Toni Schmidbauer <toni@stderr.at>
;; Created: 11 June 2013
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/tosmi/emacs-udisksctl
;; Package-Requires: ((emacs "24.1"))

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

(defvar udisksctl-process-buffer-name "*udisksctl-process*")
(defvar udisksctl-process nil)
(defvar udisksctl-device nil)

(defvar udisksctl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'udisksctl-unlock)
    (define-key map "l" 'udisksctl-lock)
    (define-key map "m" 'udisksctl-mount)
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

(setq debug-on-error nil)

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
	   (add-to-list 'udisksctl-status-list (cons (match-string 1) (match-string 2)))))
    (cond ((or
	    (re-search-forward "Unmounted \\([^[:space:]]+\\)\." nil t)
	    (re-search-forward "Locked \\([^[:space:]]+\\)\." nil t))
	   (udisksctl-remove-mapping (match-string 1))))))

(defun udisksctl-remove-mapping (searchkey)
  "Search for SEARCHKEY in the status list."
  (let((key (assoc searchkey udisksctl-status-list)))
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

(provide 'udisksctl)
;;; udisksctl.el ends here
