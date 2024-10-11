;;; dired-actions.el --- Paste, move, symlink, files/dirs in dired buffers -*- lexical-binding:t -*-
;;
;; Author: justinlime
;; URL: https://github.com/justinlime/dired-actions.el
;; Version: 1.0
;; Keywords; dired, actions cut, copy, paste, yank, symlink, move
;; Package-Requires: ((emacs "28.1"))
;;
;;; License
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;; dired-actions allows you to preform various actions such as
;; `copy/pasting', `moving', and `symlink' files/dirs between Dired
;; buffers.
;;
;; `dired-async' is automatically supported when installed.
;;
;;; Code:

(require 'dired)
(require 'dired-aux)

(defgroup dired-actions nil
  "Perform various actions on marked Dired files."
  :prefix "dired-actions-"
  :group 'applications)

(defcustom dired-actions-post-action-hook nil
  "A hook that is run after performing an action (excluding capture)."
  :type 'hook
  :group 'dired-actions)

(defcustom dired-actions-post-capture-hook nil
  "A hook that is run after capturing files."
  :type 'hook
  :group 'dired-actions)

(defcustom dired-actions-post-clear-hook nil
  "A hook that is run after clearing captured files."
  :type 'hook
  :group 'dired-actions)

(defvar dired-actions--files nil
  "The most recent captured files.")

(defun dired-actions-capture ()
  "Capture marked Dired files into `dired-actions--files'."
  (interactive)
  (mapc (lambda (file) (add-to-list 'dired-actions--files file)) (dired-get-marked-files))
  (let* ((added (length (dired-get-marked-files)))
         (total (length dired-actions--files))
         (msg (ngettext "file is" "files are" total)))
    (message "%d %s now captured [%s added]." total msg added))
  (run-hooks 'dired-actions-post-capture-hook))

(defun dired-actions-clear ()
  "Empty the `dired-actions--files' list."
  (interactive)
  (setq dired-actions--files nil)
  (message "Emptied the capture file list.")
  (run-hooks 'dired-actions-post-clear-hook))

(defun dired-actions-execute (file-creator operation marker-char)
  "Apply provided FILE-CREATOR, OPERATION, and MARKER-CHAR to `dired-create-files' as their respective arguments.

Applies the `FILE-CREATOR' to every filename present in `dired-actions--files', with the destination
being resolved to `dired-current-directory'.
"
  (if (length= dired-actions--files 0)
    (message "No captured files to operate on.")
    (if (not (dired-mark-pop-up " *Captured Files*" nil dired-actions--files #'yes-or-no-p
                                (format 
                                  (ngettext "%s [%d file]" "%s [%d files]" (length dired-actions--files))
                                    operation (length dired-actions--files))))
      (message "%s aborted." operation)
      ;; prevent dired-create-files from mutating the capture list.
      (let ((fn-list (copy-sequence dired-actions--files)))
        (dired-create-files file-creator operation fn-list
          (lambda (file) (expand-file-name (file-name-nondirectory (directory-file-name file)) (dired-current-directory))) marker-char))
      (revert-buffer)))
  (run-hooks 'dired-actions-post-action-hook))

(defun dired-actions-move ()
  "Move the file/s from `dired-actions--files' to `dired-current-directory'."
  (interactive)
  (dired-actions-execute #'rename-file "MOVE" ?M)
  ;; keep the files usable in the ring for other actions, by remapping the base filenames to `dired-current-directory' 
  (setq dired-actions--files
    (mapcar (lambda (file) (expand-file-name (file-name-nondirectory (directory-file-name file)) (dired-current-directory))) dired-actions--files)))

(defun dired-actions-copy ()
  "Copy the file/s from `dired-actions--files' to `dired-current-directory'."
  (interactive)
  (dired-actions-execute #'dired-copy-file "COPY" ?C))

(defun dired-actions-symlink ()
  "Create a symlink to the file/s from `dired-actions--files'
to `dired-current-directory'."
  (interactive)
  (dired-actions-execute #'make-symbolic-link "SYM-LINK" ?S))

(defun dired-actions-relative-symlink ()
  "Create a relative symlink to the file/s from `dired-actions--files'
to `dired-current-directory'."
  (interactive)
  (dired-actions-execute #'dired-make-relative-symlink "RELATIVE SYM-LINK" ?R))
