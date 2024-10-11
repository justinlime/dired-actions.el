;;; dired-actions.el --- Paste, move, symlink, hardlink, files/dirs in dired buffers -*- lexical-binding:t -*-
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
;; `copy/pasting', `moving', and `sym/hardlinking' files/dirs between Dired
;; buffers.
;;
;; `dired-async' is automatically supported when installed.
;;
;;; Code:

(require 'dired)

(defgroup dired-actions nil
  "Perform various actions on marked Dired files."
  :prefix "dired-actions-"
  :group 'applications)

(defcustom dired-actions-post-action-hook nil
  "A hook that is run after performing an action (excluding copy)."
  :type 'hook
  :group 'dired-actions)

(defcustom dired-actions-post-copy-hook nil
  "A hook that is run after copying files to file ring."
  :type 'hook
  :group 'dired-actions)

(defvar dired-actions--file-ring nil
  "The most recent copied files.")

(defun dired-actions--execute (action action-name)
  "Execute a function to run for every item in the file ring.
Argument ACTION argument to call with `dired-create-files' with as
its' FILE-CREATOR.
Argument ACTION-NAME The name of the action for logging."
  (dolist (file dired-actions--file-ring)
    (dired-create-files action action-name dired-actions--file-ring
      #'(lambda (file) (file-name-nondirectory (directory-file-name file)))))
  (setq dired-actions--file-ring nil)
  (revert-buffer)
  (run-hooks 'dired-actions-post-action-hook))

(defun dired-actions-copy ()
  "Copy marked Dired files to the file ring."
  (interactive)
  (setq dired-actions--file-ring (dired-get-marked-files))
  (dired-unmark-all-marks)
  (run-hooks 'dired-actions-post-copy-hook))

(defun dired-actions-move ()
  "Move the file/s from the file ring to the current dir."
  (interactive)
  (dired-actions--execute #'rename-file "MOVE"))

(defun dired-actions-paste ()
  "Paste the file/s from the file ring to the current dir."
  (interactive)
  (dired-actions--execute #'dired-copy-file "PASTE"))

;; Hardlinking directories isnt supported by dired-hardlink,
;; also its just not a good idea
(defun dired-actions-hardlink ()
  "Create a relative symlink for the file/s from the file ring to the current dir.
DOES NOT WORK WITH DIRECTORIES IF PRESENT IN THE FILE RING."
  (interactive)
  (dired-actions--execute #'dired-hardlink "HARD-LINK"))

(defun dired-actions-symlink ()
  "Create a symlink for the the file/s from the file ring to the current dir."
  (interactive)
  (dired-actions--execute #'make-symbolic-link "SYM-LINK"))

(defun dired-actions-symlink-relative ()
  "Create a relative symlink for the the file/s from the file ring to the current dir."
  (interactive)
  (dired-actions--execute #'dired-make-relative-symlink "RELATIVE SYM-LINK"))

(provide 'dired-actions)

;;; dired-actions.el ends here
