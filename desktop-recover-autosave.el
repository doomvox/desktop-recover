;;; desktop-recover-autosave.el ---

;; Copyright 2009 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: desktop-recover-autosave.el,v 1.10 2009/07/08 03:01:37 doom Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Auto-saving the current session via desktop.el.
;; This version ties it into the auto-save-hook.

;; Using a few hints from some code by Paul Tipper posted on the emacswiki
;;   http://www.emacswiki.org/emacs/DeskTop

;; Put this file into your load-path and the following into your ~/.emacs:

;;   (desktop-save-mode 1)

;;   (load-library "desktop-recover-autosave")
;;   (require 'desktop-recover-autosave)

(provide 'desktop-recover-autosave)
(eval-when-compile
  (require 'cl))

(require 'desktop)

;;======
;; poor man's pod/lazy man's info:
;;   dummy variables with documentation attached

(defvar desktop-recover-autosave-dangling-buffers-doc ""
  "We use \"dangling buffers\" to mean buffers without associated files.
Typically we will exclude the special display buffers (which
usually begin with an asterix), dired buffers, and so on.  We're
concerned here with buffers used for temporary notes that might've
been prematurely lost by an emacs crash \(due to a broken connect\).

Along with the automated desktop save feature, we will make save
these dangling buffers to temporary files, making them a little
less ephemeral, though not as permanent as ordinary files.
A clean exit from emacs should erase them.")

;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

(defcustom desktop-recover-autosave-tmp-dir
  (substitute-in-file-name
   "$HOME/.emacs.d/desktop-recover-autosave-tmp")
  "Location where dangling buffers that have no associated files are saved.")

(defvar desktop-recover-autosave-clean-exit-flag "desktop_recover_clean_exit.flag"
  "The existance of a file of this name signals that we did a clean exit.")

;; TODO Add a way to turn this off?  Any safety features to add?
(defun desktop-recover-do-saves-automatically ()
  "Makes the desktop saved automatically using the auto-save-hook."
  (add-hook 'auto-save-hook
            (lambda ()
              (desktop-recover-autosave-save-with-danglers))))


;; TODO
;; Need a way to skip things like my ubiquitous "shell-misc" buffers.
;; desktop.el has a skip list, right?
;; Simplest thing for me to do: rename it "*shell-misc*"... but that
;; breaks my old keystroke macros.
;;
;; Would it make sense to be able to skip all shell buffers?
;; Ideally: provide a list of major-modes that will be skipped,
;; so file/buffer names don't matter so much.
(defun desktop-recover-autosave-save-with-danglers ()
  "Desktop autosave routine that preserves buffers that have no associated files.
Works by saving them to a standard tmp directory, then using desktop.el to save
them along with the other open files.  After re-starting emacs, you should then
have buffers corresponding to the old dangling buffers (though now they'll be
associated with files in the special tmp location).
See: `desktop-recover-autosave-dangling-buffers-doc'"
  (interactive)
  (let* ( (DEBUG nil)
          (preserve-buffer (current-buffer))
          (temp-loc desktop-recover-autosave-tmp-dir)
          (dangling-buffers (desktop-recover-autosave-list-dangling-buffers))
          )
    (unless (file-exists-p temp-loc)
      (make-directory temp-loc t))

    ;; save each to temp location using a file name identical to the buffer name,
    (dolist (buffy dangling-buffers)
            (set-buffer buffy)
            (let* (
                    (  buffy-name (buffer-name  buffy) )
                    (  file-nameo (concat temp-loc "/" buffy-name) )
                    )
              (write-file file-nameo) ;; this *is* saving the buffer in the tmp location,
                                      ;; but something is destroying the dangling buffers
              ))
    (switch-to-buffer preserve-buffer)
    (deactivate-mark)
    ;; (desktop-save-in-desktop-dir)
    (desktop-recover-autosave-force-save-in-desktop-dir)
    (desktop-recover-autosave-clear-clean-save-flag)
  ))

(defun desktop-recover-autosave-display-dangling-buffers ()
  "List buffers without files or directories, skipping internal and display (*) buffers.
Returns a list of buffer objects."
  (interactive)
  (let* ( (dangling-buffers (desktop-recover-autosave-list-dangling-buffers))
          (buffname-list  (mapcar 'buffer-name dangling-buffers) )
                 )
      (desktop-recover-autosave-display-list-other-window buffname-list "*mah buffers*")
    ))

; TODO should this be refactored to use desktop-recover-autosave-list-ordinary-buffers ?
(defun desktop-recover-autosave-list-dangling-buffers ()
  "List buffers without files or directories, skipping internal and display (*) buffers.
Returns a list of buffer objects."
  (interactive)
  (let* ((DEBUG nil)
         (preserve-buffer (current-buffer))
         (output-list)
         )
    (and DEBUG (message "running desktop-recover-autosave-list-dangling-buffers"))
    (save-excursion
      (dolist (buffy (buffer-list))
        (set-buffer buffy) ; switch to buffer so we can check 'major-mode'
        (let* ((file-nameo (buffer-file-name buffy) )
               (buffy-name (buffer-name  buffy) )
               )
          (cond (
                 (and
                  ;; looking for buffers without file names
                  (not file-nameo)
                  ;; Skip directories
                  (not (string= major-mode "dired-mode")) ;; TODO any better way?
                  ;; Skip internal buffers.
                  (not (string= (substring buffy-name 0 1) " "))
                  ;; Skip dynamic display buffers
                  (not (string= (substring buffy-name 0 1) "*"))
                  )
                 (and DEBUG (message "buffer without associated file: %s" buffy-name))
                 (push buffy output-list)
                 )
                )))
      (switch-to-buffer preserve-buffer)
      (deactivate-mark)
      (and DEBUG (message "finished desktop-recover-autosave-list-dangling-buffers"))
  output-list)))

(defun desktop-recover-autosave-list-ordinary-buffers ()
  "List buffers, skipping dired buffers, internal and display (*) buffers.
Returns a list of buffer objects.  Note that 'ordinary' buffers include
'dangling' buffers without associated files."
  (interactive)
  (let* ( (DEBUG nil)
          (preserve-buffer (current-buffer))
          (output-list)
        )
    (and DEBUG (message "running desktop-recover-autosave-list-ordinary-buffers"))
    (save-excursion
      (dolist (buffy (buffer-list))
        (set-buffer buffy) ; switch to buffer so we can check 'major-mode'
        (let* ( (  file-nameo (buffer-file-name buffy) )
                (  buffy-name (buffer-name  buffy) )
                )
          (cond (
                 (and
                  ;; Skip directories
                  (not (string= major-mode "dired-mode")) ;; TODO any better way?
                  ;; Skip internal buffers (begin with space)
                  (not (string= (substring buffy-name 0 1) " "))
                  ;; Skip dynamic display buffers (begin with asterix)
                  (not (string= (substring buffy-name 0 1) "*"))
                  )
                 (and DEBUG (message "buffer: %s" buffy-name))
                 (push buffy output-list)
                 )
                )))
      (switch-to-buffer preserve-buffer)
      (deactivate-mark)
      (and DEBUG (message "finished desktop-recover-autosave-list-ordinary-buffers"))
  output-list)))


(defun desktop-recover-autosave-list-ordinary-buffer-names ()
  "List names of ordinary buffers, skipping dired, internal and display (*) buffers.
Returns a list of buffer names.  Note that 'ordinary' buffers include
'dangling' buffers without associated files."
  (interactive)
  (let* ( (ordinary-buffers (desktop-recover-autosave-list-ordinary-buffers))
          (buffname-list  (mapcar 'buffer-name ordinary-buffers) )
                 )
    buffname-list))

;; TODO -- there's a need for window handling utilities and usage standards
;; TODO -- since this is destructive, should have safety checks
;;         only work on asterix buffers?
;;         Make buffer name optional (generated default)
;; TODO -- only works with a list of strings.
;;         is there pp code that would work with, say, a list of objects like buffers?
(defun desktop-recover-autosave-display-list-other-window (list buffer-name)
  "Dipplays LIST of strings in BUFFER-NAME in a second window.
Closes all other windows except for the current window and the newly created one."
  (delete-other-windows)
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buffer-name)
  (mark-whole-buffer)
  (delete-region (mark) (point))
  (insert (mapconcat 'identity list "\n"))   ;;; TODO how to insert a list?
  (deactivate-mark))

(defun desktop-recover-autosave-print-ordinary-buffer-names ()
  "Print ordinary buffer names (non-directory, non-asterix, non-internal)."
  (interactive)
  (let* ( (buff_list   (desktop-recover-autosave-list-ordinary-buffer-names) )
          (buff_string (mapconcat 'identity buff_list "\n"))
          )
    (print buff_string)
    ))

(defun desktop-recover-autosave-insert-ordinary-buffer-names ()
  "Insert ordinary buffer names (non-directory, non-asterix, non-internal).
Inserts names into the current buffer, at point, one on each line."
  (interactive)
  (let* ( (buff_list   (desktop-recover-autosave-list-ordinary-buffer-names) )
;          (buff_string (mapconcat 'identity buff_list "\n"))
          )
    (dolist (item buff_list)
            (insert item)
            (insert "\n")
    )))

;; TODO request a hook to do this.  Doesn't seem to exist.
;; TODO
;; Of the various ways of doing this, let's try just touching a file here,
;; which the autosave code will delete. (would the reverse be better?)
;; TODO better than touch: use emacs to save a buffer that has an
;; identifying message in it (current pid?) (Oh: list of stuff to
;; be saved... then you could check timestamps later to see if they
;; all were saved...)
(defun desktop-recover-autosave-save-buffers-kill-terminal ()
  "Wrapper around save-buffers-kill-terminal to flag clean exits.
Actually, it flags the fact that we *tried* to exit cleanly, since
there's easy no way to check if all saves were completed before
emacs died."
  (let* (( clean-exit-flag-file
           (concat
            (desktop-recover-autosave-fixdir
             desktop-recover-autosave-tmp-dir)
            desktop-recover-autosave-clean-exit-flag))
         ;; set these to override defaults
         (output-buffer nil)
         (error-buffer  nil)
         (cmd (format "touch %s" clean-exit-flag-file))
         )
    (desktop-recover-autosave-save-with-danglers) ;; TODO double-check. right save?
    ;; we do this *after* the above, because that also clears the flag
    (shell-command cmd output-buffer error-buffer)
    (save-buffers-kill-terminal)
    ))

(defun desktop-recover-autosave-clear-clean-save-flag ()
  "Remove the clean save flag (until the next clean save really happens)."
  (let* (( clean-exit-flag-file
           (concat
            (desktop-recover-autosave-fixdir
             desktop-recover-autosave-tmp-dir)
            desktop-recover-autosave-clean-exit-flag))
         )
    (delete-file clean-exit-flag-file)
    ))

(defun desktop-recover-autosave-fixdir (dir &optional root)
  "Fixes the DIR.
Conditions directory paths for portability and robustness.
Some examples:
 '~/tmp'             => '/home/doom/tmp/'
 '~/tmp/../bin/test' => '/home/bin/test/'
Note: converts relative paths to absolute, using the current
default-directory setting, unless specified otherwise with the
ROOT option.  Note side-effect: converts the empty string into
the default-directory or the ROOT setting."
  (let ((return
         (substitute-in-file-name
          (convert-standard-filename
           (file-name-as-directory
            (expand-file-name dir root))))))
    return))

(defun desktop-recover-autosave-force-save (dirname &optional release)
   "Force save of desktop by wiping out any existing file first.
This ensures you will not have any question about modtimes
getting in the way."
   (message "desktop-force-save called")
   (setq desktop-dirname (file-name-as-directory (expand-file-name dirname)))
   (desktop-remove)
   (desktop-save dirname release)
   )

(defun desktop-recover-autosave-force-save-in-desktop-dir ()
  "Save the desktop in directory `desktop-dirname'."
  (interactive)
  (if desktop-dirname
      (desktop-recover-autosave-force-save desktop-dirname)
    (call-interactively 'desktop-save))
  (message "Desktop saved in %s" (abbreviate-file-name desktop-dirname)))
