;;; desktop-recover.el ---

;; Copyright 2009 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: desktop-recover.el,v 0.0 2009/07/08 02:59:21 doom Exp $
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

;;  Works with the .emacs-desktop files (as generated by desktop.el),
;;  but unlike with desktop-read, provides an interactive menu that
;;  let's the user choose which buffers will be restored.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'desktop-recover)

;;; Code:

(provide 'desktop-recover)
(eval-when-compile
  (require 'cl))

(require 'desktop)
(require 'thingatpt)


;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(defcustom desktop-recover-location "$HOME/.emacs.d"
  "The default location from which we save and restore desktop files.
Note: desktop.el has a desktop-dirname location, but that can not be
used reliably as a user setting, because the code changes it under some
circumstances.")
(put 'desktop-recover-location 'risky-local-variable t)
(setq desktop-recover-location
      (desktop-recover-autosave-fixdir desktop-recover-location))

;; TODO defcustom?
(defvar desktop-recover-buffer-name "*Desktop Buffer Restore Menu*"
  "Buffer name for the desktop restore menu.")

(defvar desktop-recover-desktop-list-doc ""
  "Many functions in this package work with a data-structure
typically called the \"desktop-list\". This describes the buffers
recorded in the .emacs-desktop file, which are candidates to be
re-loaded when desktop-recover-interactive is run.  The
desktop-list is a list of lists, with one row per buffer, where
each row is a list of (in this order): name, path, mode, and the
desktop-create-buffer call.  The \"name\" is the name sans path,
the \"path\" is the full-name, including path, the mode is the
name of the associated emacs mode (e.g. \"text-mode\"), and the
\"desktop-create-buffer call\" is the code (in string form) that
will need to be run to restore the buffer.")

;; ======================
;; read .emacs-desktop  files

;; desktop.el just loads the .emacs-desktop files (which are elisp code)
;; I want to deal with each file conditionally, so I need to parse it
;; myself


;; TODO need to set these in here? avoiding save during eval might be
;; good... but I think I can do that by not enabling saves until we've
;; reloaded...
    ;; Avoid desktop saving during evaluation of desktop buffer.
;;    (let ((desktop-save nil)
;;           (desktop-first-buffer nil)

;;           (owner (desktop-owner))


;; TODO maybe, dirname should not be optional?
(defun desktop-recover-interactive (&optional dirname)
  "Read the .emacs-desktop file, bring up menu to approve buffer restoration."
  (interactive) ;; maybe: interactive D?
  (let* ( (file (desktop-read-initialization dirname))
          ;; an .emacs-desktop file is in sections labeled like so:
          (global-section-marker ";; Global section")
          (buffer-section-marker ";; Buffer section")
          ;; prepend ^ to make them regexps
          (global-section-pattern
            (concat "^" global-section-marker))
          (buffer-section-pattern
            (concat "^" buffer-section-marker))
          (global-section "")
          (buffer-section "")
          (desktop-list)  ;; list of lists, one row for each desktop buffer
         )
    (find-file file)

    ;; parse into global and buffer sections
    ;; TODO need to check the file format version, no?  Warn if it's wrong.
    (goto-char (point-min))
    (re-search-forward global-section-pattern)
    (forward-line 1)
    (let* ( (beg (point) )
            (end)
            )
      (re-search-forward buffer-section-pattern)
      (previous-line 1)
      (move-end-of-line 1)
      (setq end (point))
      (setq global-section (buffer-substring beg end))
      )

    (forward-line 2)
    (let* ( (beg (point) )
            (end (point-max))
            )
      (setq buffer-section (buffer-substring beg end))
      )

    (eval (read global-section))

    (setq desktop-list
          (desktop-parse-buffer-section buffer-section))

    ;; display the desktop-list in a buffer with mode
    ;; designed for interactive selection.
    (desktop-recover-show-menu desktop-list)

    ;; TODO do I need to do this here? (I doubt it).
    ;;    (desktop-read-tail)
    ))


(defun desktop-parse-buffer-section (buffer-section)
  "Associate file system names with desktop-create-buffer code.
Parses the 'buffer section' of a .emacs.desktop file (passed in
as the string BUFFER-SECTION) subdividing it into
desktop-create-buffer function calls, and picking out names from
them to use for user confirmation.  Returns the desktop-list, a
list of lists, with one row per buffer, where each row is a
list (in this order) of: name, path, mode, and the
desktop-create-buffer call.  See \[[desktop-recover-desktop-list-doc]]."
  (interactive) ;; DEBUG only
  (let* (
          ( dcb-string "(desktop-create-buffer" )
          ( dcb-pattern (format "^[ \t]*?%s[ \t]" dcb-string) )
          ( dcb-list )
          ( dcb-lines )
          ( file-format )
          ( mode )
          ( file-name ) ;; full name, with path
          ( name )      ;; file/dir name without path
          ( misc )      ;;  a list of stuff... multiple paths for tree dired?
          ( first-misc ) ;; the primary dir in dired-mode (with trailing slash)
          ( path )
          ( record )
          ( desktop-list )
         )

    ;; here we split on the initial funcall string, then prepend it again
    ;; to have the list of complete function calls
    (setq dcb-list
          (mapcar (lambda (item)
                    (concat dcb-string " " item))
                    (split-string buffer-section dcb-pattern t)))

    (dolist (dcb-code dcb-list)
      (message "dcb code: %s\n" dcb-code)

        ;; parse dcb-code as a list
        (setq dcb-lines (split-string dcb-code "\n" t))

        (setq file-format (car (cdr (split-string (nth 0 dcb-lines) " " t))))
        (setq file-name (desktop-recover-clean-string (nth 1 dcb-lines)))
        (setq name (desktop-recover-clean-string (nth 2 dcb-lines)))
        (setq mode (desktop-recover-clean-string (nth 3 dcb-lines)))
        (setq misc (nth 8 dcb-lines))  ;; will need extra clean?

        (cond ((string= mode "dired-mode")
               (message "case dired")
               (setq first-misc (desktop-recover-snag-first-item misc))
               (setq path first-misc))
              (t
               (message "default (non-dired)")
               (setq path file-name)
               ))

        (setq record
              (append
               (mapcar 'eval
                       '(name path mode dcb-code))
               record))

        (setq desktop-list (cons record desktop-list))
      )
    desktop-list))

(defun desktop-recover-clean-string (string)
  "Strip leading/trailing whitespace, and also, leading single-quotes."
  (let (
         (strip-lead-space-pattern "^[ \t]*\\([^ \t]*.*\\)")
         (strip-trail-space-pattern "\\(.*?\\)[ \t]*$")

         (strip-lead-apostrophe-pattern "^'*\\(.*\\)")
         (temp1)
         (temp2)
         (clean)
         )
    (if
        (string-match strip-lead-space-pattern string)
        (setq temp1 (match-string 1 string))
      (setq temp1 string))

    (if
        (string-match strip-trail-space-pattern temp1)
        (setq temp2 (match-string 1 temp1))
      (setq temp2 temp1))

    (string-match strip-lead-apostrophe-pattern temp2)
    (setq clean (match-string 1 temp2))
    clean))

(defun desktop-recover-snag-first-item (list-string)
  "Get's the first item out of the list stored in LIST-STRING.
Intended to deal with the 'desktop-buffer-misc' field of a desktop-create-buffer call.
Which may look something like:
  '(\"/home/doom/End/Pit/\")    "
  (let* (
          (list (eval (read list-string)))
          (first-item (car list))
         )
    first-item))

;; TODO rename to: desktop-full-name-and-path or something?
;; But Why not just use desktop-full-file-name directly?
;; TODO Q: should this check for file existance? A: no.
(defun desktop-read-initialization (&optional dirname)
  "Returns the full name and path of the desktop file.
Uses the standard name \"\", located either in the given
DIRNAME or in the default `desktop-recover-location'. "
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (or
             ;; If DIRNAME is specified, use it.
             (and (< 0 (length dirname)) dirname)
             ;; Otherwise fall back on the default
             desktop-recover-location))))
    ;; now get the full file name.
    (desktop-full-file-name dirname)
    )

(defun desktop-read-initialization-old (&optional dirname)
  "Does precisely the same folderol as the desktop-read function,
up to the point where it loads the desktop file: instead it returns the
full name of the desktop file located in DIRNAME."
;; TODO Consider the possibility that "desktop.el" is completely out-to-lunch
;; half of the time, and that slavishly imitating it is bad news, as this
;; routine clearly is.
  (unless noninteractive ;; whole function is a no-op if used interactively...
    (setq desktop-dirname
          (file-name-as-directory
           (expand-file-name
            (or
             ;; If DIRNAME is specified, use it.
             (and (< 0 (length dirname)) dirname)
             ;; Otherwise search desktop file in desktop-path.
             (let ((dirs desktop-path))
               (while (and dirs
                           (not (file-exists-p
                                 (desktop-full-file-name (car dirs)))))
                 (setq dirs (cdr dirs)))
               (and dirs (car dirs)))
             ;; If not found and `desktop-path' is non-nil, use its first element.
             (and desktop-path (car desktop-path))
             ;; Default: Home directory.
             "~"))))
    (if (file-exists-p (desktop-full-file-name))
	;; Desktop file found, but is it already in use?
	(let ((desktop-first-buffer nil)
	      (desktop-buffer-ok-count 0)
	      (desktop-buffer-fail-count 0)
	      (owner (desktop-owner))
	      ;; Avoid desktop saving during evaluation of desktop buffer.
	      (desktop-save nil))
	  (if (and owner
		   (memq desktop-load-locked-desktop '(nil ask))
		   (or (null desktop-load-locked-desktop)
		       (not (y-or-n-p (format "Warning: desktop file appears to be in use by PID %s.\n\
Using it may cause conflicts.  Use it anyway? " owner)))))
	      (let ((default-directory desktop-dirname))
		(setq desktop-dirname nil)
		(run-hooks 'desktop-not-loaded-hook)
		(unless desktop-dirname
		  (message "Desktop file in use; not loaded.")))
	    (desktop-lazy-abort)

            (desktop-full-file-name) ;; TODO question: why not pass in a dirname?
                                     ;; relies on default-directory to pass the value?

            ;; returns the file name (make this clearer?)

	    ))
      ;; No desktop file found.
      (desktop-clear) ;; -- TODO so why are we fucking clearing the desktop?
      (let ((default-directory desktop-dirname))
        (run-hooks 'desktop-no-desktop-file-hook))
      (message "No desktop file.")
      nil)))

;; TODO not yet in use.  (make sure you understand it, eh?)
;; I don't see any need for these features at all, actually...
;; the buffer order juggling is mildly inane (and useless for my purpose)
;; and the detailed report of success or failure is pretty useless...
;; if the file you wanted didn't get opened, you'll just go open it.
(defun desktop-read-tail (&optional dirname)
  "The code that the desktop-read function executes after loading the desktop file.
This folderol was cut and paste from there on the theory that whatever it is
that this is doing it may be a good idea for me to do also."
  (interactive)
  ;;; TODO add anything else here?
  ;;;
  ;; Remember when desktop buffer was modified.
  (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
  ;; If it wasn't already, mark it as in-use, to bother other
  ;; desktop instances.
  (unless owner
    (condition-case nil
        (desktop-claim-lock)
      (file-error (message "Couldn't record use of desktop file")
                  (sit-for 1))))

  ;; `desktop-create-buffer' puts buffers at end of the buffer list.
  ;; We want buffers existing prior to evaluating the desktop (and
  ;; not reused) to be placed at the end of the buffer list, so we
  ;; move them here.
  (mapc 'bury-buffer
        (nreverse (cdr (memq desktop-first-buffer (nreverse (buffer-list))))))
  (switch-to-buffer (car (buffer-list)))
  (run-hooks 'desktop-delay-hook)
  (setq desktop-delay-hook nil)
  (run-hooks 'desktop-after-read-hook)
  (message "Desktop: %d buffer%s restored%s%s."
           desktop-buffer-ok-count
           (if (= 1 desktop-buffer-ok-count) "" "s")
           (if (< 0 desktop-buffer-fail-count)
               (format ", %d failed to restore" desktop-buffer-fail-count)
             "")
           (if desktop-buffer-args-list
               (format ", %d to restore lazily"
                       (length desktop-buffer-args-list))
             ""))
  )

;;; The following code handles the display of  marker, name, path, mode,
;;; where the is set ("*") if buffer is to be loaded,
;;; the return key is the "do-it" that accepts the displayed settings.
;;; and the "m" and "u" keys control whether the current line is set

(defvar desktop-recover-unmarker " "
  "Symbol used for an unmarked buffer that will not be reloaded by default.")

(defvar desktop-recover-marker "*"
  "Symbol used for a marked buffer that will be reloaded by default.")

(define-derived-mode desktop-recover-mode
  text-mode "desktop-recover"
  "Major mode to display candidates for buffers to be restored when re-starting emacs.
\\{desktop-recover-mode-map}"
  (use-local-map desktop-recover-mode-map)
  )

(define-key desktop-recover-mode-map "\C-m" 'desktop-recover-do-it)
(define-key desktop-recover-mode-map "m"    'desktop-recover-mark-move-down)
(define-key desktop-recover-mode-map "u"    'desktop-recover-unmark-move-down)
(define-key desktop-recover-mode-map "n"    'next-line)
(define-key desktop-recover-mode-map "p"    'previous-line)
(define-key desktop-recover-mode-map "*"    'desktop-recover-mark-move-down)

;; TODO SOON need a more complicated routine that doesn't just run the dcb
;; but also gets the path, checks for a newer autosave
;;   (if (desktop-recover-newer-auto-save path)
;;       (recover-this-file))
;; Q: is there any reason not to *always* do this recover step?
(defun desktop-recover-do-it ()
  "Accept the current settings of the restore menu buffer.
Runs the appropriate \"descktop-create-buffer\" calls stored
in the desktop-list data structure."
  (interactive)
  (let* (
         (marker-pattern "^[ \t]*\\*") ;; line begins with asterix
         (line-count (count-lines (point-min) (point-max)))
         (dcb-code)
         (dcb-list)
         )
    (goto-char (point-min))
    (while ;; loop over all lines in buffer
        (progn
          (save-excursion
            (move-beginning-of-line 1)
            ;; if line is marked with an asterix...
            (if (thing-at-point-looking-at marker-pattern)
                (progn
                  (setq dcb-code
                        (get-char-property (point) 'dcb))
                  ;; Save up the dcb-code sexps in a list, to execute later
                  (setq dcb-list (cons (eval dcb-code) dcb-list))
                  )))
          (forward-line 1)
          (< (line-number-at-pos) line-count)
    ))
    (dolist (dcb dcb-list)
      (eval (read dcb)))
    ))


(defun desktop-recover-mark ()
  "Set the marker for the current line: add leading asterix."
  (interactive)
  (save-excursion
  (setq buffer-read-only nil)
    (move-beginning-of-line 1)
    (forward-char 1)
    (delete-char 1)
    (insert desktop-recover-marker)
    (setq overwrite-mode nil)
    (setq buffer-read-only 't)
  ))


(defun desktop-recover-unmark ()
  "Unset the marker for the current line: remove leading asterix."
  (interactive)
  (save-excursion
  (setq buffer-read-only nil)
    (move-beginning-of-line 1)
    (forward-char 1)
    (delete-char 1)
    (insert desktop-recover-unmarker)
    (setq overwrite-mode nil)
    (setq buffer-read-only 't)
  ))

(defun desktop-recover-mark-move-down ()
  "Set marker on the current line, move down one."
  (interactive)
  (desktop-recover-mark)
  (forward-line 1))

(defun desktop-recover-unmark-move-down ()
  "Unset marker of the current line, move down one."
  (interactive)
  (desktop-recover-unmark)
  (forward-line 1))

;; TODO
;; Do I need to add commands to allow the user to manually
;; reject an auto-save file?

;; This is intended to be run at emacs init time (run from
;; desktop-recover-interactive) so there's no need for a keybinding
(defun desktop-recover-show-menu (desktop-list)
  "Displays info about buffers that are candidates to be restored.
These are buffers that existed when the last 'desktop-recover' was
done." ;; TODO wrong word? Is it "recover" or "save"?
;; TODO currently uses one, fixed defvar value: desktop-recover-buffer-name
;; Any reason to allow concurrent usage? (need multiple unique buffer names then)
  (interactive)
  (let* (
          (menu-contents)
         )
    (setq menu-contents (desktop-recover-build-menu-contents desktop-list))

    (switch-to-buffer desktop-recover-buffer-name)
    (setq buffer-read-only nil)
    (mark-whole-buffer)               ;; TODO find more elispy way?
    (delete-region (mark) (point))
    (insert menu-contents)
    (desktop-recover-mode)
    (setq buffer-read-only 't)
  ))

(defun desktop-recover-build-menu-contents (desktop-list)
  "Builds the menu text from the DESKTOP-LIST data."
  (let (
         (name) (path) (mode) (dcb-code)
         (marker "*")
         (hash-mark "#")
         (line "")
         (menu-contents "")
         (line-fmt "%3s%-35s%-42s")
         )
         (dolist (record desktop-list)
           ;; unpack the record
           (setq name     (nth 0 record))
           (setq path     (nth 1 record))
           (setq mode     (nth 2 record))
           (setq dcb-code (nth 3 record))

           (cond ((desktop-recover-by-default-p record)
                  (setq line (format
                              line-fmt
                              (concat " " desktop-recover-marker " ")
                              name
                              path
                              )))
                  (t
                   (setq line (format line-fmt
                               (concat " " desktop-recover-unmarker " ")
                               name
                               path
                               ))
                   ))
           (put-text-property 0 1 'dcb dcb-code line)

           ;; TODO make this part of the above line-fmt?
           (if (desktop-recover-newer-auto-save path)
               (set1 line (concat line " " hash-mark)))

           (setq menu-contents
                 (concat menu-contents line "\n"))
           )
         menu-contents))

(defun desktop-recover-clean-exit-p ()
  "Does it look like emacs exited cleanly?"
  (let* (
         (tmp-dir (desktop-recover-autosave-fixdir
                   desktop-recover-autosave-tmp-dir))
         (clean-exit-flag-file
          (concat tmp-dir desktop-recover-autosave-clean-exit-flag))
         (retval (file-exists-p clean-exit-flag-file))
         )
    retval))

(defun desktop-recover-by-default-p (record)
  "Examine RECORD to determine if this buffer should be reloaded by default.
A file should not be re-loaded if was an automatically saved temporary
buffer and emacs exited cleanly.  RECORD should be a list of
name, path, mode and dcb-code."
  (let* (
         (name) (path) (mode) (dcb-code)
         (clean-exit-p (desktop-recover-clean-exit-p))
         (tmp-dir (desktop-recover-autosave-fixdir desktop-recover-autosave-tmp-dir))
         (recover-p t) ;; return value
         )
    ;; unpack the record
    (setq name     (nth 0 record))
    (setq path     (nth 1 record))
    (setq mode     (nth 2 record))
    (setq dcb-code (nth 3 record))

    (message "the temp directory: %s" tmp-dir) ;; DEBUG

    (cond ((and
            (string=
             (desktop-recover-autosave-fixdir path)
             tmp-dir)
            (clean-exit-p))
           (setq recover-p nil))
          )
    recover-p))


(defun desktop-recover-newer-auto-save (path)
  "Given PATH (full path and file name) check for newer auto-save file."
  (let* (
      ;; (name (file-name-nondirectory path)) ;; could just pass that in too
      ;; (a-s-name (format "#%s#" name))
         (a-s-path (make-auto-save-file-name path))
         )
    ;; if a-s-path does not exist, this is nil
    (file-newer-than-file-p a-s-path path)
    ))



;;; desktop-recover.el ends here
