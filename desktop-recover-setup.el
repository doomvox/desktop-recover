;;; desktop-recover-setup.el --
;; Copyright 2007 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu

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

;;; Set-up for the desktop.el package, and related things.

;;; This is an example of one way of using the code from the
;;; desktop-recover project.

;;; To use, place this file (and it's relatives) in your load-path,
;;; and add the line:
;;;
;;;    (load-library "desktop-recover-setup")
;;;
;;; to one of your emacs init files (e.g. ~/.emacs).

(provide 'desktop-recover-setup)
(eval-when-compile
  (require 'cl))

;; TODO trying load-library rather than require.  Any improvement? (No)
;; (load-library "desktop")
(require 'desktop)

;; TODO Revise these stale remarks:
;; TODO I personally am setting this variable in my emacs_launcher.pl script.
;;      where would normal human beings want to set it?
;;  Perhaps: write a function that sets it based on certain other things
;;           (is there another emacs running already?
;;            what is the current "project"?)
;; Perhaps a bigger problem: there's a danger that desktop will munge this setting.
;; Perhaps: repeatedly set it from some other source that it can't munge?

;; (setq desktop-dirname "/home/doom/End/Cave/DesktopAutosave/lib/emacs/t/tmp/")
;; (setq desktop-dirname "/home/doom/")


;; introducing a new var setting, which wherever possible, we will use
;; instead of desktop.el's confusing search process.
(setq desktop-recover-location "")

(require 'desktop-recover-autosave)
(require 'desktop-recover)

;; TODO -- trying moving these settings down to *after* the point at which all
;; the libraries have been loaded.  Does *this* help?

;; Turning desktop.el off (we'll use it indirectly)
(desktop-save-mode -1)
; Do you hate pop-up dialog boxes as much as I do?  TODO put this in luddite mode?
(setq use-dialog-box nil)

(setq desktop-load-locked-desktop t)

(desktop-recover-interactive)

;; TODO think carefully about whether you want to do this... mysterious lock-up problems.
(desktop-recover-do-saves-automatically)

;; So that we can tell if exit was clean.
(define-key ctl-x-map "\C-c" 'desktop-recover-autosave-save-buffers-kill-terminal)
