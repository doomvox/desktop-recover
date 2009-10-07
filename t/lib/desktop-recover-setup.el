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

;;; This is a July 19, 2009 copy of it, mutated to run the automated tests.
;;; It is used in places like:
;;    lib/emacs/t/00-desktop-autosave.t

;;; To use, place this file (and it's relatives) in your load-path,
;;; and add the line:
;;;
;;;    (load-library "desktop-recover-setup")
;;;
;;; to one of your emacs init files (e.g. ~/.emacs).

(provide 'desktop-recover-setup)
(eval-when-compile
  (require 'cl))

; Do you hate pop-up dialog boxes as much as I do?
(setq use-dialog-box nil)

;; Turning desktop.el off (we'll use it indirectly)
(desktop-save-mode -1)

;; TODO generalize these locations, make them relative to dev location:
;;   /home/doom/End/Cave/DesktopAutosave
(add-to-list 'load-path
  (expand-file-name "/home/doom/End/Cave/DesktopAutosave/lib/emacs/t/lib"))
(add-to-list 'load-path
  (expand-file-name "/home/doom/End/Cave/DesktopAutosave/lib/emacs"))


;; Note: tried load-library rather than require, without seeing any improvement.
(require 'desktop)
;; (load-library "desktop")

;; TODO I personally am setting this variable in my emacs_launcher.pl script.
;;      where would normal human beings want to set it?
;;  Perhaps: write a function that sets it based on certain other things
;;           (is there another emacs running already?
;;            what is the current "project"?)

(setq desktop-dirname "/home/doom/End/Cave/DesktopAutosave/lib/emacs/t/tmp/")
;; (setq desktop-dirname "/home/doom/")
;; (load-library "desktop-recover-autosave")
(require 'desktop-recover-autosave)
;; (load-library "desktop-recover")
(require 'desktop-recover)

;;; DEBUG
;;; (desktop-recover-interactive) ;;note: to debug comment out and run manually
