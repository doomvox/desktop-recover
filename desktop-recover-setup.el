;;; desktop-recover-setup.el --
;; Copyright 2009 Joseph Brenner
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

(load-library "desktop")
(load-library "desktop-recover")

;; This is optional now (what's shown is the default setting)
(setq desktop-recover-location
      (desktop-recover-fixdir "$HOME/.emacs.d/"))

;; bind a new exit command, so that we can tell if exit was clean.
(define-key ctl-x-map "\C-c" 'desktop-recover-save-buffers-kill-terminal)

;; And this brings up the interactive buffer restore menu
  (desktop-recover-interactive)
