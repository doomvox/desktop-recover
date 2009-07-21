;;; desktop-setup.el --
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

;;; Set-up for the dialog.el package, and related things.
;;; Ideally, one-and-only one emacsen should call this.
;;; annoying complications ensue if multiple emacsen are trying
;;; to use the desktop.el package.

(provide 'desktop-setup)
(eval-when-compile
  (require 'cl))

(require 'desktop)

(load-library "desktop-recover")

; BLEH!!!
(setq use-dialog-box t)
