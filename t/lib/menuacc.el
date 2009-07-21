;;; menuacc.el --- Add accelerator marks to menu bar

;; Copyright (C) 2005, 2006 by Lennart Borgman

;; Author:     Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: 2005-08-16
;; Version: 0.52
;; Last-Updated: Thu May 25 01:06:18 2006 (7200 +0200)
;; Keywords: w32 menus keyboard


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file adds accelerator to the top level menu names in the menu
;; bar. Those accelerator are put on the first letter of the names so
;; that the same keysequences should be used to access the menus with
;; or without this file.
;;
;; Currently only MS Windows is supported.
;;
;; On MS Windows the accelerator keys will be underlined when you
;; access the menus with the ALT key (but not when you use F10 for
;; some reason). This is the default behaviour on MS Windows.
;;
;; To use this module put it in Emacs load-path and require or load
;; it:
;;
;;    (require 'menuacc)
;;    (add-hook 'menu-bar-update-hook 'menuacc-add-accel t)
;;
;; Or, maybe better: use custom to set menuacc-active.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst menuacc-marker
  (cond
   ((eq system-type 'windows-nt)
    "&")
   (t
    (error "System type %s is not yet supported by menuacc.el" system-type))))

(defvar menuacc-used-chars '(?- ?\ ))
(defvar menuacc-old-alist nil)
(defvar menuacc-read nil)

(defun menuacc-save()
  ""
  (let* ((buff (find-file-noselect menuacc-file))
         (standard-output buff))
    (with-current-buffer buff
      (erase-buffer)
      (insert "(progn\n")
      (insert "(setq menuacc-used-chars\n'")
      (pp menuacc-used-chars)
      (insert ")\n\n")
      (insert "(setq menuacc-old-alist\n'")
      (pp menuacc-old-alist)
      (insert " ))"))))

(defun menuacc-read()
  (with-current-buffer (find-file-noselect menuacc-file)
    (setq menuacc-read t)
    (setq buffer-save-without-query t)
    (when (< 0 (length (buffer-string)))
      (condition-case err
          (eval (read (buffer-string)))
        (error
         (message "%s" (error-message-string err))
         (sit-for 4)
         )))))


(defun menuacc-add-accel1(map)
  (when map
    (mapcar (lambda (elt)
              (when (and (listp elt)
                         (listp (cdr elt)))
                (let* ((tail (cdr elt))
                       (cartail (car tail)))
                  (unless (arrayp cartail)
                    (setq tail (cdr tail))
                    (setq cartail (car tail)))
                  (when (arrayp cartail)
                    ;;(unless (equal menuacc-marker (substring cartail 0 1))
                    ;;  (setcar tail (concat menuacc-marker cartail)))
                    (unless (string-match menuacc-marker cartail)
                      ;;(message "cartail=%s %s" cartail tail)
                      (let* ((old (assoc cartail menuacc-old-alist))
                             (newtext (if old (cdr old) ""))
                             (inserted old))
                        (unless old
                          (mapc (lambda(c)
                                  (let ((dc (elt (downcase (string c)) 0)))
                                    (unless (or inserted
                                                (member dc menuacc-used-chars))
                                      (setq newtext (concat newtext menuacc-marker))
                                      (setq inserted t)
                                      (setq menuacc-used-chars
                                            (cons dc
                                                  menuacc-used-chars)))
                                    (setq newtext (concat newtext (string c)))
                                    ;;(message "%s %s" inserted newtext)(sit-for 1)
                                   ))
                                cartail))
                        (unless inserted
                          (setq newtext (concat menuacc-marker cartail)))
                        (unless old
                          (setq menuacc-temp-changed t)
                          (setq menuacc-old-alist
                                (cons (cons cartail newtext)
                                      menuacc-old-alist)))
                        (setcar tail newtext))
                      )
                    ))))
            (lookup-key map [menu-bar]))))

(defun menuacc-add-accel()
  ;; We are in trouble if we get an error here because it is run in
  ;; the hook so protect the code.
  ;;(message "-----------------------")
  (condition-case err
      (progn
        (unless menuacc-read
          (menuacc-read))
        (let (menuacc-temp-changed)
          (mapc 'menuacc-add-accel1 (current-active-maps))
          (when menuacc-temp-changed
            (menuacc-save)))
        )
    (error
     (message "Error in menuacc-add-accel: %s" (error-message-string err))
     (sit-for 4)
     (remove-hook 'menu-bar-update-hook 'menuacc-add-accel)))
  t)

(defcustom menuacc-file "~/.menuacc"
  "File to save menu accelerators in."
  :type 'file
  :group 'emacsw32
  :require 'menuacc
  )

(defcustom menuacc-active nil
  "Add underlined accelerators to menus dynamically if non-nil."
  :type 'boolean
  :group 'emacsw32
  :require 'menuacc
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (and value
                  window-system)
             (progn
               (add-hook 'menu-bar-update-hook 'menuacc-add-accel t)
               (menuacc-add-accel))
           (remove-hook 'menu-bar-update-hook 'menuacc-add-accel))))


(provide 'menuacc)

;;; menuacc.el ends here
