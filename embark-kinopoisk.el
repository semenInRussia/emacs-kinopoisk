;;; embark-kinopoisk.el --- Create `embark' keymaps and finders for films using `kinopoisk' (cinema-service) -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semenInRussia/emacs-kinopoisk

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Create `embark' keymaps and finders for films using `kinopoisk'
;; (cinema-service).

;; Here provided a finder that capture a film name inside double quotes, so to
;; apply an action insert a film name inside quotes and use `embark-act'.  And place the cursor
;; before opening double quote.  For example, if the buffer is:
;;
;; "Black Mirror"
;;
;; and you need to open the `kinopoisk' page, then locate the cursor before
;; quote call `embark-act' + hit RET.

;;; Code:

(require 'kinopoisk)
(require 'embark)


(defvar-keymap embark-kinopoisk-film-map
  :doc "Keymap with a few film actions using `kinopoisk'."
  :parent embark-general-map
  "RET" #'kinopoisk-film-open-in-web)

(defun embark-kinopoisk-film ()
  "Target a film at point of the form \"Film Name\".

Note that the cursor should be located before the opening double quote and the
result is either nil or list where the `car' is symbol \\='kinopoisk-film and
the second element has the type kinopoisk-film"
  (and
   (looking-at "\"")
   (save-excursion
     ;; skip opening quote
     (forward-char 1)
     (when-let*
         ((start (point))
          (end (if (search-forward "\"" nil :noerror)
                   (1- (point))
                 (point-max)))
          (name (buffer-substring-no-properties start end))
          (film (kinopoisk-search-one-film name)))
       (-cons* 'kinopoisk-film
               film
               start end)))))

(add-to-list 'embark-target-finders #'embark-kinopoisk-film)

(provide 'embark-kinopoisk)
;;; embark-kinopoisk.el ends here
