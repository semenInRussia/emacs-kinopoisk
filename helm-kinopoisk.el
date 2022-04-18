;;; helm-kinopoisk.el --- Wrapper of `kinopoisk' for `helm'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1

;; Package-Requires: ((emacs "27.1") (helm "0.0.0") (dash "2.18.0") (s "1.12.0") (kinopoisk "0.1"))

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

;; Wrapper of `kinopoisk' for `helm'

;;; Code:

(require 'dash)
(require 'kinopoisk)
(require 's)

(defgroup helm-kinopoisk nil
  "Wrapper of `kinopoisk' for `helm'."
  :group 'tools)

(defvar helm-kinonpoisk-source
  '((name . "HELM Kinopoisk")
    (candidates . helm-kinopoisk--search-candidates)
    (volatile)
    (action .
     (("Just Return "                . identity)
      ("Open Web Page of Kinopoisk " . kinopoisk-film-open-in-web))))
  "Source for `helm-kinopoisk-search'.")

(defun helm-kinopoisk--search-candidates ()
  "Search candidates for `helm-kinopoisk-search'."
  (--map
   (cons (helm-kinopoisk--format-film-for-display it) it)
   (kinopoisk-search-films helm-pattern)))

(defun helm-kinopoisk--format-film-for-display (film)
  "Format `kinopoisk-film' FILM as str for display in `helm'."
  (format
   "(%s) %s"
   (kinopoisk-film-year film)
   (kinopoisk-film-name film)))

(defun helm-kinopoisk-search-films ()
  "Search films from Kinopoisk via `helm'."
  (interactive)
  (helm :sources '(helm-kinonpoisk-source)))

(provide 'helm-kinopoisk)
;;; helm-kinopoisk.el ends here
