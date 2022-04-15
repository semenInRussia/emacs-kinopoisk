;;; test-helper.el --- Helper functions to test kinopoisk  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(defun kinopoisk-ensure-is-valid-film (film)
  "Ensure that FILM is valid object of `kinopoisk-film', check types of fields."
  (should (kinopoisk-film-p film))
  (should (numberp (kinopoisk-film-id film)))
  (should (stringp (kinopoisk-film-name film)))
  (should (stringp (kinopoisk-film-year film)))
  (should (numberp (kinopoisk-film-rating film)))
  (should (< 0 (kinopoisk-film-rating film) 100))
  (should (stringp (kinopoisk-film-poster-url film)))
  (should (stringp (kinopoisk-film-slogan film)))
  (should (stringp (kinopoisk-film-rating-age-limits film)))
  (should (stringp (kinopoisk-film-short-description film)))
  (should (stringp (kinopoisk-film-description film)))
  (should (numberp (kinopoisk-film-length film)))
  (should (listp (kinopoisk-film-countries film))))

;;; test-helper.el ends here
