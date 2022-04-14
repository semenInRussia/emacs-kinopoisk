;;; kinopoisk-test.el --- Tests for kinopoisk.el

;; Copyright (C) 2013 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>

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

;; Tests for kinopoisk.el

;;; Code:

(require 'ert)
(require 'kinopoisk)
(require 'ht)

(ert-deftest kinopoisk-search-films
    ()
  (let ((film (car (kinopoisk-search-films "матрица"))))
    (should (kinopoisk-film-p film))
    (should (equal (kinopoisk-film-original-name film) "The Matrix"))
    (should (numberp (kinopoisk-film-id film)))
    (should (stringp (kinopoisk-film-name film)))
    (should (stringp (kinopoisk-film-year film)))
    (should (numberp (kinopoisk-film-rating film)))
    (should (< 0 (kinopoisk-film-rating film) 100))
    (should (stringp (kinopoisk-film-poster-url film)))
    (should (numberp (kinopoisk-film-length film)))
    (should (listp (kinopoisk-film-countries film)))))

(ert-deftest kinopoisk-film-from-id
    ()
  (let ((film (kinopoisk-film-from-id 301)))
    (should (kinopoisk-film-p film))
    (should (equal (kinopoisk-film-original-name film) "The Matrix"))
    (should (numberp (kinopoisk-film-id film)))
    (should (stringp (kinopoisk-film-name film)))
    (should (stringp (kinopoisk-film-year film)))
    (should (numberp (kinopoisk-film-rating film)))
    (should (< 0 (kinopoisk-film-rating film) 100))
    (should (stringp (kinopoisk-film-poster-url film)))
    (should (numberp (kinopoisk-film-length film)))
    (should (listp (kinopoisk-film-countries film)))))

(ert-deftest kinopoisk-format-url
    ()
  (should
   (equal
    (kinopoisk-format-url "/v2.2/films/%s" '(3))
    "https://kinopoiskapiunofficial.tech/api/v2.2/films/3")))

(ert-deftest kinopoisk-check-get-from-json
    ()
  (let* ((obj (ht ("a" "1") ("b" "2"))))
    (should (equal (kinopoisk-get-from-json 'a obj) "1"))))

(ert-deftest kinopoisk-check-get-from-json-multi-keys
    ()
  (let* ((obj (ht ("a" "1") ("b" "2"))))
    (should (equal (kinopoisk-get-from-json '(c a b) obj) "1"))))

(ert-deftest kinopoisk-check-get-from-json-and-map-func
    ()
  (let* ((obj (ht ("a" "1") ("b" "2"))))
    (should
     (equal
      (kinopoisk-get-from-json 'a obj '(string-to-number val))
      1))))

(ert-deftest kinopoisk-check--into-film-length
    ()
  (should (= (kinopoisk--into-film-length 136) 136))
  (should (= (kinopoisk--into-film-length "136") 136))
  (should (= (kinopoisk--into-film-length "2:16") 136)))

(provide 'kinopoisk-test)

;;; kinopoisk-test.el ends here
