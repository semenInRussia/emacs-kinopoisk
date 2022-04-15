;;; kinopoisk.el --- API of `kinopoisk` (cinema-service) for Emacs Lisp

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.18.0") (s "1.12.0"))
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

;; API of `kinopoisk` (cinema-service) for Emacs Lisp

;;; Code:

(require 'dash)
(require 'json)
(require 's)
(require 'url)

(defgroup kinopoisk nil
  "API of Kinopoisk (cinema-service)."
  :group 'tools)

(defcustom kinopoisk-api-key "2f48f0fa-4db5-472a-914c-60134675efb1"
  "API key for Kinopoisk."
  :group 'kinopoisk
  :type 'string)

(defcustom kinopoisk-api-root-url "https://kinopoiskapiunofficial.tech/api"
  "Root URL for Kinopoisk API."
  :group 'kinopoisk
  :type 'string)

(defclass kinopoisk-film ()
  ((id :initarg :id :accessor kinopoisk-film-id)
   (name :initarg :name :accessor kinopoisk-film-name)
   (original-name
    :initarg :original-name
    :accessor kinopoisk-film--original-name)
   (year :initarg :year :accessor kinopoisk-film-year)
   (rating :initarg :rating :accessor kinopoisk-film-rating)
   (poster-url
    :initarg :poster-url
    :accessor kinopoisk-film--poster-url)
   (length :initarg :length :accessor kinopoisk-film-length)
   (countries :initarg :countries :accessor kinopoisk-film-countries)
   (slogan :initarg :slogan :accessor kinopoisk-film--slogan)
   (description :initarg :description :accessor kinopoisk-film-description)
   (short-description
    :initarg :short-description
    :accessor kinopoisk-film--short-description)
   (rating-age-limits
    :initarg :rating-age-limits
    :accessor kinopoisk-film--rating-age-limits))
  "Object for films of Kinopoisk API.")

(defmacro kinopoisk-define-film-field-accessor (field-name)
  "Define field accessor for fild of `kinopoisk-film' with name FIELD-NAME.
Take value of FIELD-NAME from result of `kinopoisk-film-from-id'.
When you use this macros, you should have accessor with followed name:
`kinopoisk-film--<field-name>' (instead of <field-name> put FIELD-NAME)"
  (let* ((field-name-str (symbol-name field-name))
         (simple-accessor
          (intern (s-concat "kinopoisk-film--" field-name-str)))
         (accessor
          (intern (s-concat "kinopoisk-film-" field-name-str))))
    `(defmethod ,accessor
         ((film kinopoisk-film))
       ,(s-lex-format "Get `${field-name-str}' of FILM.")
       (unless (,simple-accessor film)
         (setf
          (,simple-accessor film)
          (->>
           film
           (kinopoisk-film-id)
           (kinopoisk-film-from-id)
           (,simple-accessor))))
       (,simple-accessor film))))

(kinopoisk-define-film-field-accessor original-name)
(kinopoisk-define-film-field-accessor slogan)
(kinopoisk-define-film-field-accessor short-description)
(kinopoisk-define-film-field-accessor rating-age-limits)
(kinopoisk-define-film-field-accessor poster-url)

(defun kinopoisk-search-one-film (query)
  "Search one `kinopoisk-film' in Kinopoisk API, which best match with QUERY."
  (->> query (kinopoisk-search-films) (-first-item)))

(defun kinopoisk-search-films (query)
  "Search some `kinopoisk-films' in Kinopoisk with QUERY."
  (->>
   query
   (kinopoisk-get-json "/v2.1/films/search-by-keyword?keyword=%s")
   (kinopoisk--tracks-from-search-json)))

(defun kinopoisk--tracks-from-search-json (obj)
  "Get tracks from JSON OBJ (result from search by keyword Kinopoisk API)."
  (->>
   obj
   (kinopoisk-get-from-json 'films)
   (-map #'kinopoisk-film-from-json)))

(defun kinopoisk-film-from-id (id)
  "Get `kinopoisk-film' with ID."
  (assert (numberp id)) ; No injections
  (->>
   id
   (kinopoisk-get-json "/v2.2/films/%s")
   (kinopoisk-film-from-json)))

(defun kinopoisk-film-from-json (obj)
  "Get `kinopoisk-film' from JSON OBJ."
  (kinopoisk-film
   :id (kinopoisk-get-from-json '(kinopoiskId filmId) obj)
   :name (kinopoisk-get-from-json 'nameRu obj)
   :original-name (kinopoisk-get-from-json 'nameOriginal obj)
   :year (kinopoisk-get-from-json 'year obj
                                  `(if (numberp val)
                                       (number-to-string val)
                                     val))
   :length (kinopoisk-get-from-json 'filmLength obj
                                    `(kinopoisk--into-film-length val))
   :poster-url (kinopoisk-get-from-json 'posterUrl obj)
   :countries (kinopoisk--film-countries-from-json obj)
   :rating (kinopoisk--film-rating-from-json obj)
   :slogan (kinopoisk-get-from-json 'slogan obj)
   :description (kinopoisk-get-from-json 'description obj)
   :short-description (kinopoisk-get-from-json 'shortDescription obj)
   :rating-age-limits (kinopoisk-get-from-json 'ratingAgeLimits obj)))

(defun kinopoisk--film-rating-from-json (obj)
  "Get rating of `kinopoisk-film' by JSON OBJ of Kinopoisk API."
  (-when-let
      (rating
       (kinopoisk-get-from-json '(rating ratingKinopoisk) obj))
    (when (stringp rating) (setq rating (string-to-number rating)))
    (* 10 rating)))

(defun kinopoisk--into-film-length (from)
  "Try transform FROM to number of minutes, these is length of film.
FROM may be: `number' or `string'"
  (cl-typecase from
    (number from)
    (string (kinopoisk--parse-film-length from))))

(defun kinopoisk--parse-film-length (str)
  "Parse STR into number of minutes, these length of film.
Example of STR - 2:16, this is 136 minutes"
  (-let*
      (((_ _ hours minutes)
        (s-match "\\(\\([0-9]\\)+:\\)?\\([0-9]+\\)" str))
       (hours (if hours (string-to-number hours) 0))
       (minutes (string-to-number minutes)))
    (+ (* hours 60) minutes)))

(defun kinopoisk--film-countries-from-json (obj)
  "Get countries from JSON OBJ of Kinopoisk API."
  (--map
   (gethash "country" it)
   (kinopoisk-get-from-json 'countries obj)))

(defun kinopoisk-get-json (uri &rest format-options)
  "Get JSON string for URI with formatted FORMAT-OPTIONS of Kinopoisk's API."
  (let ((url-request-extra-headers
         `(("X-API-KEY" . ,kinopoisk-api-key))))
    (->>
     (kinopoisk-format-url uri format-options)
     (url-retrieve-synchronously)
     (kinopoisk--json-from-buffer))))

(defun kinopoisk-format-url (uri format-options)
  "Format url with URI for Kinopoisk API, use FORMAT-OPTIONS as in `format'."
  (s-concat kinopoisk-api-root-url
            (apply 'format uri format-options)))

(defun kinopoisk-get-from-json (key obj &optional form)
  "Get KEY of Kinopoisk API JSON OBJ, after call FORM to value."
  (setq form (or form 'val))
  (-some-->
      (cl-typecase key
        (symbol (gethash (symbol-name key) obj))
        (cons
         (kinopoisk-get-from-json          ; I'm very stupid, fix this, please
          (--first (kinopoisk-get-from-json it obj) key)
          obj)))
    (unless (eq it :null) it)
    (eval form `((val . ,it)))))

(defun kinopoisk--json-from-buffer (buffer)
  "In BUFFER get JSON, buffer created via `url-retrieve-synchronously'."
  (with-current-buffer buffer
    (-->
     (buffer-substring (1+ url-http-end-of-headers) (point-max))
     (decode-coding-string it 'utf-8)
     (json-parse-string it))))

(provide 'kinopoisk)

;;; kinopoisk.el ends here
