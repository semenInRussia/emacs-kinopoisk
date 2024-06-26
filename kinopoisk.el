;;; kinopoisk.el --- API of `kinopoisk' (cinema-service) for Emacs Lisp

;; Copyright (C) 2022-2023 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semenInRussia/emacs-kinopoisk

;; This file is not part of GNU Emacs.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; API of `kinopoisk` (cinema-service) for Emacs Lisp.  NOTE, that some
;; functions different from original Web API of Kinopoisk.

;;; Code:

;;; Load Libraries

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 's)
(require 'url)

;;; Custom

(defgroup kinopoisk nil
  "API of Kinopoisk (cinema-service)."
  :group 'tools)

(defcustom kinopoisk-api-key "07e275e8-8050-4fa4-8941-435872876974"
  "API key for Kinopoisk.
Visit https://kinopoiskapiunofficial.tech"
  :group 'kinopoisk
  :type 'string)

(defcustom kinopoisk-api-root-url "https://kinopoiskapiunofficial.tech/api"
  "Root URL for Kinopoisk API."
  :group 'kinopoisk
  :type 'string)

(defcustom kinopoisk-types-of-top
  '(best popular await)
  "Types of top, for get top use `kinopoisk-get-films-top'."
  :group 'kinopoisk
  :type 'string)

(defcustom kinopoisk-default-type-of-top
  'best
  "Defalt type of top, for get top use `kinopoisk-get-films-top'.
One of `kinopoisk-types-of-top'."
  :group 'kinopoisk
  :type '(radio
          (const :tag "Top 250 Best Films" best)
          (const :tag "Top 100 Popular Films" popular)
          (const :tag "Top Await Films" await)))

(defcustom kinopoisk-in-search-film-fields
  '(id name description year length countries rating poster-url)
  "Fileds of `kinopoisk-film' which will accessed after search film.
See
https://kinopoiskapiunofficial.tech/documentation/api/#/films/get_api_v2_2_films__id_"
  :group 'kinopoisk
  :type '(repeat symbol))

(defcustom kinopoisk-film-basic-fields
  '((id (kinopoiskId filmId))
    (name (nameRu))
    (original-name (nameOriginal))
    (year
     (year)
     (if (numberp val) (number-to-string val) val))
    (rating
     (rating ratingKinopoisk)
     (kinopoisk--into-film-rating val))
    (web-url (webUrl))
    (poster-url (posterUrl))
    (length (filmLength) (kinopoisk--into-film-length val))
    (countries
     (countries)
     (kinopoisk--from-film-countries-of-json val))
    (genres (genres) (kinopoisk--from-film-genres-of-json val))
    (slogan (slogan))
    (description (description))
    (short-description (shortDescription))
    (is-serial (serial))
    (rating-age-limits (ratingAgeLimits)))
  "This define all fields of `kinopoisk-film' and its way to get from JSON.
This is list in which each element is list from: field's name, keys of JSON, and
after form.  Second is List of keys of JSON from Kinopoisk API, if some taked
then take first valid JSON value.  After form is some Elisp code which will
evaluate after taking from JSON"
  :group 'kinopoisk
  :type 'list)

(defcustom kinopoisk-film-special-fields
  '((videos
     :accessor kinopoisk-film--videos
     :initform nil
     :initarg :videos))
  "Some fields of `kinopoisk-film' which not included to search by ID.
This is list in which each element is list from car field's name and cdr which
is options of `defclass' fields defnition"
  :group 'kinopoisk
  :type 'list)

;;; Macros

(defun kinopoisk--get-class-field (field)
  "Get sexp expression of `kinopoisk-film' FIELD.
See `kinopoisk-film-basic-fields'."
  (let* ((symbol (kinopoisk-film-field-symbol field))
         (initarg (kinopoisk-film-field-initarg field))
         (accessor (kinopoisk-film-field-accessor symbol)))
    `(,symbol
      :initarg ,initarg
      :accessor ,accessor)))

(defun kinopoisk-film-field-symbol (field)
  "Get symbol of FIELD, FIELD is one of `kinopoisk-film-basic-fields'."
  (car field))

(defun kinopoisk-film-field-json-keys (field)
  "Get keys from JSON of FIELD, FIELD is one of `kinopoisk-film-basic-fields'."
  (-second-item field))

(defun kinopoisk-film-field-after-form (field)
  "Get after form of FIELD, FIELD is one of `kinopoisk-film-basic-fields'."
  (if (= (length field) 3) (-last-item field) 'val))

(defun kinopoisk-search-film-field-p (field)
  "Return t, when FIELD is one of fields which accessed after search film.
Using `kinopoisk-in-search-film-fields'"
  (-contains-p kinopoisk-in-search-film-fields field))

(defun kinopoisk-film-field-accessor (field)
  "Get accessor for FIELD of `kinopoisk-film'."
  (let ((prefix
         (if (kinopoisk-search-film-field-p field)
             "kinopoisk-film-"
           "kinopoisk-film--")))
    (->> field (symbol-name) (s-prepend prefix) (intern))))

(defun kinopoisk-film-field-initarg (field)
  "Get initarg of FIELD, which is one of `kinopoisk-film-basic-fields'."
  (->>
   field
   (kinopoisk-film-field-symbol)
   (symbol-name)
   (s-prepend ":")
   (intern)))

(defun kinopoisk--film-field-from-json (field)
  "Get sexp expression for `kinopoisk-from-id' for FIELD.
FIELD is one of `kinopoisk-film-basic-fields'.
One condition is that in function `kinopoisk-from-json' argument of JOSN object
called `obj'"
  (let* ((initarg (kinopoisk-film-field-initarg field))
         (after-form (kinopoisk-film-field-after-form field))
         (json-keys (kinopoisk-film-field-json-keys field)))
    `(,initarg
      (kinopoisk-get-from-json ',json-keys obj ',after-form))))

(defmacro kinopoisk-define-film-from-json ()
  "Define `kinopoisk-film-from-json' func using `kinopoisk-film-basic-fields'."
  `(defun kinopoisk-film-from-json (obj)
     "Get `kinopoisk-film' from JSON OBJ."
     (kinopoisk-film
      ,@(--mapcat
         (kinopoisk--film-field-from-json it)
         kinopoisk-film-basic-fields))))

(defmacro kinopoisk-define-film-field-accessor (field-name &optional accessor)
  "Define field accessor for field with name FIELD-NAME of `kinopoisk-film'.
If ACCESSOR is symbol, then name of function-accessor will be ACCESSOR.  Take
value of FIELD-NAME from result of `kinopoisk-film-from-id'.  When you use this
macros, you should have accessor with followed name:
`kinopoisk-film--<field-name>' (instead of <field-name> put FIELD-NAME)"
  (let* ((field-name-str (symbol-name field-name))
         (simple-accessor
          (intern (s-concat "kinopoisk-film--" field-name-str)))
         (accessor
          (or
           accessor
           (intern (s-concat "kinopoisk-film-" field-name-str)))))
    `(cl-defmethod ,accessor
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

(defmacro kinopoisk-define-film-class ()
  "Define symbol `kinopoisk-film' class, using `kinopoisk-film-basic-fields'."
  `(progn
     (defclass kinopoisk-film ()
       ,(-concat
         (--map
          (kinopoisk--get-class-field it)
          kinopoisk-film-basic-fields)
         kinopoisk-film-special-fields)
       "Object for films of Kinopoisk API.")
     (kinopoisk-define-film-field-accessor original-name)
     (kinopoisk-define-film-field-accessor slogan)
     (kinopoisk-define-film-field-accessor short-description)
     (kinopoisk-define-film-field-accessor genres)
     (kinopoisk-define-film-field-accessor rating-age-limits)
     (kinopoisk-define-film-field-accessor web-url)
     (kinopoisk-define-film-field-accessor is-serial
                                           kinopoisk-film-is-serial-p)))

(defun kinopoisk-film-basic-fields-define-functions ()
  "Define some functions for `kinopoisk-film' which depends on film fields.
See `kinopoisk-film-basic-fields'."
  (kinopoisk-define-film-class)
  (kinopoisk-define-film-from-json))

;;; Search Film

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

;;; Film by ID

(defun kinopoisk-film-from-id (id)
  "Get `kinopoisk-film' with ID."
  (and
   (numberp id)                         ; defender from injections
   (->>
    id
    (kinopoisk-get-json "/v2.2/films/%s")
    (kinopoisk-film-from-json))))

;;; Film Length

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

(defun kinopoisk--into-film-rating (from)
  "Get from FROM rating of film, FROM may be one of `number', `string'.
Rating is number from 0 to 100"
  (->
   (cl-typecase from ;nofmt
     (number from)
     (string (string-to-number from)))
   (* 10)))

;;; Parse from JSON

(defun kinopoisk--from-film-countries-of-json (countries)
  "Take COUNTRIES as value of film's JSON, return normal list of countries."
  (--map (gethash "country" it) countries))

(defun kinopoisk--from-film-genres-of-json (genres)
  "Take GENRES as value of film's JSON, return normal list of genres."
  (--map (gethash "genre" it) genres))

;;; Fetch JSON

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
    (if (eq it :false) nil it)
    (if (eq it :true) t it)
    (eval form `((val . ,it)))))

(defvar url-http-end-of-headers)
(defun kinopoisk--json-from-buffer (buffer)
  "In BUFFER get JSON, buffer created via `url-retrieve-synchronously'."
  (with-current-buffer buffer
    (-->
     (buffer-substring (1+ url-http-end-of-headers) (point-max))
     (decode-coding-string it 'utf-8)
     (json-parse-string it))))

;;; Define Classes with Fields

(kinopoisk-film-basic-fields-define-functions)

(cl-defmethod kinopoisk-film-open-in-web
  ((film kinopoisk-film))
  "Open FILM in web browser."
  (->> film (kinopoisk-film-web-url) (browse-url)))

(defun kinopoisk-print-film (film)
  "Print a `kinopoisk' FILM."
  (message
   (concat "Film \""
           (kinopoisk-film-name film)
           "\"")))

(cl-defmethod kinopoisk-film-copy-web-url
  ((film kinopoisk-film))
  "Copy Web Url of page on Kinopoisk about FILM."
  (->> film (kinopoisk-film-web-url) (kill-new)))

(cl-defmethod kinopoisk-film-copy-name
  ((film kinopoisk-film))
  "Copy the name of a FILM."
  (->> film (kinopoisk-film-name) (kill-new)))

(cl-defmethod kinopoisk-film-copy-description
  ((film kinopoisk-film))
  "Copy the description of a FILM."
  (->> film (kinopoisk-film-description) (kill-new)))

(cl-defmethod kinopoisk-film-videos
  ((film kinopoisk-film))
  "Get videos (`kinopoisk-film-video') of FILM (trailers and etc.)."
  (or
   (kinopoisk-film--videos film)
   (setf
    (kinopoisk-film--videos film)
    (kinopoisk--search-videos-of-film film))))

;;; Video

(defclass kinopoisk-film-video ()
  ((name :initarg :name :accessor kinopoisk-film-video-name)
   (site :initarg :site :accessor kinopoisk-film-video-site)
   (url :initarg :url :accessor kinopoisk-film-video-url))
  "Video for film, for get use `kinopoisk-film-videos'")

(cl-defmethod kinopoisk-film-video-open-in-web
  ((video kinopoisk-film-video))
  "Open VIDEO in Web Browser."
  (->> video (kinopoisk-film-video-url) (browse-url)))

(cl-defmethod kinopoisk--search-videos-of-film
  ((film kinopoisk-film))
  "Search videos (`kinopoisk-film-video') for FILM (trailers and etc.)."
  (->>
   film
   (kinopoisk-film-id)
   (kinopoisk-get-json "/v2.2/films/%s/videos")
   (kinopoisk-get-from-json 'items)
   (-map #'kinopoisk-film-video-from-json)))

(defun kinopoisk-film-video-from-json (obj)
  "Get from JSON OBJ of Kinopoisk API `kinopoisk-film-video'."
  (kinopoisk-film-video
   :name (kinopoisk-get-from-json 'name obj)
   :url (kinopoisk-get-from-json 'url obj)
   :site (kinopoisk-get-from-json 'site obj)))

;;; Top of Films

(defcustom kinopoisk--films-of-top-per-page 20
  "Number of films a one page of Kinopoisk top.
See function `kinopoisk-get-nth-film-of-top'"
  :group 'kinopoisk
  :type 'number)

(defun kinopoisk-get-nth-film-of-top (n &optional type)
  "Get Nth film of top with type TYPE, without see to cash.
See `kinopoisk-get-films-top'"
  (let* ((page (/ n kinopoisk--films-of-top-per-page))
         (n-on-page (% n kinopoisk--films-of-top-per-page))
         (films-of-page (kinopoisk-get-films-top type page)))
    (nth n-on-page films-of-page)))

(defun kinopoisk-get-films-top (&optional type page)
  "Get top with type TYPE of films (`kinopoisk-film') from Kinopoisk.
Return 20 `kinopoisk-film'.  If PAGE is non-nil then return PAGE-th top.  TYPE
is one of `kinopoisk-types-of-top'.  TYPE defaults to
`kinopoisk-default-type-of-top'"
  (->>
   (kinopoisk--format-url-of-films-top type page)
   (kinopoisk-get-json)
   (kinopoisk-get-from-json 'films)
   (-map #'kinopoisk-film-from-json)))

(defun kinopoisk--format-url-of-films-top (&optional type page)
  "Get url for get top of films, on page PAGE and with TYPE.

Note, that in original Kinopoisk API minimal page is 1, but this isn't
convinence, so PAGE will automatically increment"
  (or type (setq type kinopoisk-default-type-of-top))
  (or page (setq page 0))
  (cl-incf page)
  (format
   "/v2.2/films/top?page=%s&type=%s"
   page
   (kinopoisk--get-type-of-top-full-name type)))

(defun kinopoisk--get-type-of-top-full-name (type)
  "Get full name of TYPE of films top.
TYPE is one of `kinopoisk-types-of-top'.  TYPE defaults to
`kinopoisk-default-type-of-top'"
  (cl-case type
    (best "TOP_250_BEST_FILMS")
    (popular "TOP_100_POPULAR_FILMS")
    (await "TOP_AWAIT_FILMS")))

(defun kinopoisk-format-type-of-top (type)
  "Format TYPE to humanize string."
  (cl-case type
    (best "Top of 250 best films")
    (popular "Top 100 Popular Films")
    (await "Top Await Films")))

(defun kinopoisk-extend-films-top (top &optional type)
  "Add to TOP with TYPE some next films of top and return extended TOP.
See `kinopoisk-get-films-top'.  Note, that this function is pure."
  (let ((page (/ (length top) kinopoisk--films-of-top-per-page)))
    (append top (kinopoisk-get-films-top type page))))

(provide 'kinopoisk)
;;; kinopoisk.el ends here
