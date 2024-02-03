;;; helm-kinopoisk.el --- Wrapper of `kinopoisk' for `helm'

;; Copyright (C) 2022 Semen Khramtsov

;; Author: Semen Khramtsov <hrams205@gmail.com>
;; Version: 0.1

;; Package-Requires: ((emacs "27.1") (helm "3.9.5") (dash "2.18.0") (s "1.12.0"))

;; Homepage: https://github.com/semenInRussia/emacs-kinopoisk

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Wrapper of `kinopoisk' for `helm'

;;; Code:

(require 'kinopoisk)

(require 'helm)
(require 'dash)
(require 'cl-lib)
(require 's)


(defgroup helm-kinopoisk nil
  "Support of `helm' for `kinopoisk'."
  :group 'tools
  :prefix 'helm-kinopoisk-)

(defcustom helm-kinopoisk-film-actions
  '(("Choose Action"                 . helm-kinopoisk--handle-film)
    ("Copy URL of Page on Kinopoisk" . kinopoisk-film-copy-web-url)
    ("Open Web Page on Kinopoisk"    . kinopoisk-film-open-in-web)
    ("See Videos about Film"         . helm-kinopoisk--film-videos))
  "Actions for `helm-kinopoisk-search'."
  :group 'helm-kinopoisk
  :type '(alist :key-type string :value-type symbol-function))

(defvar helm-kinonpoisk-search-source
  `((name . "HELM Kinopoisk")
    (candidates . helm-kinopoisk--search-candidates)
    (volatile)
    (persistent-action . kinopoisk-film-open-in-web)
    (persistent-help . "See in the Web Browser")
    (action . ,helm-kinopoisk-film-actions))
  "Source for `helm-kinopoisk-search'.")

(defun helm-kinopoisk--handle-film (film)
  "Do any `helm' action with FILM (`kinopoisk-film')."
  (funcall (helm-kinopoisk--choose-search-action) film))

(defun helm-kinopoisk--choose-search-action ()
  "Choose one of `helm-kinopoisk-film-actions', ignoring first."
  (let ((action-name
         (->>
          helm-kinopoisk-film-actions
          (cdr)
          (completing-read
           "Choose one of this action for `kinopoisk-search'."))))
    (alist-get
     action-name
     helm-kinopoisk-film-actions
     nil
     nil
     #'string-equal)))

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

(defun helm-kinopoisk--film-videos (film)
  "See videos of FILM via `helm'."
  (let ((source-name
         (format
          "HELM Videos of Film \"%s\""
          (kinopoisk-film-name film)))
        (candidates (helm-kinopoisk--film-videos-candidates film)))
    (helm
     :sources
     `((name . ,source-name)
       (candidates . ,candidates)
       (action . (("See Video" . kinopoisk-film-video-open-in-web)))))))

(defun helm-kinopoisk--film-videos-candidates (film)
  "Return candidates for see videos of `kinopoisk-film' FILM via `helm'."
  (->>
   film
   (kinopoisk-film-videos)
   (--map (cons (kinopoisk-film-video-name it) it))))

;;;###autoload
(defun helm-kinopoisk-search-films ()
  "Search films from Kinopoisk via `helm'."
  (interactive)
  (helm :sources '(helm-kinonpoisk-search-source)))

(defvar helm-kinopoisk-see-films-top--type nil
  "Type of films top from Kinopoisk.
See `kinopoisk-get-films-top'")

(defvar helm-kinopoisk-see-films-top-source
  `((name                   . "HELM Kinopoisk Top of Films")
    (candidates             . helm-kinopoisk-see-films-top--candidates)
    (action                 . ,helm-kinopoisk-film-actions)
    (persistent-action      . kinopoisk-film-open-in-web)
    (persistent-help        . "See in the Web Browser")
    (cleanup                . helm-kinopoisk-see-films-top--cleanup)
    (init                   . helm-kinopoisk-see-films-top--init)
    (candidate-number-limit . 250)))

;;;###autoload
(defun helm-kinopoisk-see-films-top (type)
  "See top of films from Kinopoisk with type TYPE via `helm'."
  (interactive (list (helm-kinopoisk-read-type-of-top)))
  (let ((helm-kinopoisk-see-films-top--type type))
    (helm :sources '(helm-kinopoisk-see-films-top-source))))

(defun helm-kinopoisk-read-type-of-top ()
  "Read from user type of Kinopoisk films top.
One of `kinopoisk-types-of-top'."
  (let ((candidates
         (->>
          kinopoisk-types-of-top
          (--map (cons (kinopoisk-format-type-of-top it) it)))))
    (helm
     :sources ;nofmt
     `((name . "HELM Kinopoisk Type of Top")
       (candidates . ,candidates)
       (action . (("Choose" . identity)))))))

(defvar helm-kinopoisk--films-of-top nil
  "Already found films of Kinopoisk film, see `kinopoisk-get-films-top'.")

(defun helm-kinopoisk-see-films-top--candidates ()
  "Get helm candidates for `helm-kinopoisk-see-films-top'."
  (helm-kinopoisk-see-films-top--update)
  (--map
   (cons (helm-kinopoisk--format-film-for-display it) it)
   helm-kinopoisk--films-of-top))

(defun helm-kinopoisk-see-films-top--update ()
  "Update helm candidates for `helm-kinopoisk-see-films-top'."
  (setq
   helm-kinopoisk--films-of-top
   (kinopoisk-extend-films-top
    helm-kinopoisk--films-of-top helm-kinopoisk-see-films-top--type)))

(defun helm-kinopoisk-see-films-top--init ()
  "Init for `helm-kinopoisk-see-films-top'."
  (add-hook
   'helm-move-selection-after-hook
   #'helm-kinopoisk-see-films-top--update-sel))

(defun helm-kinopoisk-see-films-top--update-sel ()
  "Update selection for `helm-kinopoisk-see-films-top'."
  (when (>=
         (helm-candidate-number-at-point)
         (length helm-kinopoisk--films-of-top))
    (helm-force-update)))

(defun helm-kinopoisk-see-films-top--cleanup ()
  "Clean up variables and hooks of `helm-kinopoisk-see-films-top'."
  (setq helm-kinopoisk--films-of-top nil)
  (remove-hook
   'helm-move-selection-after-hook
   #'helm-kinopoisk-see-films-top--update-sel))

(provide 'helm-kinopoisk)
;;; helm-kinopoisk.el ends here
