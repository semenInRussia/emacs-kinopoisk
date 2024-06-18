;;; embark-kinopoisk.el --- Create `embark' keymaps and finders for films using `kinopoisk' (cinema-service) -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 semenInRussia

;; Author: semenInRussia <hrams205@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (dash "2.18.0") (s "1.12.0"))
;; Homepage: https://github.com/semenInRussia/emacs-kinopoisk

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

(declare-function helm-kinopoisk--film-videos "helm-kinopoisk")

(defvar-keymap embark-kinopoisk-film-map
  :doc "Keymap with a few film actions using `kinopoisk'."
  :parent embark-general-map
  "RET" #'kinopoisk-film-open-in-web
  "v" #'helm-kinopoisk--film-videos
  "w w" #'kinopoisk-film-copy-web-url
  "w u" #'kinopoisk-film-copy-web-url
  "w n" #'kinopoisk-film-copy-name
  "w d" #'kinopoisk-film-copy-description)

(defun embark-kinopoisk-film ()
  "Target a film at point of the form \"Film Name\".

Note that the cursor should be located before the opening double quote and the
result is either nil or list where the `car' is symbol \\='kinopoisk-film and
the second element has the type kinopoisk-film"
  (save-excursion
    (when (and (looking-at "\"") (not (forward-char 1)))
      (let* ( ;; skip opening quote
             (start (point))
             (end (if (search-forward "\"" nil :noerror)
                      (1- (point))
                    (point-max)))
             (name (buffer-substring-no-properties start end))
             (film (kinopoisk-search-one-film name)))
        `(kinopoisk-film ,film
                         ,start . ,end)))))

(defgroup embark-kinopoisk nil
  "Support of `embark' for `kinopoisk'."
  :group 'tools)

;;;###autoload
(define-minor-mode embark-kinopoisk-mode
  "Support of `embark' for films using Kinopoisk API."
  :global t
  :group 'embark-kinopoisk
  (cond
   (embark-kinopoisk-mode
    ;; enable
    (add-to-list 'embark-target-finders #'embark-kinopoisk-film)
    (setf (alist-get 'kinopoisk-film embark-keymap-alist)
          'embark-kinopoisk-film-map))
   (t
    ;; disable
    (setq embark-target-finders (delete 'embark-kinopoisk-film embark-target-finders))
    (setq embark-keymap-alist (delete '(kinopoisk-film . embark-kinopoisk-film-map)
                                      embark-keymap-alist)))))

(provide 'embark-kinopoisk)
;;; embark-kinopoisk.el ends here
