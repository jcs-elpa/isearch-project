;;; isearch-project.el --- Incremental search through the whole project.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-03-18 15:16:04

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Incremental search through the whole project.
;; Keyword: convenience, search
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (cl-lib "0.6"))
;; URL: https://github.com/jcs090218/isearch-project

;; This file is NOT part of GNU Emacs.

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
;; Incremental search through the whole project.
;;

;;; Code:

(require 'cl-lib)
(require 'isearch)


(defgroup isearch-project nil
  "Incremental search through the whole project."
  :prefix "isearch-project-"
  :group 'isearch
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/isearch-project"))


(defvar isearch-project-search-path ""
  "Record the current search path, so when next time it searhs would not \
need to research from the start.")

(defvar isearch-project-project-dir ""
  "Current isearch project directory.")

(defvar isearch-project-files '()
  "List of file path in the project.")

(defvar isearch-project-files-starting-index -1
  "Starting search path index, use with `isearch-project-files' list.")

(defvar isearch-project-files-current-index -1
  "Current search path index, use with `isearch-project-files' list.")

(defvar isearch-project-run-advice t
  "Flag to check if run advice.")


(defun isearch-project-prepare ()
  "Incremental search preparation."
  (let ((prepare-success nil))
    (setq isearch-project-project-dir (cdr (project-current)))
    (when isearch-project-project-dir
      ;; Get the current buffer name.
      (setq isearch-project-search-path (buffer-file-name))
      ;; Get all the file from the project.
      (setq isearch-project-files (directory-files-recursively isearch-project-project-dir ""))
      ;; Reset to -1.
      (setq isearch-project-files-current-index -1)

      (setq prepare-success t))
    prepare-success))


(define-key isearch-mode-map (kbd "C-s") #'isearch-project-forward-symbol-at-point)
(define-key isearch-mode-map (kbd "C-r") #'isearch-project-backward-symbol-at-point)

;;;###autoload
(defun isearch-project-backward-symbol-at-point ()
  "Incremental search backward at current point in the project."
  (interactive)
  (isearch-project-symbol-at-point 'backward))

;;;###autoload
(defun isearch-project-forward-symbol-at-point ()
  "Incremental search forward at current point in the project."
  (interactive)
  (isearch-project-symbol-at-point 'forward))

(defun isearch-project-symbol-at-point (&optional dt)
  "Incremental search at current point in the project.
DT : search direction."
  (if (isearch-project-prepare)
      (progn
        (isearch-project-add-advices)
        (isearch-forward-symbol-at-point)
        (isearch-repeat dt)
        (when (eq dt 'backward)
          (isearch-repeat dt))
        (isearch-project-remove-advices))
    (error "Cannot isearch project without project directory defined")))

;;;###autoload
(defun isearch-project-forward ()
  "Incremental search in the project."
  (interactive)
  (if (isearch-project-prepare)
      (progn
        (isearch-project-add-advices)
        (isearch-forward)
        (isearch-project-remove-advices))
    (error "Cannot isearch project without project directory defined")))


(defun isearch-project-add-advices ()
  "Add all needed advices."
  (advice-add 'isearch-repeat :after #'isearch-project-advice-isearch-repeat-after))

(defun isearch-project-remove-advices ()
  "Remove all needed advices."
  (advice-remove 'isearch-repeat #'isearch-project-advice-isearch-repeat-after))


(defun isearch-project-contain-string (in-sub-str in-str)
  "Check if a string is a substring of another string.
Return true if contain, else return false.
IN-SUB-STR : substring to see if contain in the IN-STR.
IN-STR : string to check by the IN-SUB-STR."
  (string-match-p in-sub-str in-str))

(defun isearch-project-get-string-from-file (filePath)
  "Return filePath's file content.
FILEPATH : file path."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun isearch-project-find-file-search (fn dt)
  "Open a file and isearch.
If found, leave it.  If not found, try find the next file.
FN : file to search.
DT : search direction."
  (find-file fn)
  (cond ((eq dt 'forward)
         (goto-char (point-min)))
        ((eq dt 'backward)
         (goto-char (point-max))))

  (isearch-search-string isearch-string nil t)

  (let ((isearch-project-run-advice nil))
    (cond ((eq dt 'forward)
           (isearch-repeat-forward))
          ((eq dt 'backward)
           (isearch-repeat-backward)))))


(defun isearch-project-advice-isearch-repeat-after (dt &optional cnt)
  "Advice when do either `isearch-repeat-backward' or `isearch-repeat-forward' \
command.
DT : search direction.
CNT : search count."
  (when (and (not isearch-success)
             isearch-project-run-advice)
    (let ((next-file-index isearch-project-files-current-index)
          (next-fn ""))
      ;; Get the next file index.
      (when (= isearch-project-files-current-index -1)
        (setq isearch-project-files-current-index
              (cl-position isearch-project-search-path
                           isearch-project-files
                           :test 'string=))
        (setq isearch-project-files-starting-index isearch-project-files-current-index)
        (setq next-file-index isearch-project-files-current-index))

      ;; Cycle it.
      (cond ((eq dt 'backward)
             (when (< next-file-index 0)
               (setq next-file-index (- (length isearch-project-files) 1))))
            ((eq dt 'forward)
             (when (>= next-file-index (length isearch-project-files))
               (setq next-file-index 0))))

      ;; Target the next file.
      (setq next-fn (nth next-file-index isearch-project-files))


      (let ((buf-content (isearch-project-get-string-from-file next-fn))
            (break-it nil)
            (search-cnt (if cnt cnt 1)))
        (while (not break-it)
          ;; Get the next file index.
          (cond ((eq dt 'backward)
                 (setq next-file-index (- next-file-index 1)))
                ((eq dt 'forward)
                 (setq next-file-index (+ next-file-index 1))))

          ;; Cycle it.
          (cond ((eq dt 'backward)
                 (when (< next-file-index 0)
                   (setq next-file-index (- (length isearch-project-files) 1))))
                ((eq dt 'forward)
                 (when (>= next-file-index (length isearch-project-files))
                   (setq next-file-index 0))))

          ;; Target the next file.
          (setq next-fn (nth next-file-index isearch-project-files))

          ;; Found match.
          (if (isearch-project-contain-string isearch-string buf-content)
              (progn
                (setq search-cnt (- search-cnt 1))
                (if (<= search-cnt 0)
                    (setq break-it t)))
            ;; Update buffer content.
            (setq buf-content (isearch-project-get-string-from-file next-fn)))))

      ;; Open the file.
      (isearch-project-find-file-search next-fn dt)

      ;; Update current file index.
      (setq isearch-project-files-current-index next-file-index))))


(provide 'isearch-project)
;;; isearch-project.el ends here
