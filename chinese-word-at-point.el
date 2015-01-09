;;; chinese-word-at-point.el --- Add `chinese-word' to `thing-at-point' function  -*- coding: utf-8; -*-

;; Copyright © 2015 Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/chinese-word-at-point.el
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.1
;; Created: 9 Jan 2015
;; Keywords: convenience, Chinese

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

;; This file provides an additional `chinese-word' thing for `thing-at-point'
;; function.

;; Using:
;;
;; 1. Add (require 'chinese-word-at-point) to your elisp code.
;; 2. Use (thing-at-point 'chinese-word).

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

(defvar chinese-word-split-command
  "echo %s | python -m jieba -q -d ' '"
  "Set command for Chinese text segmentation.
The result should separated by one space.

I know two Chinese word segmentation tools, which have command line
interface, are jieba (结巴中文分词) and scws, both of them are hosting
on Github.")

(defun chinese-word--split-by-space (chinese-string)
  "Split CHINESE-STRING by one space.
Return Chinese words as a string separated by one space"
  (shell-command-to-string
   (format chinese-word-split-command chinese-string)))

(defun chinese-word-at-point-bounds ()
  "Return the bounds of the (most likely) Chinese word at point."
  (save-excursion
    ;; FIXME: only Chinese string should be split
    (when (thing-at-point 'word)
      (let* ((boundary (bounds-of-thing-at-point 'word))
             (beginning-pos (car boundary))
             (end-pos (cdr boundary))
             (current-pos (point))
             (index beginning-pos)
             (old-index beginning-pos))
        (dolist (word (split-string (chinese-word--split-by-space
                                     (thing-at-point 'word t))))
          (cl-incf index (length word))
          (if (and (>= current-pos old-index)
                   (< current-pos index))
              (cl-return (cons old-index index))
            (if (= index end-pos)       ; When point is just behind word
                (cl-return (cons old-index index)))
            (setq old-index index)))))))

(put 'chinese-word 'bounds-of-thing-at-point 'chinese-word-at-point-bounds)

;;;###autoload
(defun chinese-word-at-point ()
  "Return the (most likely) Chinese word at point, or nil if none is found."
  (thing-at-point 'chinese-word))

(provide 'date-at-point)
;;; chinese-word-at-point.el ends here
