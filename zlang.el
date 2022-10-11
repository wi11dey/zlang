;;; zlang.el --- zlang major mode for Emacs -*- lexical-binding: t -*-

;; Author: Will Dey
;; Version: 1.0
;; Keywords: zlang, languages, lisp

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; For a full copy of the GNU Affero General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'scheme)

(defconst zlang-font-lock-keywords-2
  (append scheme-font-lock-keywords-2
	  '(("(\\(function\\)\\>" . 1)
	    ("'\\(\\sw+\\)\\>" 1 font-lock-variable-name-face))))

(defun zlang-function-indent (&rest args)
  (when (derived-mode-p 'zlang-mode)
    (apply #'lisp-indent-specform 1 args)))
(put 'function 'scheme-indent-function #'zlang-function-indent)

(define-derived-mode zlang-mode scheme-mode "zlang"
  "Major mode for editing zlang."
  (setf (car font-lock-defaults) '(scheme-font-lock-keywords
				   scheme-font-lock-keywords-1
				   zlang-font-lock-keywords-2)))

(provide 'zlang)

;;; zlang.el ends here
