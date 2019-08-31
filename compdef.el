;;; compdef.el --- A stupid completion definer. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/compdef
;;
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; A stupid completion definer.

;; We keep reinventing the wheel on how to set local completion
;; backends.  `compdef' does this for both CAPF and company
;; simultaneously (in case `company-capf' needs tweaking), with some
;; auto-magic thrown in for convenience.  `compdef' is intentionally
;; stupid.  I've seen some really powerful solutions to this problem,
;; but they all seem to assume a certain approach to configuring
;; completions and are thus usually embedded in a starter kit like
;; Doom Emacs, Spacemacs... `compdef' isn't that clever. It just
;; works.

;;; Code:
(require 'cl-lib)
(require 'derived)

(defvar compdef--use-package-keywords
  '(:compdef :company :capf)
  "Kywords `compdef' adds to `use-package'.")

(defvar company-backends)

(defun compdef--enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun compdef--hook-p (symbol)
  "Check if SYMBOL is a hook."
  (or
   (string-suffix-p "-hook" (symbol-name symbol))
   (string-suffix-p "-functions" (symbol-name symbol))))

;;;###autoload
(cl-defun compdef (&key modes capf company)
  "Set local completion backends for MODES.
Infer hooks for MODES. If actual hooks are passed use them
directly. Set `company-backends' to COMPANY if not nil. Set
`completion-at-point-functions' to CAPF if not nil. All arguments
can be quoted lists as well as atoms."
  (let* ((capf (compdef--enlist capf))
         (company (compdef--enlist company))
         (modes (compdef--enlist modes)))
    (cl-loop for mode in modes
             as hook = (if (compdef--hook-p mode) mode
                         (derived-mode-hook-name mode))
             do (add-hook hook
                          (defalias
                            (intern (format "compdef-%s-fun" (symbol-name hook)))
                            (lambda ()
                              (when capf (setq-local completion-at-point-functions capf))
                              (when company (setq-local company-backends company)))
                            (format
                             "`compdef' for %s."
                             (symbol-name hook)))))))

(with-eval-after-load 'use-package-core
  (declare-function use-package-concat "use-package")
  (declare-function use-package-process-keywords "use-package")
  (defvar use-package-keywords)
  (defvar use-package-deferring-keywords)

  (dolist (keyword compdef--use-package-keywords)
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :init)))

  (defalias 'use-package-normalize/:compdef #'use-package-normalize-symlist)
  (defalias 'use-package-normalize/:company #'use-package-normalize-symlist)
  (defalias 'use-package-normalize/:capf #'use-package-normalize-symlist)

  (defun compdef--plist-multi-delete (plist properties)
    "Delete PROPERTIES from PLIST."
    (if properties
        (compdef--plist-multi-delete
         (use-package-plist-delete plist (car properties))
         (cdr properties))
      plist))

  (defun use-package-handler/:compdef (name keyword args rest state)
    (let ((modes (or (if (eq keyword :compdef)
                         args
                       (plist-get rest :compdef))
                     name))
          (company (plist-get rest :company))
          (capf (plist-get rest :capf)))
      (use-package-concat
       (use-package-process-keywords name
         (compdef--plist-multi-delete
          rest compdef--use-package-keywords)
         state)
       `((compdef
          :modes ',modes
          :company ',company
          :capf ',capf)))))

  (defalias 'use-package-handler/:company #'use-package-handler/:compdef)
  (defalias 'use-package-handler/:capf #'use-package-handler/:compdef))

(provide 'compdef)
;;; compdef.el ends here
