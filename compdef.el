;;; compdef.el --- A stupid completion definer. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/compdef
;;
;; Version: 0.2
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
  '(:compdef :capf :company)
  "Keywords `compdef' adds to `use-package'.")

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
             do (add-hook
                 hook
                 (lambda ()
                   (when capf (setq-local completion-at-point-functions capf))
                   (when company (setq-local company-backends company)))))))

(with-eval-after-load 'use-package-core
  (declare-function use-package-concat "use-package")
  (declare-function use-package-process-keywords "use-package")
  (declare-function use-package-normalize-symlist "use-package")
  (defvar use-package-keywords)

  (dolist (keyword compdef--use-package-keywords)
    (setq use-package-keywords
          (use-package-list-insert
           keyword use-package-keywords :init)))

  (defalias 'use-package-normalize/:compdef #'use-package-normalize-symlist)
  (defalias 'use-package-normalize/:company #'use-package-normalize-symlist)
  (defalias 'use-package-normalize/:capf #'use-package-normalize-symlist)

  (defun use-package-handler/:compdef (name _keyword args rest state)
    (use-package-process-keywords name rest
      (plist-put state :compdef args)))

  (defun compdef--use-package-handler (name keyword args rest state)
    (use-package-concat
     (use-package-process-keywords name rest state)
     `((compdef
        :modes ',(or (plist-get state :compdef) name)
        ,keyword ',args))))

  (defalias 'use-package-handler/:capf #'compdef--use-package-handler)
  (defalias 'use-package-handler/:company #'compdef--use-package-handler))

(provide 'compdef)
;;; compdef.el ends here
