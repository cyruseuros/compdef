;;; compdef.el --- A stupid completion definer. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Uros Perisic

;; Author: Uros Perisic
;; URL: https://gitlab.com/jjzmajic/compdef
;;
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; A stupid completion definer.

;; We keep reinventing the wheel on how to set completion capf-funs
;; locally.  `compdef' does this for you for both CAPF and company,
;; with some auto-magic thrown in for convenience.  `compdef' is
;; intentionally stupid.  I've seen some really powerful solutions to
;; this problem, but they all seem to assume a certain approach to
;; configuring completions and are thus usually embedded in a starter
;; kit like Doom Emacs, Spacemacs...  `compdef' isn't that clever. It
;; just works.

;;; Code:
(cl-defun compdef (&key modes hooks
                        capf-funs company-funs)
  "Set completion BACKENDS for MODES using HOOKS.
If COMPANY is t, use set `company-funs', else set
`completion-at-point-functions'.  If HOOKS are nil, infer them from
MODES. MODES and HOOKS can be quoted lists as well as atoms."
  (let* ((capf-funs (if (listp capf-funs) capf-funs (list capf-funs)))
         (company-funs (if (listp capf-funs) capf-funs (list capf-funs)))
         (modes (if (listp modes) modes (list modes)))
         (hooks (or hooks (cl-loop for mode in modes collect
                             (intern (concat (symbol-name mode) "-hook"))))))
    (cl-loop for hook in hooks do
             (add-hook hook
                       (lambda ()
                         (when capf-funs
                           (set (make-local-variable 'completion-at-point-functions)
                                capf-funs))
                         (when company-funs
                           (set (make-local-variable 'company-funs)
                                company-funs)))))))

(provide 'compdef)
;;; compdef.el ends here
