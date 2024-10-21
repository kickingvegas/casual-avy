;;; casual-avy.el --- Transient UI for Avy -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-avy
;; Keywords: tools
;; Version: 1.4.4-rc.1
;; Package-Requires: ((emacs "29.1") (avy "0.5.0") (casual "2.0.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Casual Avy is an opinionated Transient-based menu for Avy.

;; INSTALLATION
;; (require 'casual-avy) ; optional if using autoloaded menu
;; (keymap-global-set "M-g" #'casual-avy-tmenu)

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this. For more details, please refer to the "Install" section on
;; this project's repository web page.

;;; Code:

(require 'transient)
(require 'avy)
(require 'display-line-numbers)
(require 'imenu)
(require 'org)
(require 'casual-lib)
(require 'casual-avy-version)

(define-obsolete-variable-alias 'casual-avy-use-unicode-symbols
  'casual-lib-use-unicode
  "1.2.0")

(defcustom casual-avy-use-unicode-symbols nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'avy)

(make-obsolete-variable 'casual-avy-imenu-modes nil "1.4.0")

(defcustom casual-avy-imenu-modes '(prog-mode makefile-mode)
  "List of modes to enable Imenu item in `casual-avy-tmenu'."
  :type '(repeat symbol)
  :group 'avy)

(defun casual-avy--customize-casual-avy-imenu-modes ()
  "Customize variable `casual-avy-imenu-modes'."
  (interactive)
  (customize-variable 'casual-avy-imenu-modes))

(defun casual-avy--customize-avy-group ()
  "Call the Avy customization group."
  (interactive)
  (customize-group "avy"))

(defconst casual-avy-unicode-db
  '((:scope . '("⬍" "#"))
    (:previous . '("↑" "Previous"))
    (:next . '("↓" "Next")))
  "Unicode symbol DB to use for Avy Transient menus.")

(defun casual-avy-unicode-get (key)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.

If the value of customizable variable `casual-lib-use-unicode'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (casual-lib-unicode-db-get key casual-avy-unicode-db))

(defun casual-avy-org-mode-p ()
  "Predicate to test if `org-mode' is enabled."
  (derived-mode-p 'org-mode))

(defun casual-avy-imenu-support-p ()
  "Predicate to test if current mode supports `imenu'."
  (if imenu--index-alist t nil))

(defun casual-avy-select-above-below (avy-fname &optional t-args)
  "Select Avy above or below function name AVY-FNAME given T-ARGS.

- AVY-FNAME function name.
- T-ARGS list of options which can include ‘--above’, ‘--below’

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (let ((t-args (if (not t-args)
                    (transient-args transient-current-command)
                  t-args)))
    (cond
     ((and (member "--above" t-args)
           (member "--below" t-args))
      ;;(message "all")
      (call-interactively (intern avy-fname)))

     ((member "--above" t-args)
      ;;(message "above")
      (call-interactively (intern (concat avy-fname "-above"))))

     ((member "--below" t-args)
      ;;(message "below")
      (call-interactively (intern (concat avy-fname "-below"))))

     (t
      ;;(message "all")
      (call-interactively (intern avy-fname))))))

(defun casual-avy-avy-goto-line (&optional t-args)
  "Jump to a line start in current buffer using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-line' (default)
• `avy-goto-line-above' (--above)
• `avy-goto-line-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-line" t-args))

(defun casual-avy-avy-goto-word-1 (&optional t-args)
  "Jump to the currently visible char at a word start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-word-1' (default)
• `avy-goto-word-1-above' (--above)
• `avy-goto-word-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-word-1" t-args))

(defun casual-avy-avy-goto-symbol-1 (&optional t-args)
  "Jump to the currently visible char at a symbol start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-symbol-1' (default)
• `avy-goto-symbol-1-above' (--above)
• `avy-goto-symbol-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-symbol-1" t-args))

(defun casual-avy-avy-goto-whitespace-end (&optional t-args)
  "Jump to the end of a whitespace sequence using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-whitespace-end' (default)
• `avy-goto-whitespace-end-above' (--above)
• `avy-goto-whitespace-end-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-whitespace-end" t-args))

(defun casual-avy-avy-goto-char-2 (&optional t-args)
  "Jump to the currently visible char1 followed by char2 using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-char-2' (default)
• `avy-goto-char-2-above' (--above)
• `avy-goto-char-2-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated as if neither were specified."
  (interactive)
  (casual-avy-select-above-below "avy-goto-char-2" t-args))

(defun casual-avy-about-avy ()
  "Casual Avy is a Transient menu for Avy.

Learn more about using Casual Avy at our discussion group on GitHub.
Any questions or comments about it should be made there.
URL `https://github.com/kickingvegas/casual-avy/discussions'

If you find a bug or have an enhancement request, please file an issue.
Our best effort will be made to answer it.
URL `https://github.com/kickingvegas/casual-avy/issues'

If you enjoy using Casual Avy, consider making a modest financial
contribution to help support its development and maintenance.
URL `https://www.buymeacoffee.com/kickingvegas'

Casual Avy was conceived and crafted by Charles Choi in
San Francisco, California.

Thank you for using Casual Avy.

Always choose love."
  (ignore))

(defun casual-avy-about ()
  "About information for Casual Avy."
  (interactive)
  (describe-function #'casual-avy-about-avy))

(defun casual-avy-scope-label (template)
  "Generate formatted Avy scope label with TEMPLATE string."
  (format template (casual-avy-unicode-get :scope)))

;;;###autoload (autoload 'casual-avy-tmenu "casual-avy" nil t)
(transient-define-prefix casual-avy-tmenu ()
  "Casual Avy Transient menu."
  ["Scope (applies to ⬍)"
   :description (lambda () (casual-avy-scope-label "Scope (applies to (%s))"))
   :class transient-row
   ("a" "Above" "--above")
   ("b" "Below" "--below")]
  [["Goto Thing"
    ("c" "Character" avy-goto-char-timer :transient nil)
    ("2" "2 Characters ⬍" casual-avy-avy-goto-char-2
     :description (lambda () (casual-avy-scope-label "2 Characters (%s)"))
     :transient nil)
    ("w" "Word ⬍" casual-avy-avy-goto-word-1
     :description (lambda () (casual-avy-scope-label "Word (%s)"))
     :transient nil)
    ("s" "Symbol ⬍" casual-avy-avy-goto-symbol-1
     :description (lambda () (casual-avy-scope-label "Symbol (%s)"))
     :transient nil)
    ("W" "Whitespace end ⬍" casual-avy-avy-goto-whitespace-end
     :description (lambda () (casual-avy-scope-label "Whitespace end (%s)"))
     :transient nil)
    ("p" "Pop mark" avy-pop-mark :transient nil)]

   ["Goto Line"
    :pad-keys t
    ("l" "Line ⬍" casual-avy-avy-goto-line
     :description (lambda () (casual-avy-scope-label "Line (%s)"))
     :transient nil)
    ("e" "End of line" avy-goto-end-of-line :transient nil)
    ("o" "Org heading" avy-org-goto-heading-timer
     :if casual-avy-org-mode-p
     :transient nil)
    ("n" "Line number" goto-line
     :if casual-lib-display-line-numbers-mode-p
     :transient nil)]

   ["Edit Other Line"
    ("C" "Copy" avy-kill-ring-save-whole-line :transient nil)
    ("k" "Kill" avy-kill-whole-line
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("m" "Move to above current line" avy-move-line
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("d" "Duplicate to above current line" avy-copy-line
     :if casual-lib-buffer-writeable-p
     :transient nil)]]

  [["Edit Other Region (choose two lines)"
    ("r" "Copy" avy-kill-ring-save-region :transient nil)
    ("K" "Kill" avy-kill-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("M" "Move to above current line" avy-move-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("D" "Duplicate to above current line" avy-copy-region
     :if casual-lib-buffer-writeable-p
     :transient nil)
    ("t" "Transpose lines in active region" avy-transpose-lines-in-region
     :if casual-lib-buffer-writeable-and-region-active-p
     :transient nil)]

   ["Index"
    ("g" "Org Goto…" org-goto :if casual-avy-org-mode-p)
    ("i" "Index…" imenu :if casual-avy-imenu-support-p)]

   ["Occur/Grep/Error"
    ("M-p" "Previous" previous-error
     :description (lambda () "%s" (format (casual-avy-unicode-get :previous)))
     :transient t)
    ("M-n" "Next" next-error
     :description (lambda () "%s" (format (casual-avy-unicode-get :next)))
     :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("," "Settings›" casual-avy-settings-tmenu :transient nil)])

(transient-define-prefix casual-avy-settings-tmenu ()
  ["Customize"
   (casual-lib-customize-unicode)
   (casual-lib-customize-hide-navigation)
   ("m" "Customize Imenu Modes" casual-avy--customize-casual-avy-imenu-modes)
   ("A" "Customize Avy Group" casual-avy--customize-avy-group)]

  [:class transient-row
          (casual-lib-quit-one)
          ("a" "About" casual-avy-about :transient nil)
          ("v" "Version" casual-avy-version :transient nil)
          (casual-lib-quit-all)])

(provide 'casual-avy)
;;; casual-avy.el ends here
