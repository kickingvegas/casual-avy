;;; casual-avy.el --- Casual Avy                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-avy
;; Keywords: tools
;; Version: 1.0.5
;; Package-Requires: ((emacs "29.1") (avy "0.5.0"))

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
;; (require 'casual-avy)
;; (keymap-global-set "M-g" #'casual-avy-tmenu)

;;; Code:

(require 'transient)
(require 'avy)
(require 'display-line-numbers)
(require 'org)
(require 'casual-avy-version)

(defcustom casual-avy-use-unicode-symbols nil
  "If non-nil then use Unicode symbols whenever appropriate for labels."
  :type 'boolean
  :group 'avy)

(defconst casual-avy-unicode-db
  '((:scope . '("⬍" "#")))
  "Unicode symbol DB to use for Avy Transient menus.")

(defun casual-avy--customize-casual-avy-use-unicode-symbols ()
  "Customize `casual-avy-use-unicode-symbols'.

Customize Casual Avy to use Unicode symbols in place of strings
when appropriate."
  (interactive)
  (customize-variable 'casual-avy-use-unicode-symbols))

(defun casual-avy--customize-avy-group ()
  "Call the Avy customization group."
  (interactive)
  (customize-group "avy"))

(defun casual-avy-unicode-db-get (key &optional db)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.
- DB alist containing Unicode symbols used by Info Transient menus.

If DB is nil, then `casual-avy-unicode-db' is used by default.

If the value of customizable variable `casual-avy-use-unicode-symbols'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (let* ((db (or db casual-avy-unicode-db))
         (unicode casual-avy-use-unicode-symbols)
         (item (alist-get key db)))
    (if unicode
        (nth 0 (eval item))
      (nth 1 (eval item)))))

(defun casual-avy-display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (if display-line-numbers t nil))

(defun casual-avy-org-mode-p ()
  "Predicate to test if `org-mode' is enabled."
  (derived-mode-p 'org-mode))

(defun casual-avy-buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun casual-avy-buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (casual-avy-buffer-writeable-p) (region-active-p)))

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
      (message "all")
      (call-interactively (intern avy-fname)))

     ((member "--above" t-args)
      (message "above")
      (call-interactively (intern (concat avy-fname "-above"))))

     ((member "--below" t-args)
      (message "below")
      (call-interactively (intern (concat avy-fname "-below"))))

     (t
      (message "all")
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

;;;###autoload (autoload 'casual-avy-tmenu "casual-avy" nil t)
(transient-define-prefix casual-avy-tmenu ()
  "Casual Avy Transient menu."
  ["Scope (applies to ⬍)"
   :description (lambda ()
                  (format "Scope (applies to (%s))"
                          (casual-avy-unicode-db-get :scope)))
   :class transient-row
   ("a" "Above" "--above")
   ("b" "Below" "--below")]
  [["Goto Thing"
    ("c" "Character" avy-goto-char-timer :transient nil)
    ("2" "2 Characters ⬍" casual-avy-avy-goto-char-2
     :description (lambda ()
                    (format "2 Characters (%s)"
                            (casual-avy-unicode-db-get :scope)))
     :transient nil)
    ("w" "Word ⬍" casual-avy-avy-goto-word-1
     :description (lambda ()
                    (format "Word (%s)"
                            (casual-avy-unicode-db-get :scope)))
     :transient nil)
    ("s" "Symbol ⬍" casual-avy-avy-goto-symbol-1
     :description (lambda ()
                    (format "Symbol (%s)"
                            (casual-avy-unicode-db-get :scope)))
     :transient nil)
    ("W" "Whitespace end ⬍" casual-avy-avy-goto-whitespace-end
     :description (lambda ()
                    (format "Whitespace end (%s)"
                            (casual-avy-unicode-db-get :scope)))
     :transient nil)
    ("p" "Pop mark" avy-pop-mark :transient nil)]

   ["Goto Line"
    :pad-keys t
    ("l" "Line ⬍" casual-avy-avy-goto-line
     :description (lambda ()
                    (format "Line (%s)"
                            (casual-avy-unicode-db-get :scope)))
     :transient nil)
    ("e" "End of line" avy-goto-end-of-line :transient nil)
    ("o" "Org heading" avy-org-goto-heading-timer
     :if casual-avy-org-mode-p
     :transient nil)
    ("n" "Line number" goto-line
     :if casual-avy-display-line-numbers-mode-p
     :transient nil)]

   ["Edit Other Line"
    ("C" "Copy" avy-kill-ring-save-whole-line :transient nil)
    ("k" "Kill" avy-kill-whole-line
     :if casual-avy-buffer-writeable-p
     :transient nil)
    ("m" "Move to above current line" avy-move-line
     :if casual-avy-buffer-writeable-p
     :transient nil)
    ("d" "Duplicate to above current line" avy-copy-line
     :if casual-avy-buffer-writeable-p
     :transient nil)]]

  ["Edit Other Region (choose two lines)"
    ("r" "Copy" avy-kill-ring-save-region :transient nil)
    ("K" "Kill" avy-kill-region
     :if casual-avy-buffer-writeable-p
     :transient nil)
    ("M" "Move to above current line" avy-move-region
     :if casual-avy-buffer-writeable-p
     :transient nil)
    ("D" "Duplicate to above current line" avy-copy-region
     :if casual-avy-buffer-writeable-p
     :transient nil)
    ("t" "Transpose lines in active region" avy-transpose-lines-in-region
     :if casual-avy-buffer-writeable-and-region-active-p
     :transient nil)]

  [:class transient-row
          ("," "Settings›" casual-avy-settings-tmenu :transient nil)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix casual-avy-settings-tmenu ()
  ["Customize"
   ("u" "Use Unicode Symbols"
    casual-avy--customize-casual-avy-use-unicode-symbols)
   ("A" "Customize Avy Group" casual-avy--customize-avy-group)]

  [:class transient-row
          ("a" "About" casual-avy-about :transient nil)
          ("v" "Version" casual-avy-version :transient nil)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'casual-avy)
;;; casual-avy.el ends here
