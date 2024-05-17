;;; casual-avy.el --- Casual Avy                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/casual-avy
;; Keywords: tools
;; Version: 1.0.2
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

(defun ca-display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (symbol-value display-line-numbers))

(defun ca-org-mode-p ()
  "Predicate to test if `org-mode' is enabled."
  (derived-mode-p 'org-mode))

(defun ca-buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun ca-buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (ca-buffer-writeable-p) (region-active-p)))

(defun ca-select-above-below (avy-fname &optional t-args)
  "Select Avy above or below function name AVY-FNAME given T-ARGS.

- AVY-FNAME function name.
- T-ARGS list of options which can include ‘--above’, ‘--below’

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
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

(defun ca-avy-goto-line (&optional t-args)
  "Jump to a line start in current buffer using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-line' (default)
• `avy-goto-line-above' (--above)
• `avy-goto-line-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
  (interactive)
  (ca-select-above-below "avy-goto-line" t-args))

(defun ca-avy-goto-word-1 (&optional t-args)
  "Jump to the currently visible char at a word start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-word-1' (default)
• `avy-goto-word-1-above' (--above)
• `avy-goto-word-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
  (interactive)
  (ca-select-above-below "avy-goto-word-1" t-args))

(defun ca-avy-goto-symbol-1 (&optional t-args)
  "Jump to the currently visible char at a symbol start using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-symbol-1' (default)
• `avy-goto-symbol-1-above' (--above)
• `avy-goto-symbol-1-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
  (interactive)
  (ca-select-above-below "avy-goto-symbol-1" t-args))

(defun ca-avy-goto-whitespace-end (&optional t-args)
  "Jump to the end of a whitespace sequence using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-whitespace-end' (default)
• `avy-goto-whitespace-end-above' (--above)
• `avy-goto-whitespace-end-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
  (interactive)
  (ca-select-above-below "avy-goto-whitespace-end" t-args))

(defun ca-avy-goto-char-2 (&optional t-args)
  "Jump to the currently visible char1 followed by char2 using T-ARGS option.

- T-ARGS list of options which can include ‘--above’, ‘--below’

Given the value of T-ARGS, one of the following functions will be called:
• `avy-goto-char-2' (default)
• `avy-goto-char-2-above' (--above)
• `avy-goto-char-2-below' (--below)

If T-ARGS includes both ‘--above’ and ‘--below’ then it is
treated implictly as if neither were specified."
  (interactive)
  (ca-select-above-below "avy-goto-char-2" t-args))

(defun ca-about-avy ()
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

(defun ca-about ()
  "About information for Casual Avy."
  (interactive)
  (describe-function #'ca-about-avy))

;;;###autoload (autoload 'casual-avy-tmenu "casual-avy" nil t)
(transient-define-prefix casual-avy-tmenu ()
  "Casual Avy Transient menu."
  ["Option (applies to ⬍)"
   :class transient-row
   ("a" "Above" "--above")
   ("b" "Below" "--below")]
  [["Goto Thing"
    ("c" "Character" avy-goto-char-timer :transient nil)
    ("2" "2 Characters ⬍" ca-avy-goto-char-2 :transient nil)
    ("w" "Word ⬍" ca-avy-goto-word-1 :transient nil)
    ("s" "Symbol ⬍" ca-avy-goto-symbol-1 :transient nil)
    ("W" "Whitespace end ⬍" ca-avy-goto-whitespace-end :transient nil)
    ("p" "Pop mark" avy-pop-mark :transient nil)]

   ["Goto Line"
    :pad-keys t
    ("l" "Line ⬍" ca-avy-goto-line :transient nil)
    ("e" "End of line" avy-goto-end-of-line :transient nil)
    ("o" "Org heading" avy-org-goto-heading-timer
     :if ca-org-mode-p
     :transient nil)
    ("n" "Line number" goto-line
     :if ca-display-line-numbers-mode-p
     :transient nil)]

   ["Edit Other Line"
    ("C" "Copy" avy-kill-ring-save-whole-line :transient nil)
    ("k" "Kill" avy-kill-whole-line
     :if ca-buffer-writeable-p
     :transient nil)
    ("m" "Move to above current line" avy-move-line
     :if ca-buffer-writeable-p
     :transient nil)
    ("d" "Duplicate to above current line" avy-copy-line
     :if ca-buffer-writeable-p
     :transient nil)]]

  ["Edit Other Region (choose two lines)"
    ("r" "Copy" avy-kill-ring-save-region :transient nil)
    ("K" "Kill" avy-kill-region
     :if ca-buffer-writeable-p
     :transient nil)
    ("M" "Move to above current line" avy-move-region
     :if ca-buffer-writeable-p
     :transient nil)
    ("D" "Duplicate to above current line" avy-copy-region
     :if ca-buffer-writeable-p
     :transient nil)
    ("t" "Transpose lines in active region" avy-transpose-lines-in-region
     :if ca-buffer-writeable-and-region-active-p
     :transient nil)]

  [:class transient-row
          ("v" "Version" casual-avy-version :transient nil)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'casual-avy)
;;; casual-avy.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ca-" . "casual-avy-"))
;; End:
