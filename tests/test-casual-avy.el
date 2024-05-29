;;; test-casual-avy.el --- Casual Avy Tests          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: tools

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

;;

;;; Code:
(require 'ert)
(require 'casual-avy-test-utils)
(require 'casual-avy)

;; TODO: Need to figure out how to import Avy when running this test from the
;; command line.

;; !!!: This test must be run interactively via `ert'.
(ert-deftest test-casual-avy-tmenu-bindings ()
  (casualt-setup)

  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "c" #'avy-goto-char-timer) test-vectors)
    (push (casualt-suffix-test-vector "2" #'avy-goto-char-2) test-vectors)
    (push (casualt-suffix-test-vector "a2" #'avy-goto-char-2-above) test-vectors)
    (push (casualt-suffix-test-vector "b2" #'avy-goto-char-2-below) test-vectors)
    (push (casualt-suffix-test-vector "w" #'avy-goto-word-1) test-vectors)
    (push (casualt-suffix-test-vector "aw" #'avy-goto-word-1-above) test-vectors)
    (push (casualt-suffix-test-vector "bw" #'avy-goto-word-1-below) test-vectors)
    (push (casualt-suffix-test-vector "s" #'avy-goto-symbol-1) test-vectors)
    (push (casualt-suffix-test-vector "as" #'avy-goto-symbol-1-above) test-vectors)
    (push (casualt-suffix-test-vector "bs" #'avy-goto-symbol-1-below) test-vectors)
    (push (casualt-suffix-test-vector "W" #'avy-goto-whitespace-end) test-vectors)
    (push (casualt-suffix-test-vector "aW" #'avy-goto-whitespace-end-above) test-vectors)
    (push (casualt-suffix-test-vector "bW" #'avy-goto-whitespace-end-below) test-vectors)
    (push (casualt-suffix-test-vector "p" #'avy-pop-mark) test-vectors)

    (push (casualt-suffix-test-vector "l" #'avy-goto-line) test-vectors)
    (push (casualt-suffix-test-vector "al" #'avy-goto-line-above) test-vectors)
    (push (casualt-suffix-test-vector "bl" #'avy-goto-line-below) test-vectors)
    (push (casualt-suffix-test-vector "e" #'avy-goto-end-of-line) test-vectors)

    ;;(push (casualt-suffix-test-vector "o" #'avy-org-goto-heading-timer) test-vectors)
    (push (casualt-suffix-test-vector "n1" #'goto-line) test-vectors)

    (push (casualt-suffix-test-vector "C" #'avy-kill-ring-save-whole-line) test-vectors)
    (push (casualt-suffix-test-vector "k" #'avy-kill-whole-line) test-vectors)
    (push (casualt-suffix-test-vector "m" #'avy-move-line) test-vectors)
    (push (casualt-suffix-test-vector "d" #'avy-copy-line) test-vectors)

    (push (casualt-suffix-test-vector "r" #'avy-kill-ring-save-region) test-vectors)
    (push (casualt-suffix-test-vector "K" #'avy-kill-region) test-vectors)
    (push (casualt-suffix-test-vector "M" #'avy-move-region) test-vectors)
    (push (casualt-suffix-test-vector "D" #'avy-copy-region) test-vectors)

    (push (casualt-suffix-test-vector "," #'casual-avy-settings-tmenu) test-vectors)

    ;;(push (casualt-suffix-test-vector "t" #'avy-transpose-lines-in-region) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-avy-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-avy-settings-tmenu-bindings ()
  (casualt-setup)

  (let ((test-vectors (list)))
    (push (casualt-suffix-test-vector "u" #'casual-avy--customize-casual-avy-use-unicode-symbols) test-vectors)
    (push (casualt-suffix-test-vector "A" #'casual-avy--customize-avy-group) test-vectors)
    (push (casualt-suffix-test-vector "a" #'casual-avy-about) test-vectors)
    (push (casualt-suffix-test-vector "v" #'casual-avy-version) test-vectors)

    (casualt-suffix-testbench-runner test-vectors
                                     #'casual-avy-settings-tmenu
                                     '(lambda () (random 5000))))
  (casualt-breakdown t))

(ert-deftest test-casual-avy-unicode-db ()
  (let* ((item (eval (alist-get :scope casual-avy-unicode-db))))
    (should (string-equal "#" (nth 1 item)))
    (should (string-equal "⬍" (nth 0 item)))))

(ert-deftest test-casual-avy-unicode-get ()
  (let ((casual-avy-use-unicode-symbols t))
    (should (string-equal "⬍" (casual-avy-unicode-db-get :scope))))

  (let ((casual-avy-use-unicode-symbols nil))
    (should (string-equal "#" (casual-avy-unicode-db-get :scope)))))

(ert-deftest test-casual-avy-display-line-numbers-mode-p ()
  (let ((display-line-numbers nil))
    (should (not (casual-avy-display-line-numbers-mode-p))))

  (let ((display-line-numbers 'relative))
    (should (casual-avy-display-line-numbers-mode-p))))

(provide 'test-casual-avy)
;;; test-casual-avy.el ends here
