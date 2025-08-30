;;; test-casual-avy.el --- Casual Avy Tests          -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Choi

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

(ert-deftest test-casual-avy-tmenu ()
  (let ((tmpfile "casual-avy-tmenu.txt"))
    (casualt-avy-setup tmpfile)
    (cl-letf ((casualt-mock #'avy-goto-char-timer)
              (casualt-mock #'avy-goto-char-2)
              (casualt-mock #'avy-goto-char-2-above)
              (casualt-mock #'avy-goto-char-2-below)
              (casualt-mock #'avy-goto-word-1)
              (casualt-mock #'avy-goto-word-1-above)
              (casualt-mock #'avy-goto-word-1-below)
              (casualt-mock #'avy-goto-symbol-1)
              (casualt-mock #'avy-goto-symbol-1-above)
              (casualt-mock #'avy-goto-symbol-1-below)
              (casualt-mock #'avy-goto-whitespace-end)
              (casualt-mock #'avy-goto-whitespace-end-above)
              (casualt-mock #'avy-goto-whitespace-end-below)
              (casualt-mock #'avy-pop-mark)

              (casualt-mock #'avy-goto-line)
              (casualt-mock #'avy-goto-line-above)
              (casualt-mock #'avy-goto-line-below)
              (casualt-mock #'avy-goto-end-of-line)
              (casualt-mock #'goto-line)

              (casualt-mock #'avy-kill-ring-save-whole-line)
              (casualt-mock #'avy-kill-whole-line)
              (casualt-mock #'avy-move-line)
              (casualt-mock #'avy-copy-line)
              (casualt-mock #'avy-kill-ring-save-region)
              (casualt-mock #'avy-kill-region)
              (casualt-mock #'avy-move-region)
              (casualt-mock #'avy-copy-region)
              (casualt-mock #'transient-quit-all))

      (let ((test-vectors
             '((:binding "ca" :command avy-goto-char-timer)
               (:binding "2ab" :command avy-goto-char-2)
               (:binding "a2ab" :command avy-goto-char-2-above)
               (:binding "b2ab" :command avy-goto-char-2-below)
               (:binding "wa" :command avy-goto-word-1)
               (:binding "awa" :command avy-goto-word-1-above)
               (:binding "bwa" :command avy-goto-word-1-below)
               (:binding "sa" :command avy-goto-symbol-1)
               (:binding "asa" :command avy-goto-symbol-1-above)
               (:binding "bsa" :command avy-goto-symbol-1-below)
               (:binding "W" :command avy-goto-whitespace-end)
               (:binding "aW" :command avy-goto-whitespace-end-above)
               (:binding "bW" :command avy-goto-whitespace-end-below)
               (:binding "p" :command avy-pop-mark)

               (:binding "l" :command avy-goto-line)
               (:binding "al" :command avy-goto-line-above)
               (:binding "bl" :command avy-goto-line-below)
               (:binding "e" :command avy-goto-end-of-line)
               (:binding "n1" :command goto-line)

               (:binding "C" :command avy-kill-ring-save-whole-line)
               (:binding "k" :command avy-kill-whole-line)
               (:binding "m" :command avy-move-line)
               (:binding "d" :command avy-copy-line)
               (:binding "r" :command avy-kill-ring-save-region)
               (:binding "K" :command avy-kill-region)
               (:binding "M" :command avy-move-region)
               (:binding "D" :command avy-copy-region)
               (:binding "C-Q" :command transient-quit-all))))

        (casualt-suffix-testcase-runner test-vectors
                                        #'casual-avy-tmenu
                                        '(lambda () (random 5000)))))
    (casualt-avy-breakdown tmpfile)))

(ert-deftest test-casual-avy-settings-tmenu ()
  (casualt-avy-setup)
  (cl-letf ((casualt-mock #'casual-lib-customize-casual-lib-use-unicode)
            (casualt-mock #'casual-avy--customize-avy-group)
            (casualt-mock #'casual-avy--customize-casual-avy-imenu-modes)
            (casualt-mock #'casual-avy-about)
            (casualt-mock #'casual-avy-version))

    (let ((test-vectors
           '((:binding "A" :command casual-avy--customize-avy-group)
             (:binding "u" :command casual-lib-customize-casual-lib-use-unicode)
             (:binding "m" :command casual-avy--customize-casual-avy-imenu-modes)
             (:binding "a" :command casual-avy-about)
             (:binding "v" :command casual-avy-version))))

      (casualt-suffix-testcase-runner test-vectors
                                      #'casual-avy-settings-tmenu
                                      '(lambda () (random 5000)))))
  (casualt-avy-breakdown))


(ert-deftest test-casual-avy-unicode-db ()
  (let* ((item (eval (alist-get :scope casual-avy-unicode-db))))
    (should (string-equal "#" (nth 1 item)))
    (should (string-equal "⬍" (nth 0 item)))))

(ert-deftest test-casual-avy-unicode-get ()
  (let ((casual-lib-use-unicode t))
    (should (string-equal "⬍" (casual-avy-unicode-get :scope)))
    (should (string-equal "↑" (casual-avy-unicode-get :previous)))
    (should (string-equal "↓" (casual-avy-unicode-get :next))))

  (let ((casual-lib-use-unicode nil))
    (should (string-equal "#" (casual-avy-unicode-get :scope)))
    (should (string-equal "Previous" (casual-avy-unicode-get :previous)))
    (should (string-equal "Next" (casual-avy-unicode-get :next)))))

(provide 'test-casual-avy)
;;; test-casual-avy.el ends here
