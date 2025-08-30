;;; casual-avy-test-utils.el --- Casual Test Utils       -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'kmacro)
(require 'casual-lib)
(require 'casual-lib-test-utils)

(defun casualt-avy-setup (&optional filename)
  "Casual avy test setup function."

  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (with-temp-file temp-filename
        (insert ";; hey there
;; wanted to say this"))
      (find-file temp-filename)
      (display-line-numbers-mode t)))

  )

(defun casualt-avy-breakdown (&optional filename)
  "Casual menu test breakdown function with FILENAME.

- FILENAME filename that is stored in /tmp"

  (when filename
    (let ((temp-filename (concat "/tmp/" filename)))
      (kill-buffer)
      (delete-file temp-filename))))


(provide 'casual-avy-test-utils)
;;; casual-avy-test-utils.el ends here
