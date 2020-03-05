;;; curl-to-elisp.el --- Convert cURL command to Emacs Lisp code  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/curl-to-elisp
;; Package-Requires: ((emacs "25.1"))
;; Keywords: lisp
;; Version: 0

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

;; XXX add doc here

;;; Code:

(require 'esh-cmd)                      ; `eshell-parse-command'

(defun curl-to-elisp--parse-recur (parse-tree)
  (pcase parse-tree
    (`(eshell-named-command ,command . ,arguments)
     (when (or (string= command "curl")
               ;; such as /usr/bin/curl
               (string-suffix-p "/curl" command))
       (throw 'curl
              ;; XXX Care to file a bug report?
              ;; Eshell does not correctly handle spaces in a multiline command, e.g,
              ;;
              ;; $ curl \
              ;;        example.com
              ;;
              ;; Eshell parses it into ("curl" "\n" "example.com")
              ;;
              ;; To workaround this issue, I remove all "\n"
              (delete "\n" (eval (car arguments))))))
    ((pred listp)
     (mapc #'curl-to-elisp--parse-recur parse-tree))))

(defun curl-to-elisp--parse (command)
  "Parse a curl COMMAND, return a list of arguments."
  (catch 'curl
    (curl-to-elisp--parse-recur
     (eshell-parse-command command nil t))
    (user-error "Not a curl command: %S" command)))

(provide 'curl-to-elisp)
;;; curl-to-elisp.el ends here
