;;; curl-to-elisp-tests.el --- Tests                 -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

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

;; Tests for curl-to-elisp.el

;;; Code:

(require 'ert)
(require 'curl-to-elisp)

(ert-deftest curl-to-elisp--parse ()
  "Test the curl command parser."
  (should (equal (curl-to-elisp--parse "curl -I example.com")
                 '("-I" "example.com")))
  ;; absolute path
  (should (equal (curl-to-elisp--parse "/usr/bin/curl -I example.com")
                 '("-I" "example.com")))
  ;; pipeline
  (should (equal (curl-to-elisp--parse "echo hello | curl -d @- localhost:7777")
                 '("-d" "@-" "localhost:7777")))
  ;; quote
  (should (equal (curl-to-elisp--parse "curl -d 'hello world' -d \"bye world\" localhost:7777")
                 '("-d" "hello world" "-d" "bye world" "localhost:7777")))
  ;; escape
  (should (equal (curl-to-elisp--parse "curl -d hello\\ world localhost:7777")
                 '("-d" "hello world" "localhost:7777")))
  ;; multiple lines
  (should (equal (curl-to-elisp--parse
                  "curl -v \\
-d hello \\
  example.com")
                 '("-v" "-d" "hello" "example.com"))))

(provide 'curl-to-elisp-tests)
;;; curl-to-elisp-tests.el ends here
