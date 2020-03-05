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

(defun curl-to-elisp--tokenize-recur (parse-tree)
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
     (mapc #'curl-to-elisp--tokenize-recur parse-tree))))

(defun curl-to-elisp--tokenize (command)
  "Return a list of arguments in curl COMMAND."
  (catch 'curl
    (curl-to-elisp--tokenize-recur
     (eshell-parse-command command nil t))
    (user-error "Not a curl command: %S" command)))

(defconst curl-to-elisp--bool-options
  '("#" "progress-bar" "-" "next" "0" "http1.0" "http1.1" "http2"
    "no-npn" "no-alpn" "1" "tlsv1" "2" "sslv2" "3" "sslv3" "4" "ipv4" "6" "ipv6"
    "a" "append" "anyauth" "B" "use-ascii" "basic" "compressed" "create-dirs"
    "crlf" "digest" "disable-eprt" "disable-epsv" "environment" "cert-status"
    "false-start" "f" "fail" "ftp-create-dirs" "ftp-pasv" "ftp-skip-pasv-ip"
    "ftp-pret" "ftp-ssl-ccc" "ftp-ssl-control" "g" "globoff" "G" "get"
    "ignore-content-length" "i" "include" "I" "head" "j" "junk-session-cookies"
    "J" "remote-header-name" "k" "insecure" "l" "list-only" "L" "location"
    "location-trusted" "metalink" "n" "netrc" "N" "no-buffer" "netrc-file"
    "netrc-optional" "negotiate" "no-keepalive" "no-sessionid" "ntlm" "O"
    "remote-name" "oauth2-bearer" "p" "proxy-tunnel" "path-as-is" "post301" "post302"
    "post303" "proxy-anyauth" "proxy-basic" "proxy-digest" "proxy-negotiate"
    "proxy-ntlm" "q" "raw" "remote-name-all" "s" "silent" "sasl-ir" "S" "show-error"
    "ssl" "ssl-reqd" "ssl-allow-beast" "ssl-no-revoke" "socks5-gssapi-nec" "tcp-nodelay"
    "tlsv1.0" "tlsv1.1" "tlsv1.2" "tr-encoding" "trace-time" "v" "verbose" "xattr"
    "h" "help" "M" "manual" "V" "version")
  "List of curl flags that are boolean options.

Adapted from URL
`https://github.com/mholt/curl-to-go/blob/a8cb21a8885c7edc6c610d6e7db63f2891fe38d6/resources/js/curl-to-go.js#L18'.")

;; curl -vH   'User-Agent: not curl' example.com
;; curl -v -H 'User-Agent: not curl' example.com
;; curl -vH'User-Agent: not curl' example.com
;; curl -v --header 'User-Agent: not curl' example.com
(defun curl-to-elisp--parse (arguments)
  "Parse ARGUMENTS, return a list of (OPTION . VALUE)."
  (let ((i 0)
        alist)
    (while (< i (length arguments))
      (pcase (nth i arguments)
        ;; -- signify the end of the options
        ("--"
         (cl-loop for x in (nthcdr (1+ i) arguments)
                  do
                  (push (cons "_" x) alist)
                  (cl-incf i)))
        ;; long option
        ;; --header 'Accept: application/json'
        ;; NOTE curl does not interpret =,
        ;; thus --header='Accept: application/json' is wrong
        ((rx bos "--" (let name (1+ not-newline)))
         (push (cons name
                     (if (member name curl-to-elisp--bool-options)
                         t
                       (nth (cl-incf i) arguments)))
               alist)
         (cl-incf i))
        ;; short option
        ;; -abc cval
        ;; -abccval
        ;; NOTE curl does not interpret =, thus -abc=cval is -a -b -c =cvalue
        ((rx bos "-" (let opts (1+ not-newline)))
         (catch 'short
           (cl-loop for j from 0
                    for o in (mapcar #'string (string-to-list opts))
                    do (if (member o curl-to-elisp--bool-options)
                           (push (cons o t) alist)
                         (progn
                           (if (= j (1- (length opts)))
                               (push (cons o (nth (cl-incf i) arguments)) alist)
                             (push (cons o (substring opts (1+ j))) alist))
                           (throw 'short nil)))))
         (cl-incf i))
        (x
         (push (cons "_" x) alist)
         (cl-incf i))))
    (nreverse alist)))


(provide 'curl-to-elisp)
;;; curl-to-elisp.el ends here
