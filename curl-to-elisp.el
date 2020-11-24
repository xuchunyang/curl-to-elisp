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

;; Convert cURL command to Emacs Lisp code.
;;
;; To get started, try
;;
;;   M=x curl-to-elisp curl -I example.com

;;; Code:

(require 'esh-cmd)                      ; `eshell-parse-command'
(require 'subr-x)                       ; `string-trim'
(require 'cl-lib)
(require 'mml)                          ; `mml-compute-boundary'
(require 'mm-url)                       ; `mm-url-encode-multipart-form-data'
(require 'seq)                          ; `seq' pattern of `pcase'
(require 'json)                         ; `json-pretty-print-buffer'

(defgroup curl-to-elisp nil
  "Convert cURL command to equivalent Emacs Lisp code."
  :group 'lisp)

(defcustom curl-to-elisp-curlie-binary (executable-find "curlie")
  "Curlie executable used by curl-to-elisp."
  :group 'curl-to-elisp
  :type 'string)

(defun curl-to-elisp--tokenize-recur (parse-tree)
  "Tokenize PARSE-TREE recursively.
Subroutine of `curl-to-elisp--tokenize'."
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
              (mapcar #'substring-no-properties
                      (delete "\n" (eval (car arguments)))))))
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
        ((and s (guard (string-prefix-p "--" s)))
         (let ((name (substring s 2)))
           (push (cons name
                       (if (member name curl-to-elisp--bool-options)
                           t
                         (nth (cl-incf i) arguments)))
                 alist))
         (cl-incf i))
        ;; short option
        ;; -abc cval
        ;; -abccval
        ;; NOTE curl does not interpret =, thus -abc=cval is -a -b -c =cvalue
        ((and s (guard (string-prefix-p "-" s)))
         (let ((opts (substring s 1)))
           (catch 'short
             (cl-loop for j from 0
                      for o in (mapcar #'string (string-to-list opts))
                      do (if (member o curl-to-elisp--bool-options)
                             (push (cons o t) alist)
                           (progn
                             (if (= j (1- (length opts)))
                                 (push (cons o (nth (cl-incf i) arguments)) alist)
                               (push (cons o (substring opts (1+ j))) alist))
                             (throw 'short nil))))))
         (cl-incf i))
        (x
         (push (cons "_" x) alist)
         (cl-incf i))))
    (nreverse alist)))

(defun curl-to-elisp--parse-header (header)
  "Parse the HTTP HEADER and return (NAME . VALUE).
If the parse fails, return nil."
  (pcase (cl-position ?: header :test #'=)
    ('nil nil)
    (n (cons (capitalize (string-trim (substring header 0 n)))
             (string-trim (substring header (1+ n)))))))

(defun curl-to-elisp--parse-form (s)
  "Parse form S in NAME=VALUE and return (NAME . VALUE).
If the parse fails, return nil."
  (pcase (cl-position ?= s :test #'=)
    ('nil nil)
    (n (cons (substring s 0 n)
             (substring s (1+ n))))))

(defun curl-to-elisp--split-string (s ch)
  "Split S into two substrings by CH.
Return nil if S does not contain CH."
  (pcase (cl-position ch s :test #'=)
    ('nil nil)
    (n    (list (substring s 0 n)
                (substring s (1+ n))))))

(defun curl-to-elisp--extract (alist)
  "Extract request from ALIST."
  (let ((reversed (reverse alist))
        url method headers data form boundary silent)
    (setq url (or (assoc-default "url" alist)
                  (assoc-default "_" alist)))
    (and url
         (not (string-match-p "\\`https?://" url))
         (setq url (concat "http://" url)))

    (when (or (assoc-default "s" alist)
              (assoc-default "silent" alist))
      (setq silent t))

    (dolist (kv alist)
      (pcase kv
        (`(,(or "H" "header") . ,s)
         (pcase (curl-to-elisp--parse-header s)
           (`(,k . ,v)
            (push (cons k v) headers))))))

    (pcase (or (assoc "A" alist)
               (assoc "user-agent" alist))
      (`(,_ . ,s)
       (push (cons "User-Agent" s) headers)))

    (pcase (or (assoc "e" alist)
               (assoc "referer" alist))
      (`(,_ . ,s)
       (push (cons "Referer" s) headers)))

    (pcase (or (assoc "u" alist)
               (assoc "user" alist))
      (`(,_ . ,s)
       (push (cons "Authorization" (concat "Basic " (base64-encode-string s)))
             headers)))

    (setq headers (nreverse headers))

    (setq method (or (and (or (assoc-default "I" alist)
                              (assoc-default "head" alist))
                          "HEAD")
                     (assoc-default "require" reversed)
                     (assoc-default "X" reversed)))

    (dolist (kv alist)
      (pcase kv
        (`(,(or "d" "data" "data-ascii" "data-binary" "data-raw") . ,v)
         (setq data (if data
                        (concat data "&" v)
                      v)))
        (`("data-urlencode" . , v)
         (let ((s (pcase (curl-to-elisp--split-string v ?=)
                    ('nil (url-hexify-string v))
                    (`(,name ,content)
                     ;; curl urlencode CONTENT and leave NAME untouched
                     (concat name "=" (url-hexify-string content))))))
           (setq data (if data
                          (concat data "&" s)
                        s))))))

    (when data
      (unless (assoc-default "Content-Type" headers)
        (push (cons "Content-Type" "application/x-www-form-urlencoded")
              headers)))

    (dolist (kv alist)
      (pcase kv
        (`(,(or "F" "form" "form-string") . ,s)
         (pcase (curl-to-elisp--parse-form s)
           (`(,name . ,content)
            (push (cons name content) form))))))

    (when form
      ;; ~ $ curl -F name=bob -d msg=hi example.com
      ;; Warning: You can only select one HTTP request method! You asked for both POST
      ;; Warning: (-d, --data) and multipart formpost (-F, --form).
      (and data (user-error "You can't use -d and -F at the same time"))
      (setq boundary (mml-compute-boundary '()))
      (push (cons "Content-Type" (concat "multipart/form-data; boundary="
			                 boundary))
            headers)
      (setq data (mm-url-encode-multipart-form-data (nreverse form) boundary)))

    (unless method
      (when data
        (setq method "POST")))

    (list url method headers data silent)))

(defun curl-to-elisp--build (url method headers data silent)
  "Build a http request using URL, METHOD, HEADERS, DATA, return a sexp."
  (let (user-agent)
    ;; Emacs prefers `url-user-agent' to `url-request-extra-headers'
    (pcase (assoc "User-Agent" headers)
      ('nil nil)
      ((and pair `(,_ . ,s))
       (setq headers (delq pair headers))
       (setq user-agent s)))
    (let (bindings)
      (when user-agent
        (push `(url-user-agent ,user-agent) bindings))
      (when method
        (push `(url-request-method ,method) bindings))
      (when headers
        (push `(url-request-extra-headers ',headers) bindings))
      (when data
        (push `(url-request-data ,data) bindings))
      (when silent
        (push '(url-show-status nil) bindings))
      (setq bindings (nreverse bindings))
      (if bindings
          `(let ,bindings
             (url-retrieve-synchronously ,url))
        `(url-retrieve-synchronously ,url)))))

(defun curl-to-elisp--trim (command)
  "Remove leading $ or # in COMMAND."
  (replace-regexp-in-string
   (rx bos (* blank) (? (in "$#")) (* blank))
   ""
   command))


;;;###autoload
(defun curl-to-elisp-httpie-to-elisp (command &optional print)
  "Convert httpie/curlie COMMAND to Emacs Lisp expression.

Return the expression.

When called interactively or PRINT is non-nil, also pretty-print
the expression in echo area."
  (interactive (list (read-string "httpie command: ") t))
  (unless curl-to-elisp-curlie-binary
    (user-error "Can't find curlie executable.  Check `curl-to-elisp-curlie-binary'"))
  (let ((command (replace-regexp-in-string
                  "^\\(curlie\\|http\\) "
                  (format "%s --curl " curl-to-elisp-curlie-binary)
                  command)))
    (curl-to-elisp
     (with-temp-buffer
       (accept-process-output
        (start-process-shell-command
         "curl-to-elisp-httpie"
         (current-buffer)
         command))
       (redisplay)
       (buffer-string))
     print)))


;;;###autoload
(defun curl-to-elisp (command &optional print)
  "Convert cURL COMMAND to Emacs Lisp expression, return the expression.

When called interactively or PRINT is non-nil, also pretty-print
the expression in echo area."
  (interactive (list (read-string "cURL command: ") t))
  (let ((expr (apply #'curl-to-elisp--build
                     (curl-to-elisp--extract
                      (curl-to-elisp--parse
                       (curl-to-elisp--tokenize
                        (curl-to-elisp--trim
                         command)))))))
    (when print
      (pp expr))
    expr))

;;;###autoload
(defun curl-to-elisp-verb (command &optional insert)
  "Convert cURL COMMAND to verb request specification, return the specification.

When INSERT is non-nil, insert the result at point.

For verb request specification, see URL
`https://github.com/federicotdn/verb#writing-request-specifications'."
  (interactive (list (read-string "cURL command: ") t))
  (pcase-exhaustive (curl-to-elisp--extract
                     (curl-to-elisp--parse
                      (curl-to-elisp--tokenize
                       (curl-to-elisp--trim
                        command))))
    ((seq url method headers data _silent)
     (let ((s (format "%s %s" (downcase (or method "GET")) url)))
       (when headers
         (setq s (concat s "\n" (mapconcat
                                 (pcase-lambda (`(,key . ,val))
                                   (format "%s: %s" key val))
                                 headers
                                 "\n"))))
       (when data
         (when-let ((type (assoc-default "Content-Type" headers))
                    (jsonp (string-match-p (rx "application/json") type)))
           (with-temp-buffer
             (insert data)
             (json-pretty-print-buffer)
             (setq data (buffer-string))))
         (setq s (concat s "\n\n" data)))
       (when insert
         (save-excursion
           (insert s)))
       s))))

(provide 'curl-to-elisp)
;;; curl-to-elisp.el ends here
