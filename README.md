# Convert cURL command to Emacs Lisp code

_Inspired by [curl-to-Go: Convert curl commands to Go code](https://mholt.github.io/curl-to-go/)._

``` emacs-lisp
(curl-to-elisp "curl example.com")
;; =>
(url-retrieve-synchronously "http://example.com")

(curl-to-elisp "curl -d 'hello world' example.com")
;; =>
(let ((url-request-method "POST")
      (url-request-extra-headers
       '(("Content-Type" . "application/x-www-form-urlencoded")))
      (url-request-data "hello world"))
  (url-retrieve-synchronously "http://example.com"))
```

To get started, try `M-x curl-to-elisp curl -I example.com`, the Emacs Lisp code will be pretty-printed in echo area.

## Dependencies

- Emacs 25.1
