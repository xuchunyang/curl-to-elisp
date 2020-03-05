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

## Supported curl options

The following options are supported, other options are ignored.

| Option                       | Note                           |
|------------------------------|--------------------------------|
| `-H, --header <header>`      |                                |
| `-I, --head`                 |                                |
| `-X, --request <command>`    |                                |
| `-d, --data <data>`          | does not interpret `@` as file |
| `--data-ascii <data>`        |                                |
| `--data-binary <data>`       |                                |
| `--data-raw <data>`          |                                |
| `-A, --user-agent <name>`    |                                |
| `-e, --referer <URL>`        |                                |
| `--url <url>`                |                                |
| `-u, --user <user:password>` | does not prompt for a password |

## Dependencies

- Emacs 25.1
