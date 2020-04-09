# Convert cURL command to Emacs Lisp code
[![Melpa](https://melpa.org/packages/curl-to-elisp-badge.svg)](https://melpa.org/#/curl-to-elisp)

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

| Option                        | Note                           |
|-------------------------------|--------------------------------|
| `-H, --header <header>`       |                                |
| `-I, --head`                  |                                |
| `-X, --request <command>`     |                                |
| `-d, --data <data>`           | does not interpret `@` as file |
| `--data-ascii <data>`         |                                |
| `--data-binary <data>`        |                                |
| `--data-raw <data>`           |                                |
| `--data-urlencode <data>`     |                                |
| `-F, --form <name=content>`   | does not interpret `@` as file |
| `--form-string <name=string>` |                                |
| `-A, --user-agent <name>`     |                                |
| `-e, --referer <URL>`         |                                |
| `--url <url>`                 |                                |
| `-u, --user <user:password>`  | does not prompt for a password |
| `-s, --silent`                |                                |

### `-L`, `--location`

Regarding the location header, curl doesn't redirect unless you use `-L`,
`--location`, however, `url-retrieve[-synchronously]` DOES automatically
redirect, to inhibit redirection, let-binding `url-max-redirections`, e.g.,

``` emacs-lisp
(let ((url-max-redirections 0))
  (display-buffer
   (url-retrieve-synchronously "http://emacs-china.org")))
```

## httpie to elisp

`curl-to-elisp-httpie-to-elisp` uses [curlie](https://curlie.io/) to
transform an [httpie](https://httpie.org/)/curlie command to elisp.

You need to have `curlie` binary in your path (or customize
`curl-to-elisp-curlie-binary`).

## Dependencies

- Emacs 25.1
