# GigaChat LLM provider for Emacs

This is a [Emacs LLM](https://github.com/ahyatt/llm) provider that supports [GigaChat API](https://developers.sber.ru/portal/products/gigachat-api).

The simplest way to use it is [Ellama](https://github.com/s-kostyaev/ellama):

```elisp
;; https://www.sberbank.com/ru/certificates
(with-eval-after-load 'gnutls
  (add-to-list 'gnutls-trustfiles (expand-file-name (convert-standard-filename "certs/russiantrustedca.pem") user-emacs-directory)))

(use-package ellama
  :init
  (require 'llm-gigachat)
  (setopt ellama-provider (make-llm-gigachat :key gigachat-auth-token :scope gigachat-scope)))
```

Development status: alpha
