;;; llm-gigachat.el --- llm module for integrating with GigaChat -*- lexical-binding: t -*-

;; Author: Max Arnold <arnold.maxim@gmail.com>
;; Homepage: https://github.com/max-arnold/llm-gigachat
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file implements the llm functionality defined in llm.el, for GigaChat
;; API.

(require 'cl-lib)
(require 'llm)
(require 'llm-request)
(require 'json)

(defgroup llm-gigachat nil
  "LLM implementation for Sber GigaChat."
  :group 'llm)

(defcustom llm-gigachat-default-chat-model "GigaChat:latest"
  "The default model to ask for."
  :type 'string
  :group 'llm-gigachat)

;; (defun llm-gigachat-default-api-key-function ()
;;   "Fetch the API key with auth-source."
;;   (auth-source-pick-first-password :host "gigachat.devices.sberbank.ru"))

;; (defcustom llm-gigachat-api-key #'llm-gigachat-default-api-key-function
;;   "GigaChat key as a string or a function that loads and returns it."
;;   :group 'llm-gigachat
;;   :type '(choice (function :tag "Function")
;;                  (string :tag "String")))

(cl-defstruct llm-gigachat
 "A provider for the GigaChat LLM provider.

KEY is the API key for the GigaChat API

SCOPE is either GIGACHAT_API_PERS or GIGACHAT_API_CORP

CHAT-MODEL is the name of the chat model to use. If unset, will use a reasonable default.

TOKEN is the temporary API token for the GigaChat API. It is required to be populated before any call.

TOKEN-EXPIRES keeps track of when the token expires, because the token must be regenerated every 30 minutes."
  key
  scope
  (chat-model llm-gigachat-default-chat-model)
  token
  token-expires)

(defun llm-gigachat-uuid ()
  "Generate unique request id."
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s%s"
                          (user-uid)
                          (emacs-pid)
                          (system-name)
                          (user-full-name)
                          (current-time)
                          (emacs-uptime)
                          (garbage-collect)
                          (random)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring rnd 17 20)
            (substring rnd 20 32))))

(cl-defun llm-gigachat-request-token (provider)
  "Make a request to get access token.  The parsed response will be returned."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/x-www-form-urlencoded")
           ("Authorization" . ,(concat "Bearer " (llm-gigachat-key provider)))
           ("RqUID" . ,(llm-gigachat-uuid))))
        (url-request-data (encode-coding-string (concat "scope=" (llm-gigachat-scope provider)) 'utf-8)))
    (let ((buf (url-retrieve-synchronously "https://ngw.devices.sberbank.ru:9443/api/v2/oauth" t nil 5)))
      (if buf
          (with-current-buffer buf
            (url-http-parse-response)
            (if (and (>= url-http-response-status 200)
                     (< url-http-response-status 300))
                (llm-request--content)
              (error "LLM request failed with code %d: %s (additional information: %s)"
                     url-http-response-status
                     (nth 2 (assq url-http-response-status url-http-codes))
                     (string-trim (llm-request--content)))))
        (error "LLM request timed out")))))

(cl-defun llm-gigachat-refresh-token (provider)
  "Refresh the key in the GigaChat PROVIDER, if needed."
  (unless (and (llm-gigachat-token provider)
               (< 60 (float-time (time-subtract (or (llm-gigachat-token-expires provider) 0) (current-time)))))
    (let ((result (json-read-from-string (llm-gigachat-request-token provider))))
      (setf (llm-gigachat-token provider) (encode-coding-string (cdr (assoc 'access_token result)) 'utf-8))
      (setf (llm-gigachat-token-expires provider) (/ (cdr (assoc 'expires_at result)) 1000))
      )
    )
  )

(defun llm-gigachat--chat-request (provider prompt &optional streaming)
  "From PROMPT, create the data for the GigaChat chat request."
  (let ((request-alist))
    (push `("model" . ,(llm-gigachat-chat-model provider)) request-alist)
    (push `("messages" . ,(apply #'vector
                                 (mapcar (lambda (interaction)
                                           `(("role" . ,(pcase (llm-chat-prompt-interaction-role interaction)
                                                         ('user "user")
                                                         ('system (error "System role not supported"))
                                                         ('assistant "assistant")))
                                             ("content" . ,(llm-chat-prompt-interaction-content interaction))))
                                         (llm-chat-prompt-interactions prompt))))
          request-alist)
    (when (llm-chat-prompt-temperature prompt)
      (push `("temperature" . ,(llm-chat-prompt-temperature prompt)) request-alist))
    (when streaming (push `("stream" . ,t) request-alist))
    request-alist))


(defun llm-gigachat--handle-response (response extractor)
  "If RESPONSE is an error, throw it, else call EXTRACTOR."
  (if (cdr (assoc 'error response))
      (error (llm-gigachat--error-message response))
    (funcall extractor response)))


(defun llm-gigachat--error-message (err-response)
  "Return a user-visible error message from ERR-RESPONSE."
  (format "Problem calling GigaChat: status: %s message: %s (%s)"
          (assoc-default 'status (assoc-default 'error err-response))
          (assoc-default 'message (assoc-default 'error err-response))
          err-response))

(defun llm-gigachat--extract-chat-response (response)
  (let ((final (cl-find-if (lambda (choice)
                             (and (string= (cdr (assoc 'finish_reason choice)) "stop")
                                  (string= (cdr (assoc 'role (cdr (assoc 'message choice)))) "assistant")))
                           (cdr (assoc 'choices response)))))
    (when final
      (cdr (assoc 'content (cdr (assoc 'message final)))))))

(defun llm-gigachat--get-partial-chat-response (response)
  "Return the text in the partial chat response from RESPONSE."
  (let ((contents '())
        (lines (split-string response "\r?\n" t)))
    (dolist (line lines)
      (when (string-match "^data: \\(.*\\)" line)
        (let ((event-data (match-string 1 line)))
          ;; Check for [DONE] message and ignore it
          (unless (string= "[DONE]" event-data)
            ;; Parse event-data as JSON
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (choices (cdr (assoc 'choices (condition-case nil
                                                     (json-read-from-string event-data)
                                                   (error (format "Json error: %s" event-data)))
                                        )))
                   )
              (dolist (delta choices)
                (let ((content (cdr (assoc 'content (assoc 'delta delta)))))
                  (setq contents (cons content contents)))))))))
    (apply #'concat (nreverse contents))))

(cl-defmethod llm-chat ((provider llm-gigachat) prompt)
  (unless (llm-gigachat-key provider)
    (error "To call GigaChat API, the key must have been set"))
  (llm-gigachat-refresh-token provider)
  (let ((response (llm-gigachat--handle-response
                   (llm-request-sync "https://gigachat.devices.sberbank.ru/api/v1/chat/completions"
                                     :headers `(("Authorization" . ,(format "Bearer %s" (llm-gigachat-token provider))))
                                     :data (llm-gigachat--chat-request provider prompt))
                   #'llm-gigachat--extract-chat-response)))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
    response))

(cl-defmethod llm-chat-streaming ((provider llm-gigachat) prompt partial-callback response-callback error-callback)
  (unless (llm-gigachat-key provider)
    (error "To call GigaChat API, the key must have been set"))
  (llm-gigachat-refresh-token provider)
  (let ((buf (current-buffer)))
    (llm-request-async "https://gigachat.devices.sberbank.ru/api/v1/chat/completions"
                       :headers `(("Authorization" . ,(format "Bearer %s" (llm-gigachat-token provider))))
                       :data (llm-gigachat--chat-request provider prompt t)
                       :on-partial (lambda (data)
                                     (when-let ((response (llm-gigachat--get-partial-chat-response data)))
                                       (llm-request-callback-in-buffer buf partial-callback response)))
                       :on-success-raw (lambda (data)
                                         (let ((response (llm-gigachat--get-partial-chat-response data)))
                                           (setf (llm-chat-prompt-interactions prompt)
                                                 (append (llm-chat-prompt-interactions prompt)
                                                         (list (make-llm-chat-prompt-interaction :role 'assistant :content response))))
                                           (llm-request-callback-in-buffer buf response-callback response))
                                         )
                       :on-error (lambda (_ data)
                                   (llm-request-callback-in-buffer buf error-callback 'error
                                                                   (llm-gigachat--error-message data))))))


;; (let ((provider (make-llm-gigachat :key gigachat-auth-token :scope gigachat-scope))
;;       (llm-warn-on-nonfree nil))
;;   (llm-chat-streaming provider (llm-make-simple-chat-prompt "Какой редактор мощнее - Emacs или Vim?")
;;                       (lambda (text) (message (format "PART: %s" text)))
;;                       (lambda (text) (message (format "RESP: %s" text)))
;;                       (lambda (type message) (message (format "ERR: %s %s" type message)))
;;                       ))

;; (let ((provider (make-llm-gigachat :key gigachat-auth-token :scope gigachat-scope))
;;       (llm-warn-on-nonfree nil))
;;   (llm-chat provider (llm-make-simple-chat-prompt "Какой редактор мощнее - Emacs или Vim?")
;;  ))

(cl-defmethod llm-nonfree-message-info ((provider llm-gigachat))
  (ignore provider)
  (cons "Sber GigaChat" "https://developers.sber.ru/docs/ru/policies/gigachat-agreement/beta"))

(provide 'llm-gigachat)
