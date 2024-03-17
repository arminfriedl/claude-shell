;;; claude-shell.el --- Claude API integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Armin Friedl
;;
;; Author: Armin Friedl <dev@friedl.net>
;; Maintainer: Armin Friedl <dev@friedl.net>
;; Created: März 16, 2024
;; Modified: März 16, 2024
;; Version: 0.0.1
;; Keywords: anthropic claude shell-maker terminals wp help tools
;; Homepage: https://github.com/arminfriedl/claude-shell
;; Package-Requires: ((emacs "29.1") (shell-maker "0.49.1"))
;;
;; This file is not part of GNU Emacs.

;; MIT License

;; Copyright (c) 2024 Armin Friedl

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'shell-maker)

(load-file "claude-shell-fontifier.el")

(defcustom claude-shell-api-token nil
  "The Anthropic API token as a string or a function that loads and returns it.

The token can be generated inside your account at
https://console.anthropic.com/settings/keys"
  :type '(choice string function)
  :group 'claude-shell)

(defvar claude-shell--api-url "https://api.anthropic.com/v1/messages"
  "The Anthropic API entry point.")

(defvar claude-shell--models
  '(("claude-3-haiku-20240307" . "Fastest and most compact model for near-instant responsiveness.")
    ("claude-3-sonnet-20240229" . "Ideal balance of intelligence and speed for enterprise workloads.")
    ("claude-3-opus-20240229" . "Most powerful model for highly complex tasks."))
  "List of Anthropic's Claude models.

See also
https://docs.anthropic.com/claude/docs/models-overview#model-comparison")

(defcustom claude-shell-model "claude-3-haiku-20240307"
  "Which model to use."
  :type (append '(choice)
                (mapcar (lambda (engine) `(const :doc ,(cdr engine) ,(car engine)))
                        claude-shell--models))
  :group 'claude-shell)

(defcustom claude-shell-system-prompts
  `(("tl;dr" . "Be as succint but informative as possible and respond in tl;dr form to my queries")
    ("General" . "You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.")
    ;; Based on https://github.com/benjamin-asdf/dotfiles/blob/8fd18ff6bd2a1ed2379e53e26282f01dcc397e44/mememacs/.emacs-mememacs.d/init.el#L768
    ("Programming" . "The user is a programmer with very limited time.
                        You treat their time as precious. You do not repeat obvious things, including their query.
                        You are as concise as possible in responses.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses.
                        Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets.
                        Whenever you output updated code for the user, only show diffs, instead of entire snippets.")
    ("Positive Programming" . "Your goal is to help the user become an amazing computer programmer.
                        You are positive and encouraging.
                        You love see them learn.
                        You do not repeat obvious things, including their query.
                        You are as concise in responses. You always guide the user go one level deeper and help them see patterns.
                        You never apologize for confusions because it would waste their time.
                        You use markdown liberally to structure responses. Always show code snippets in markdown blocks with language labels.
                        Don't explain code snippets. Whenever you output updated code for the user, only show diffs, instead of entire snippets."))

  "List of system prompts to choose from.

If prompt is a cons, its car will be used as a title to display.

For example:

\(\"Translating\" . \"You are a helpful English to Spanish assistant.\")\"
\(\"Programming\" . \"The user is a programmer with very limited time...\")"
  :type '(alist :key-type (string :tag "Title")
                :value-type (string :tag "Prompt value"))
  :group 'claude-shell)

(defcustom claude-shell-system-prompt "Programming"
  "Which prompt to use."
  :type (append '(choice)
                (mapcar (lambda (prompt) `(const :doc ,(cdr prompt) ,(car prompt)))
                        claude-shell-system-prompts))
  :get (lambda (symbol) (assoc-string (symbol-value symbol) claude-shell-system-prompts))
  :group 'claude-shell)

(defcustom claude-shell-streaming 'nil
  "Whether or not to stream Anthropic responses (show chunks as they arrive)."
  :type 'boolean
  :group 'claude-shell)

(defvar claude-shell--api-version "2023-06-01")

(defun claude-shell--curl-flags ()
  "Collect flags for a `curl' command to call the Anthropic API."
  (let ((token (cond ((functionp claude-shell-api-token) (funcall claude-shell-api-token))
                     ((stringp claude-shell-api-token) claude-shell-api-token)
                     (t (error "No API token configured in variable claude-shell-api-token")))))
    `("--silent"
      "--header" ,(format "x-api-key: %s" token)
      "--header" "content-type: application/json"
      "--header" ,(format "anthropic-version: %s" claude-shell--api-version))))

(defun claude-shell--call-api (object)
  "Submit the OBJECT to the API end-point at URL.

The OBJECT will be JSON encoded and sent as HTTP POST data."
  (let ((json (shell-maker--json-encode object))
        (json-path (make-temp-file "claude-shell-request" nil ".json")))
    (with-temp-file json-path (insert json))
    (append (list "curl" claude-shell--api-url)
            (claude-shell--curl-flags)
            `("--data" ,(format "@%s" json-path)))))

(defun claude-shell--prompt (prompt)
  "Submit the given PROMPT to the Anthropic API.

Returns the JSON response as a string. See
https://docs.anthropic.com/claude/reference/messages_post for the
interpretation."

  `(:max_tokens  1024
    :model ,claude-shell-model
    :system ,(cdr claude-shell-system-prompt)
    :messages [(:role "user"
                :content ,(substring-no-properties prompt))]
    :stream ,(if claude-shell-streaming 't :false)))

(defun claude-shell--extract-claude-response (json)
  "Extract Claude response from JSON."
  (if (eq (type-of json) 'cons)
      (let-alist json ;; already parsed
        (or (unless (seq-empty-p .content)
              (let-alist (seq-first .content) .text))
            .error.message
            ""))
    (if-let (parsed (shell-maker--json-parse-string json))
        (string-trim
         (let-alist parsed
           (unless (seq-empty-p .content)
             (let-alist (seq-first .content) .text))))
      (if-let (parsed-error (shell-maker--json-parse-string-filtering
                             json "^curl:.*\n?"))
          (let-alist parsed-error
            .error.message)))))


(defvar claude-shell--config
  (make-shell-maker-config
   :name "Claude"
   :prompt (format "Claude(%s/%s)> " claude-shell-model (car claude-shell-system-prompt))
   :validate-command
   (lambda (_command)
     (unless claude-shell-api-token
       "Variable `claude-shell-api-token' needs to be set to your key.

Try M-x set-variable claude-shell-api-token

or

(setq claude-shell-api-token \"my-key\")"))
   :execute-command
   (lambda (command _history callback error-callback)
     (shell-maker-async-shell-command
      (claude-shell--call-api (claude-shell--prompt command))
      claude-shell-streaming
      #'claude-shell--extract-claude-response
      callback
      error-callback))
   :on-command-finished
   (lambda (command output)
     (claude-shell-fontifier--put-source-block-overlays)
     (run-hook-with-args 'chatgpt-shell-after-command-functions
                         command output))
   :redact-log-output
   (lambda (output)
     (if claude-shell-api-token
         (replace-regexp-in-string (regexp-quote claude-shell-api-token)
                                   "SK-REDACTED-ANTHROPIC-KEY"
                                   output)
       output))))

;;; FastGPT shell

;;;###autoload
(defun claude-shell ()
  "Start an FastGPT shell."
  (interactive)
  (shell-maker-start claude-shell--config))


(provide 'claude-shell)
;;; claude-shell.el ends here
