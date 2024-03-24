;;; claude-shell.el --- Integration with Anthropic's Claude LLM -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Armin Friedl
;;
;; Author: Armin Friedl <dev@friedl.net>
;; Maintainer: Armin Friedl <dev@friedl.net>
;; Created: März 16, 2024
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
;; `claude-shell' is a comint-based shell to interact with Antrophic's Claude
;; AI. It is based on `shell-maker' providing a convenience layer on top of
;; comint. It strives to be similar in functionality and spirit to
;; `chatgpt-shell' (https://github.com/xenodium/chatgpt-shell) just for Claude
;; models.
;;
;; `claude-shell' provides an interactive chat with Claude in a dedictated shell
;; buffer. It also integrates with the rest of Emacs to allow seamlessly calling
;; out to Claude on-demand. Similar to the many AI copilots around.
;;
;; You must set `claude-shell-api-token' to your API token before using it.
;;
;; Run `claude-shell' to get an interactive Claude shell.
;;
;;; Code:

(require 'shell-maker)
(require 'claude-shell-fontifier)

(defvar claude-shell--api-url "https://api.anthropic.com/v1/messages"
  "The Anthropic API entry point.")

(defvar claude-shell--api-version "2023-06-01"
  "The Anthropic API version.")

(defvar claude-shell--models
  '(("claude-3-haiku-20240307" . "Fastest and most compact model for near-instant responsiveness.")
    ("claude-3-sonnet-20240229" . "Ideal balance of intelligence and speed for enterprise workloads.")
    ("claude-3-opus-20240229" . "Most powerful model for highly complex tasks."))
  "List of Anthropic's Claude models.

See also
https://docs.anthropic.com/claude/docs/models-overview#model-comparison")

(defcustom claude-shell-api-token nil
  "The Anthropic API token as a string or a function that loads and returns it.

The token can be generated inside your account at
https://console.anthropic.com/settings/keys"
  :type '(choice string function)
  :group 'claude-shell)

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
  :group 'claude-shell)

(defcustom claude-shell-streaming 'nil
  "Whether or not to stream Anthropic responses (show chunks as they arrive)."
  :type 'boolean
  :group 'claude-shell)

(defcustom claude-shell-after-command-functions nil
  "Abnormal hook (i.e. with parameters) invoked after each command.

This is useful if you'd like to automatically handle or suggest things
post execution.

For example:

\(add-hook `claude-shell-after-command-functions'
   (lambda (command output)
     (message \"Command: %s\" command)
     (message \"Output: %s\" output)))"
  :type 'hook
  :group 'claude-shell)

(defun claude-shell-system-prompt ()
  "Get the currently chosen value for the system prompt."
  (assoc-string claude-shell-system-prompt claude-shell-system-prompts))

(defun claude-shell-swap-model ()
  "Change model from `claude-shell--models'."
  (interactive)
  (unless (eq major-mode 'claude-shell-mode)
    (user-error "Not in a shell"))
  (let ((choice (completing-read "Model: " (map-keys claude-shell--models))))
    (customize-set-value 'claude-shell-model choice)
    (claude-shell--update-prompt)
    (shell-maker-interrupt nil)))

(defun claude-shell-swap-system-prompt ()
  "Change system prompt choice from `claude-shell-system-prompts'."
  (interactive)
  (unless (eq major-mode 'claude-shell-mode)
    (user-error "Not in a shell"))
  (when-let ((duplicates (claude-shell-duplicate-map-keys claude-shell-system-prompts)))
    (user-error "Duplicate prompt names found %s. Please remove" duplicates))
  (let* ((choices (append (list "None")
                          (map-keys claude-shell-system-prompts)))
         (choice (completing-read "System prompt: " choices)))
    (if (or (string-equal choice "None")
            (string-empty-p (string-trim choice)))
        (customize-set-value 'claude-shell-system-prompt nil)
      (customize-set-value 'claude-shell-system-prompt choice)))
  (claude-shell--update-prompt)
  (shell-maker-interrupt nil))

(defun claude-shell--load-awesome-prompts-parse-alist ()
  "Helper function for `claude-shell-load-awesome-prompts'.

Download awesome-prompts and parse into a list of label and
prompt cons."
  (let ((url "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv")
        (collector '()))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (forward-line 2)
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (split (split-string line "," 'nil "\""))
               (head (car split))
               (tail (apply #'concat (cdr split))))
          (push (cons head tail) collector)
          (forward-line 1))))
    collector))


(defun claude-shell-load-awesome-prompts ()
  "Load `claude-shell-system-prompts' from awesome-chatgpt-prompts.

Downloaded from https://github.com/f/awesome-chatgpt-prompts."
  (interactive)
  (let ((prompts (claude-shell--load-awesome-prompts-parse-alist)))
    (setq claude-shell-system-prompts
          (map-merge 'list
                     claude-shell-system-prompts
                     (seq-sort (lambda (lhs rhs) (string-lessp (car lhs) (car rhs))) prompts)))
    (message "Loaded awesome-chatgpt-prompts")
    (setq claude-shell-system-prompt nil)
    (claude-shell-swap-system-prompt)))


(defun claude-shell-duplicate-map-keys (map)
  "Return duplicate keys in MAP."
  (let ((keys (map-keys map))
        (seen '())
        (duplicates '()))
    (dolist (key keys)
      (if (member key seen)
          (push key duplicates)
        (push key seen)))
    duplicates))

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

(defun claude-shell--make-payload (history)
  "Submit the given HISTORY to the Anthropic API.

Returns the payload as a Lisp structure for serialization into json. See
https://docs.anthropic.com/claude/reference/messages_post for the
interpretation."

  (let ((history (mapcan (lambda (l)
                           (when (cdr l)
                             `((:role "user" :content ,(car l)) (:role "assistant" :content ,(cdr l)))))
                         (butlast history)))
        (command `((:role "user" :content ,(caar (last history))))))

    `(:max_tokens  1024
      :model ,claude-shell-model
      :system ,(cdr (claude-shell-system-prompt))
      :messages ,(vconcat (append history command))
      :stream ,(if claude-shell-streaming 't :false))))

(defun claude-shell--update-prompt ()
  "Update the `shell-maker' prompt."
  (let ((shell-prompt (format "Claude(%s/%s)> " claude-shell-model (car (claude-shell-system-prompt)))))
    (shell-maker-set-prompt shell-prompt (concat "^" shell-prompt))))

(defun claude-shell--extract-claude-response (json)
  "Extract Claude response from JSON."

  (let ((parsed (cond
                 ((eq (type-of json) 'cons) json)
                 (t (shell-maker--json-parse-string json)))))
    (let-alist parsed
      (string-trim
       (or (unless (seq-empty-p .content)
             (let-alist (seq-first .content) .text))
           (progn
             (message "Claude API error:\n\t%s" json)
             (format "Error: %s" .error.message))
           "")))))

(defvar claude-shell--config
  (make-shell-maker-config
   :name "Claude"
   :prompt (format "Claude(%s/%s)> " claude-shell-model (car (claude-shell-system-prompt)))
   :validate-command
   (lambda (_command)
     (unless claude-shell-api-token
       "Variable `claude-shell-api-token' needs to be set to your key.

Try M-x set-variable claude-shell-api-token

or

(setq claude-shell-api-token \"my-key\")"))
   :execute-command
   (lambda (_command history callback error-callback)
     (shell-maker-async-shell-command
      (claude-shell--call-api (claude-shell--make-payload history))
      claude-shell-streaming
      #'claude-shell--extract-claude-response
      callback
      error-callback))
   :on-command-finished
   (lambda (command output)
     (claude-shell-fontifier--put-source-block-overlays)
     (run-hook-with-args 'claude-shell-after-command-functions
                         command output))
   :redact-log-output
   (lambda (output)
     (if claude-shell-api-token
         (replace-regexp-in-string (regexp-quote claude-shell-api-token)
                                   "SK-REDACTED-ANTHROPIC-KEY"
                                   output)
       output))))

;;;###autoload
(defun claude-shell ()
  "Start an FastGPT shell."
  (interactive)
  (shell-maker-start claude-shell--config)
  (claude-shell--update-prompt)
  (define-key claude-shell-mode-map (kbd "C-c C-v")
              #'claude-shell-swap-model)
  (define-key claude-shell-mode-map (kbd "C-c C-s")
              #'claude-shell-swap-system-prompt))

(provide 'claude-shell)
;;; claude-shell.el ends here
