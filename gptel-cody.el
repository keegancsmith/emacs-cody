;;; gptel-cody.el --- Cody support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sourcegraph

;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>

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

;; This file adds support for the Cody API to gptel

;;; Code:
(require 'gptel)

(cl-defstruct (gptel-cody (:constructor gptel--make-cody)
                          (:copier nil)
                          (:include gptel-backend)))

(cl-defmethod gptel-curl--parse-stream ((_backend gptel-cody) _info)
  "Parse Cody's streaming response."
  (let ((content-strs))
    (condition-case nil
        (while (re-search-forward "^data:" nil t)
          (when-let* ((response (gptel--json-read))
                      (delta (plist-get response :deltaText)))
            (push delta content-strs)))
      (error
       (goto-char (match-beginning 0))))
    (apply #'concat (nreverse content-strs))))

(cl-defmethod gptel--parse-response ((_backend gptel-cody) response _info)
  "Parse Cody's RESPONSE."
  (plist-get response :deltaText))

(cl-defmethod gptel--request-data ((_backend gptel-cody) prompts)
  "Prepare REQUEST-DATA for Cody API."
  `(:model "anthropic/claude-3-5-sonnet-20240620"
           :messages ,(vconcat prompts)
           :maxTokensToSample ,(or gptel-max-tokens 4000)
           :temperature ,(or gptel-temperature 0)
           :topK -1
           :topP -1
           :stopSequences ["</CODE5711>"]
           :stream ,(or (and gptel-stream gptel-use-curl
                             (gptel-backend-stream gptel-backend))
                        :json-false)))

(cl-defmethod gptel--parse-buffer ((_backend gptel-cody) &optional max-entries)
  "Parse current buffer backwards from point and return a list of prompts.

MAX-ENTRIES is the number of queries/responses to include for context."
  (let ((prompts) (prop))
    (while (and
            (or (not max-entries) (>= max-entries 0))
            (setq prop (text-property-search-backward
                        'gptel 'response
                        (when (get-char-property (max (point-min) (1- (point)))
                                                 'gptel)
                          t))))
      (push (list :speaker (if (prop-match-value prop) "assistant" "human")
                  :text
                  (string-trim
                   (buffer-substring-no-properties (prop-match-beginning prop)
                                                   (prop-match-end prop))
                   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                           (regexp-quote (gptel-prompt-prefix-string)))
                   (format "[\t\r\n ]*\\(?:%s\\)?[\t\r\n ]*"
                           (regexp-quote (gptel-response-prefix-string)))))
            prompts)
      (and max-entries (cl-decf max-entries)))
    (cons (list :speaker "system"
                :text gptel--system-message)
          prompts)))

;;;###autoload
(cl-defun gptel-make-cody
    (name &key
          (header (lambda ()
                    (when-let (key (gptel--get-api-key))
                      `(("Authorization" . ,(concat "token " key))
                        ("Content-Type" . "application/json")
                        ("Accept-Encoding" . "gzip;q=0")
                        ("User-Agent" . "edit / v1")))))
          (host "sourcegraph.com")
          (protocol "https")
          (endpoint "/.api/completions/stream")
          (stream t)
          (models '("anthropic/claude-3-5-sonnet-20240620"
                    "anthropic/claude-3-opus-20240229"
                    "openai/gpt-4o"
                    "google/gemini-1.5-pro"
                    "openai/cody-chat-preview-001"
                    "openai/cody-chat-preview-002"
                    "google/gemini-1.5-flash"
                    "anthropic/claude-3-haiku-20240307"
                    "fireworks/accounts/fireworks/models/mixtral-8x7b-instruct"))
          (key 'gptel-api-key)
          curl-args)
  "Create a Cody API backend for gptel.

NAME is a string to identify this backend.

Keyword arguments:

CURL-ARGS (optional) is a list of additional curl arguments.

HOST (optional) is the API host, defaults to \"sourcegraph.com\".

MODELS is a list of available model names.

STREAM is a boolean to toggle streaming responses, defaults to t.

PROTOCOL (optional) specifies the protocol, https by default.

ENDPOINT (optional) is the API endpoint for completions.

HEADER (optional) is for additional headers to send with each
request. It should be an alist or a function that returns an
alist.

KEY (optional) is a variable whose value is the API key, or
function that returns the key."
  (declare (indent 1))
  (let ((backend (gptel--make-cody
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models models
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :url (concat protocol "://" host endpoint "?api-version=2&client-name=gptel&client-version=v1"))))
    (setf (alist-get name gptel--known-backends
                     nil nil #'equal)
          backend)))

(provide 'gptel-cody)
;;; gptel-cody.el ends here
