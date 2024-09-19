;;; gptel-cody.el --- Cody support for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sourcegraph

;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Version: 0.0.1-alpha
;; Package-Requires: ((emacs "27.1") (gptel "0.9.0"))
;; Keywords: convenience
;; URL: https://github.com/sourcegraph/gptel-cody

;; SPDX-License-Identifier: Apache-2.0

;;; Commentary:
;; This file adds support for the Cody API to gptel

;;; Code:
(require 'gptel)

(defconst gptel-cody--client-name "Cody-Emacs-gptel"
  "The client name for Cody API requests.")

(defconst gptel-cody--version
  (package-get-version)
  "The version of gptel-cody package.")

(defconst gptel-cody--user-agent
  (format "%s/%s" gptel-cody--client-name gptel-cody--version)
  "The User-Agent string for Cody API requests.")

(cl-defstruct (gptel-cody (:constructor gptel--make-cody)
                          (:copier nil)
                          (:include gptel-backend)))

(cl-defmethod gptel-cody-fetch-models-async ((backend gptel-cody))
  "Asynchronously fetch models for the Cody backend and update the models slot."
  (let* ((host (gptel-backend-host backend))
         (url (format "https://%s/.api/modelconfig/supported-models.json" host))
         (url-request-method "GET")
         (url-request-extra-headers
          (funcall (gptel-backend-header backend))))
    (url-retrieve
     url
     (lambda (status)
       (if-let ((err (plist-get status :error)))
           (message "Error fetching Cody models: %s" err)
         (goto-char (point-min))
         (re-search-forward "\n\n")
         (let* ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol)
                (response (json-read))
                (default-chat-model (cdr (assoc 'chat (cdr (assoc 'defaultModels response)))))
                (all-models (cdr (assoc 'models response)))
                (chat-models (seq-filter
                              (lambda (model)
                                (and (member "chat" (cdr (assoc 'capabilities model)))
                                     (not (string-match-p "fast-edit" (cdr (assoc 'modelRef model))))))
                              all-models))
                (chat-models-names (mapcar (lambda (model) (cdr (assoc 'modelRef model))) chat-models))
                (chat-models-names (cons default-chat-model (remove default-chat-model chat-models-names))))
           (setf (gptel-cody-models backend) chat-models-names)
           (message "Updated %s models: %s" (gptel-backend-name backend) chat-models-names))))
     nil t t)))

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
  `(:model ,gptel-model
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

(cl-defun gptel-cody-add-directive ()
  "Add the Cody directive to the end of `gptel-directives` if missing."
  (unless (assq 'cody gptel-directives)
    (customize-set-variable 'gptel-directives
                            (append gptel-directives
                                    '((cody . "You are Cody, an AI coding assistant from Sourcegraph. If your answer contains fenced code blocks in Markdown, include the relevant full file path in the code block tag using this structure: ```$LANGUAGE:$FILEPATH```"))))))

;;;###autoload
(cl-defun gptel-make-cody
    (name &key
          (host "sourcegraph.com")
          (protocol "https")
          (endpoint "/.api/completions/stream")
          (stream t)
          (models nil)
          (key 'gptel-api-key)
          (header (lambda ()
                    (when-let (key-resolved (gptel--get-api-key key))
                      `(("Authorization" . ,(concat "token " key-resolved))
                        ("User-Agent" . ,gptel-cody--user-agent)))))
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
  (gptel-cody-add-directive)
  (declare (indent 1))
  (let ((backend (gptel--make-cody
                  :curl-args curl-args
                  :name name
                  :host host
                  :header header
                  :key key
                  :models (or models (if (string= host "sourcegraph.com")
                                         '("anthropic/claude-3-5-sonnet-20240620")
                                       '("anthropic::2023-06-01::claude-3.5-sonnet")))
                  :protocol protocol
                  :endpoint endpoint
                  :stream stream
                  :url (concat protocol "://" host endpoint "?api-version=2&client-name=" gptel-cody--client-name "&client-version=" gptel-cody--version))))
    ;; using default models, so fetch from remote.
    (unless models (gptel-cody-fetch-models-async backend))
    (setf (alist-get name gptel--known-backends
                     nil nil #'equal)
          backend)))

(provide 'gptel-cody)
;;; gptel-cody.el ends here
