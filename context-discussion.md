### does gptel make it possible to extend its context sources. Right now in its transient menu it offers adding a file or a buffer. I want to do a remote API call to retrieve context. Is that possible to do?

Yes, gptel does make it possible to extend its context sources, and you can add a remote API call to retrieve context. While this functionality isn't directly exposed in the transient menu, you can extend gptel's context capabilities by:

1. Creating a new function to fetch context from your remote API.
2. Adding this context to gptel's context system.
3. Optionally, adding a new option to the transient menu.

Here's how you could approach this:

1. Create a function to fetch and add context from your API:

```elisp
(defun my-gptel-add-api-context ()
  "Fetch context from a remote API and add it to gptel's context."
  (interactive)
  (let ((api-response (my-fetch-from-api))) ; Replace with your API call
    (with-temp-buffer
      (insert api-response)
      (gptel-context-add))))
```

2. The `gptel-context-add` function (aliased as `gptel-add`) is flexible and can work with different types of input. By calling it in a temporary buffer with the API response, you're effectively adding the API response as context.

3. If you want to add this to the transient menu, you can advise the `gptel-menu` function to add your new option:

```elisp
(defun my-gptel-menu-add-api-option (orig-fun &rest args)
  "Advice to add API context option to gptel-menu."
  (let ((transient-current-prefix 'gptel-menu))
    (transient-append-suffix 'gptel-menu "Context"
      '("a" "Add API context" my-gptel-add-api-context))))

(advice-add 'gptel-menu :around #'my-gptel-menu-add-api-option)
```

This will add a new option "a" to add API context in the "Context" section of the gptel menu.

4. If you want more control over how the API context is formatted or stored, you can modify the `gptel-context--alist` directly:

```elisp
(defun my-gptel-add-api-context ()
  "Fetch context from a remote API and add it to gptel's context."
  (interactive)
  (let ((api-response (my-fetch-from-api)))
    (push (cons "API Context" api-response) gptel-context--alist)))
```

5. You might also want to modify the `gptel-context--wrap-default` function or set a custom `gptel-context-wrap-function` to handle your new context type appropriately.

By implementing these extensions, you can effectively add a remote API as a context source for gptel, making it available through both programmatic calls and the transient menu interface.

### I want to process the prompt by the user and inspect things like the current repository the user is in to decide what context to include when "enhanced" context is on. Adjust your response to account for that.

Certainly! Your requirement to process the user's prompt and inspect the current environment (like the Git repository) to decide on context is a great use case for enhancing gptel's functionality. Here's an adjusted approach that takes this into account:

1. First, let's create a function to gather enhanced context:

```elisp
(defun my-gptel-gather-enhanced-context (prompt)
  "Gather enhanced context based on PROMPT and current environment."
  (let ((context ""))
    ;; Check if we're in a Git repository
    (when (vc-git-root default-directory)
      (setq context (concat context "Current Git repository: "
                            (vc-git-root default-directory) "\n")))

    ;; Analyze the prompt for keywords
    (cond
     ((string-match-p "\\bfile\\b" prompt)
      (setq context (concat context "Current file: " (buffer-file-name) "\n")))
     ((string-match-p "\\bproject\\b" prompt)
      (setq context (concat context "Project root: "
                            (project-root (project-current)) "\n")))
     ((string-match-p "\\bfunction\\b" prompt)
      (setq context (concat context "Current function: "
                            (which-function) "\n"))))

    ;; You could add more conditions here based on the prompt or environment

    context))
```

2. Now, let's modify gptel's context wrapping function to include this enhanced context:

```elisp
(defun my-gptel-context-wrap-function (message contexts)
  "Wrap MESSAGE with CONTEXTS and enhanced context."
  (let* ((enhanced-context (my-gptel-gather-enhanced-context message))
         (original-context (gptel-context--string contexts))
         (combined-context (concat enhanced-context original-context)))
    (if (> (length combined-context) 0)
        (pcase gptel-use-context
          ('system (concat message "\n\n" combined-context))
          ('user (concat combined-context "\n\n" message))
          ('nil message))
      message)))

(setq gptel-context-wrap-function #'my-gptel-context-wrap-function)
```

3. To make this "enhanced" context toggleable, we can add a new option to gptel's menu:

```elisp
(defvar my-gptel-use-enhanced-context nil
  "Whether to use enhanced context in gptel queries.")

(defun my-gptel-toggle-enhanced-context ()
  "Toggle enhanced context for gptel."
  (interactive)
  (setq my-gptel-use-enhanced-context (not my-gptel-use-enhanced-context))
  (message "Enhanced context %s"
           (if my-gptel-use-enhanced-context "enabled" "disabled")))

(defun my-gptel-menu-add-enhanced-context-option (orig-fun &rest args)
  "Advice to add enhanced context toggle to gptel-menu."
  (let ((transient-current-prefix 'gptel-menu))
    (transient-append-suffix 'gptel-menu "Context"
      '("e" "Toggle enhanced context" my-gptel-toggle-enhanced-context))))

(advice-add 'gptel-menu :around #'my-gptel-menu-add-enhanced-context-option)
```

4. Finally, update the context wrap function to respect this toggle:

```elisp
(defun my-gptel-context-wrap-function (message contexts)
  "Wrap MESSAGE with CONTEXTS and enhanced context if enabled."
  (let* ((enhanced-context (if my-gptel-use-enhanced-context
                               (my-gptel-gather-enhanced-context message)
                             ""))
         (original-context (gptel-context--string contexts))
         (combined-context (concat enhanced-context original-context)))
    (if (> (length combined-context) 0)
        (pcase gptel-use-context
          ('system (concat message "\n\n" combined-context))
          ('user (concat combined-context "\n\n" message))
          ('nil message))
      message)))
```

This approach allows you to:

1. Gather enhanced context based on the user's prompt and current environment.
2. Integrate this enhanced context with gptel's existing context system.
3. Toggle the use of enhanced context via the gptel menu.
4. Process each prompt to decide what context to include when enhanced context is enabled.

You can further expand the `my-gptel-gather-enhanced-context` function to include more sophisticated analysis of the prompt or to make API calls if necessary. This setup provides a flexible framework for adding intelligent, context-aware enhancements to gptel queries.

<!-- Local Variables: -->
<!-- gptel-model: "anthropic/claude-3-5-sonnet-20240620" -->
<!-- gptel--backend-name: "Cody" -->
<!-- gptel--bounds: ((210 . 2528) (2745 . 6960)) -->
<!-- End: -->
