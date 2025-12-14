;; various helpers for markdown mode that pull in some of obisidan's features, including
;; extended wiki link syntax and autocompletion for insertion
;; correct rendering of inline image links

(defun elamdf/insert-relative-wiki-link ()
  "Prompt with completion for a file and insert its path relative
to the directory of the file backing the current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((base (file-name-directory buffer-file-name))
         (file (read-file-name "Insert relative path: " base nil t)))
    (insert (concat "[[" (string-remove-suffix ".md" (file-relative-name file base)) "]]" ))))


;; allow us to follow wiki links with headers
;; the only thing we've actually changed is markdown-follow-wiki-link but I don't know how to replace the markdown-mode internal function pointers so we just replace the refs in our custom version of the call stack for C-c C-o
(require 'markdown-mode)
(defun elamdf/markdown-follow-wiki-link (name &optional other)
  "Follow the wiki link NAME, supporting optional #HEADER fragments.
Convert NAME to a file name and call `find-file'. Ensure the new buffer
is in `markdown-mode'. Open in another window when OTHER is non-nil."

  (let* ((wp (file-name-directory buffer-file-name))
         (i (string-match-p "#" name))
         (fname  (if i (substring name 0 i) name))
         (header (and i (substring name (1+ i))))
         (filename (markdown-convert-wiki-link-to-filename fname)))

    (when other (other-window 1))
    (let ((default-directory wp))
      (find-file filename))

    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))
         (warn "AAA")
    ;; Jump to header if present.
         (when (and header (not (string-empty-p header)))
         (warn header)
      ;; Prefer imenu if available (works well with markdown headings)

         ;; Fallback: search for a markdown heading line whose title matches
         (goto-char (point-min))

         (when (re-search-forward
                (format "^#+[ \t]+%s[ \t]*$" (regexp-quote header))
                nil t)
           (beginning-of-line)
           (recenter)))))

(defun elamdf/markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.
See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (elamdf/markdown-follow-wiki-link (markdown-wiki-link-link) arg)
    (user-error "Point is not at a Wiki Link")))

(defun elamdf/markdown-follow-thing-at-point (arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or the another window if
ARG is non-nil.
See `markdown-follow-link-at-point' and
`markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((markdown-link-p)
         (markdown-follow-link-at-point))
        ((markdown-wiki-link-p)
         (elamdf/markdown-follow-wiki-link-at-point arg))
        (t
         (let* ((values (markdown-link-at-pos (point)))
                (url (nth 3 values)))
           (unless url
             (user-error "Nothing to follow at point"))
           (markdown--browse-url url)))))

(defun elamdf/display-image-linked-at-point (start end file )
  "Display the linked image in place of the given link"

          (when (not (zerop (length file)))
            (unless (file-exists-p file)
              (let* ((download-file (funcall markdown-translate-filename-function file))
                     (valid-url (ignore-errors
                                  (member (downcase (url-type (url-generic-parse-url download-file)))
                                          markdown-remote-image-protocols))))
                (if (and markdown-display-remote-images valid-url)
                    (setq file (markdown--get-remote-image download-file))
                  (when (not valid-url)
                    ;; strip query parameter
                    (setq file (replace-regexp-in-string "?.+\\'" "" file))
                    (unless (file-exists-p file)
                      (setq file (url-unhex-string file)))))))
            (when (file-exists-p file)
              (let* ((abspath (if (file-name-absolute-p file)
                                  file
                                (concat default-directory file)))
                     (image
                      (cond ((and markdown-max-image-size
                                  (image-type-available-p 'imagemagick))
                             (create-image
                              abspath 'imagemagick nil
                              :max-width (car markdown-max-image-size)
                              :max-height (cdr markdown-max-image-size)))
                            (markdown-max-image-size
                             (create-image abspath nil nil
                                           :max-width (car markdown-max-image-size)
                                           :max-height (cdr markdown-max-image-size)))
                            (t (create-image abspath)))))
                (when image
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'display image)
                    (overlay-put ov 'face 'default)
                    (push ov markdown-inline-image-overlays))))))
          )

(defconst obsidian-embed-regex
  "!\\[\\[\\([^]\n|]+\\)\\(?:|\\([^]\n]+\\)\\)?\\]\\]"
  "Match Obsidian embeds like ![[fname]] or ![[fname|350]].
Group 1 = fname, Group 2 = alias/size (optional).")
(defconst resource-folder-name "0 Resources")

(defun image-fname-to-path (name)
  (concat (projectile-project-root) "/" resource-folder-name "/" name)
  )

(defun elamdf/markdown-display-inline-images ()
  "Add inline image overlays to image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."
  (interactive)
  (unless (display-images-p)
    (error "Cannot show images"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-link-inline nil t)
        (let* ((start (match-beginning 0))
               (imagep (match-beginning 1))
               (end (match-end 0))
               (file (image-fname-to-path (match-string-no-properties 6))))
        (if imagep (elamdf/display-image-linked-at-point start end file))
        ))
      (goto-char (point-min))
      (while (re-search-forward obsidian-embed-regex nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (file (image-fname-to-path (match-string-no-properties 1))))
         (elamdf/display-image-linked-at-point start end file )
        ))
      )
    )
  )

(defun elamdf/markdown-toggle-inline-images ()
  "Toggle inline image overlays in the buffer."
  (interactive)
  (if markdown-inline-image-overlays
      (markdown-remove-inline-images)
     (elamdf/markdown-display-inline-images)))

(provide 'obsidian-helpers)
