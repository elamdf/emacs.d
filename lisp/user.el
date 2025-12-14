
;; global defs
(defvar elamdf/org-projects-dir "~/Documents/projects/")

(defun elamdf/elfeed-org-capture-or-goto ()
  "Create an Org capture entry for the current elfeed entry, or jump to it if it already exists.
Run this from an `elfeed-entry` buffer."
  (interactive)
  (require 'org-capture)
  (let* ((entry (elfeed-entry--get-entry))
         (title (elfeed-entry-title entry))
         (link (elfeed-entry-link entry))
         (author (or (elfeed-meta entry :author) ""))
         (org-file (expand-file-name "notes.org" elamdf/org-projects-dir)) ;; change this to your file
         (headline "Articles"))        ;; and this to your headline
    (if (not (and title link))
        (message "Missing title or link in elfeed entry.")
      (with-current-buffer (find-file-noselect org-file)
        (goto-char (point-min))
        (if (re-search-forward (regexp-quote link) nil t)
            (progn
              (org-show-entry)
              (org-reveal)
              (switch-to-buffer (current-buffer))
              (message "Found existing entry for: %s" title))
          (let ((org-capture-link-is-already-stored t)
                (org-stored-links `((,link ,title)))
                (org-capture-entry
                 `("e" "Elfeed article" entry
                   (file+headline ,org-file ,headline)
                   ,(concat "* [[%:link][%:title]]\n"
                            ":PROPERTIES:\n"
                            ":AUTHOR: " author "\n"
                            ":LINK: %:link\n"
                            ":END:\n\n%?"))))
            (org-capture)))))))


(defun elamdf/compare-todo-status (a b)
  "Compare strings A and B based on embedded TODO statuses: TODO < WAIT < DONE.
Return 1 if A > B, 0 if A = B, and -1 if A < B."
  (let ((status-order '("TODO" "READ" "WATCH" "WAIT" "DONE")))
    (cl-labels ((status-rank (str)
                  (or (cl-position-if (lambda (s) (string-match-p s str)) status-order)
                      (length status-order))))  ; if none found, return a high rank
      (let ((rank-a (status-rank a))
            (rank-b (status-rank b)))
        (cond
         ((< rank-a rank-b) -1)
         ((> rank-a rank-b) 1)
         (t 0))))))
;; org meeting stuff
(defvar elamdf/meeting-notes-dir
  (expand-file-name "~/Documents/meeting_notes")
  "Directory where new meeting notes files are created.")

(defun elamdf/create-meeting-notes-file ()
  "Create a new Org file in `elamdf/meeting-notes-dir`, insert and expand the `meet` snippet."
  (interactive)
  (let* ((default-directory elamdf/meeting-notes-dir)
         (_ (unless (file-exists-p elamdf/meeting-notes-dir)
              (make-directory elamdf/meeting-notes-dir t)))
         (filename (make-temp-file "meeting-" nil ".org"))
         (buf (find-file filename)))
    (with-current-buffer buf
      (erase-buffer)
      ;; insert snippet key and expand
      (insert "<meet")
      (goto-char (point-max))
      (yas-expand))
    (switch-to-buffer buf)))

(defun elamdf/rename-meeting-notes-file-maybe ()
  "If the just-finished YASnippet had key 'meeting', rename the file."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
      (let* ((title (match-string 1))
             (safe-title (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (downcase title)))
             (date (format-time-string "%Y-%m-%d"))
             (new-name (file-name-nondirectory (format "%s_%s.org" safe-title date)))
             (new-path (expand-file-name new-name elamdf/meeting-notes-dir)))
        (unless (string= new-path buffer-file-name)
          (when (or (not (file-exists-p new-path))
                    (yes-or-no-p (format "Rename file to %s?" new-name)))
            (rename-file buffer-file-name new-path t)
            (set-visited-file-name new-path t t)
            (set-buffer-modified-p nil)
            (message "Renamed and switched to: %s" new-name)))))))


(defun elamdf/insert-org-participant-tags ()
  "Prompt for participant names, add new ones to the file, and return a #+TAGS: line as a string."
  (let* ((file "~/.org-participants.txt")
         (existing (if (file-exists-p file)
                       (split-string (with-temp-buffer
                                       (insert-file-contents file)
                                       (buffer-string)) "\n" t)
                     '()))
         (crm-separator "[ \t]*,[ \t]*") ;; allow names with spaces
         (input (completing-read-multiple
                 "Participants (comma-separated): " existing nil nil))
         (tags (mapcar (lambda (n)
                         (concat "@" (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (downcase n)) "_participant"))
                       input))
         (tag-line (concat " " (string-join tags " "))))
    ;; Append new names to the file if needed
    (let ((new-names (seq-remove (lambda (n) (member n existing)) input)))
      (when new-names
        (with-temp-buffer
          (insert (mapconcat #'identity new-names "\n"))
          (insert "\n")
          (append-to-file (point-min) (point-max) file))))
    tag-line))  ;; Return this string instead of inserting


(defvar elamdf/org-quotes
  '("\“All you have to do is write one true sentence. Write the truest sentence that you know.\” Ernest Hemingway"))

(defun elamdf/show-random-org-quote ()
  "Display a random quote when a new Org file is opened."
  (when (and buffer-file-name
             (string-equal (file-name-extension buffer-file-name) "org")
             (not (file-exists-p buffer-file-name)))
    (message "%s" (nth (random (length elamdf/org-quotes)) elamdf/org-quotes))))

(defun elamdf/ensure-ollama-running (&rest args)
  "Start `ollama serve` if it's not already running."
  (unless (get-process "ollama")
    (let ((proc (start-process-shell-command
                 "ollama daemon" "*ollama daemon*"
                 "pgrep -f 'ollama serve' || ollama serve")))
      (set-process-query-on-exit-flag proc nil))))

(defun elamdf/org-project-files ()
  "Return a list of all .org files in `elamdf/org-projects-dir`."
  (directory-files-recursively elamdf/org-projects-dir "\\.org$"))


(defun elamdf/zotero-latest-capture-string ()

  "Return an Org entry string with title, authors, and tag from the most recently added Zotero item."
  (let* ((query
          "SELECT title.value AS title, \
                  group_concat(creators.lastName || ', ' || creators.firstName, ', ') AS authors, \
                  group_concat(c.collectionName, '/') AS collection_path \
           FROM items \
           JOIN itemData AS titleData ON titleData.itemID = items.itemID \
           JOIN fields ON titleData.fieldID = fields.fieldID \
           JOIN itemDataValues AS title ON titleData.valueID = title.valueID \
           LEFT JOIN itemCreators ON itemCreators.itemID = items.itemID \
           LEFT JOIN creators ON itemCreators.creatorID = creators.creatorID \
           LEFT JOIN collectionItems AS ci ON ci.itemID = items.itemID \
           LEFT JOIN collections AS c ON c.collectionID = ci.collectionID \
           WHERE fields.fieldName = 'title' AND items.itemTypeID != 3 \
           GROUP BY items.itemID \
           ORDER BY items.dateAdded DESC \
           LIMIT 1;")
         (cmd (format "sqlite3 -readonly -separator \"|\" ~/Zotero/zotero.sqlite \"%s\"" query))
         (raw (shell-command-to-string cmd)))

    (if (string-match "\\(.*?\\)|\\(.*?\\)|\\(.*\\)" raw)
        (let* ((title (string-trim (match-string 1 raw)))
               (authors (string-trim (match-string 2 raw)))
               (tag-path (string-trim (match-string 3 raw)))
               (tag (->> tag-path
                         (replace-regexp-in-string " " "_")
                         (replace-regexp-in-string "/" "_"))))
          (format "%s :%s:
:PROPERTIES:
:AUTHORS: %s" title   (or (and (not (string-blank-p tag)) (format ":%s:" tag)) "") (or (and (not (string-blank-p authors)) authors) "") ))
      "[No Zotero item found] \nqq%?")))


(defun elamdf/hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('elamdf/hs-cycle
           (hs-hide-level 1)
           (setq this-command 'elamdf/hs-cycle-children))
          ('elamdf/hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'elamdf/hs-cycle-subtree))
          ('elamdf/hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'elamdf/hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun elamdf/hs-global-cycle ()
  (interactive)
  (pcase last-command
    ('elamdf/hs-global-cycle
     (save-excursion (hs-show-all))
     (setq this-command 'elamdf/hs-global-show))
    (_ (hs-hide-all))))

(defun elamdf/org-word-count ()
  "Count words in region/buffer, estimate pages, and reading time.
Excludes lines beginning with * or #. Prints result in echo area."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (word-count
          (save-excursion
            (goto-char start)
            (let ((count 0)
                  (inhibit-field-text-motion t))
              (while (< (point) end)
                (beginning-of-line)
                (unless (looking-at-p "^[*#<]")
                  (let ((line-end (line-end-position)))
                    (while (re-search-forward "\\w+\\W*" line-end t)
                      (setq count (1+ count)))))
                (forward-line 1))
              count)))
         (words-per-page 400)
         (reading-speed 215)
         (page-count (/ (+ word-count words-per-page -1) words-per-page))
         (reading-time (/ (+ word-count reading-speed -1) reading-speed)))
    (message "%d words, ~%d pages, ~%d min read"
             word-count page-count reading-time)))

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

(provide 'user)
