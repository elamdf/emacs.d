
;; global defs
(defvar my/org-projects-dir "~/Documents/projects/")


(defun my/compare-todo-status (a b)
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
(defvar meeting-notes-dir
  (expand-file-name "~/Documents/meeting_notes")
  "Directory where new meeting notes files are created.")

(defun my/create-meeting-notes-file ()
  "Create a new Org file in `meeting-notes-dir`, insert and expand the `meet` snippet."
  (interactive)
  (let* ((default-directory meeting-notes-dir)
         (_ (unless (file-exists-p meeting-notes-dir)
              (make-directory meeting-notes-dir t)))
         (filename (make-temp-file "meeting-" nil ".org"))
         (buf (find-file filename)))
    (with-current-buffer buf
      (erase-buffer)
      ;; insert snippet key and expand
      (insert "<meet")
      (goto-char (point-max))
      (yas-expand))
    (switch-to-buffer buf)))

(defun my/rename-meeting-notes-file-maybe ()
  "If the just-finished YASnippet had key 'meeting', rename the file."
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TITLE: \\(.*\\)$" nil t)
            (let* ((title (match-string 1))
                   (safe-title (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (downcase title)))
                   (date (format-time-string "%Y-%m-%d"))
                   (new-name (file-name-nondirectory (format "%s_%s.org" safe-title date)))
                   (new-path (expand-file-name new-name meetings-dir)))
              (unless (string= new-path buffer-file-name)
                (when (or (not (file-exists-p new-path))
                          (yes-or-no-p (format "Rename file to %s?" new-name)))
                  (rename-file buffer-file-name new-path t)
                  (set-visited-file-name new-path t t)
                  (set-buffer-modified-p nil)
                  (message "Renamed and switched to: %s" new-name)))))))


(defun my/insert-org-participant-tags ()
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


(defvar my/org-quotes
  '("\“All you have to do is write one true sentence. Write the truest sentence that you know.\” Ernest Hemingway"))


(defun my/show-random-org-quote ()
  "Display a random quote when a new Org file is opened."
  (when (and buffer-file-name
             (string-equal (file-name-extension buffer-file-name) "org")
             (not (file-exists-p buffer-file-name)))
    (message "%s" (nth (random (length my/org-quotes)) my/org-quotes))))



(defun my/ensure-ollama-running (&rest args)
  "Start `ollama serve` if it's not already running."
  (unless (get-process "ollama")
    (let ((proc (start-process-shell-command
                 "ollama daemon" "*ollama daemon*"
                 "pgrep -f 'ollama serve' || ollama serve")))
      (set-process-query-on-exit-flag proc nil))))

(defun my/org-project-files ()
  "Return a list of all .org files in `my/org-projects-dir`."
  (directory-files-recursively my/org-projects-dir "\\.org$"))


(defun my/zotero-latest-capture-string ()

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
          (format "%s %s :%s:" title (or (and (not (string-blank-p authors)) (format "by %s" authors)) "")  (or (and (not (string-blank-p tag)) (format ":%s:" tag)) "") ))
      "[No Zotero item found] \nqq%?")))


(defun hs-cycle (&optional level)
  (interactive "p")
  (let (message-log-max
        (inhibit-message t))
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;; TODO: Fix this case. `hs-show-block' needs to be
           ;; called twice to open all folds of the parent
           ;; block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
               (hs-hide-block)
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level))))

(defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

(provide 'user)
