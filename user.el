;; org stuff
(defun my/org-todo-list-swiper ()
  "Open `org-todo-list` and start `swiper` in its buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Org Agenda*")))
    ;; Generate the global todo list
    (org-todo-list)
    ;; Wait for the agenda buffer to be displayed
    (with-current-buffer buf
      (while (not (get-buffer-window buf))
        (sit-for 0.05)))
    ;; Switch to that window and run swiper
    (select-window (get-buffer-window buf))
    (swiper)))


(defun my/compare-todo-status (a b)
  "Compare strings A and B based on embedded TODO statuses: TODO < WAIT < DONE.
Return 1 if A > B, 0 if A = B, and -1 if A < B."
  (let ((status-order '("TODO" "WAIT" "DONE")))
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
                   (new-name (format "%s_%s.org" safe-title date))
                   (new-path (expand-file-name new-name (file-name-directory buffer-file-name))))
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
  "quotes to display on a new org file"
  '("“All you have to do is write one true sentence. Write the truest sentence that you know.” Ernest Hemingway"))


(defun my/show-random-org-quote ()
  "Display a random quote when a new Org file is opened."
  (when (and buffer-file-name
             (string-equal (file-name-extension buffer-file-name) "org")
             (not (file-exists-p buffer-file-name)))
    (message "%s" (nth (random (length my/org-quotes)) my/org-quotes))))

(provide 'user)
