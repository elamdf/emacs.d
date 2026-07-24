;;; org-reminders-export.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-id)
(require 'subr-x)

(defun org-reminders--tsv-escape (s)
  (replace-regexp-in-string
   "[\t\r\n]+" " "
   (string-trim (or s ""))))

(defun org-reminders--export-file (file outbuf)
  "Append active TODO entries from FILE to OUTBUF."
  (let* ((existing-buffer (find-buffer-visiting file))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buffer
          (unless (derived-mode-p 'org-mode)
            (org-mode))
          (org-with-wide-buffer
           (org-map-entries
            (lambda ()
              (let ((todo (org-get-todo-state)))
                (when (and todo
                           (not (member todo org-done-keywords)))
                  (let* ((id (org-id-get))
                         (title (org-get-heading t t t t))
                         (outline-path (mapconcat
                                        #'identity
                                        (org-get-outline-path t t)
                                        "/"))
                         (source (format "%s:%d"
                                         (buffer-file-name)
                                         (line-number-at-pos)))
                         (key (secure-hash 'sha1
                                           (if id
                                               (concat "org-id:" id)
                                             (concat "org-location:"
                                                     (file-truename (buffer-file-name))
                                                     ":"
                                                     outline-path
                                                     ":"
                                                     title)))))
                    (with-current-buffer outbuf
                      (insert
                       (mapconcat
                        #'org-reminders--tsv-escape
                        (list key id title source)
                        "\t")
                       "\n"))))))
            nil
            'file)))
      (when (and (not existing-buffer)
                 (buffer-live-p buffer))
        (kill-buffer buffer)))))

(defun org-reminders-export-todos (files out-file)
  "Export non-done Org TODO headings in FILES to OUT-FILE as TSV."
  (let ((outbuf (generate-new-buffer " *org-reminders-export*")))
    (unwind-protect
        (progn
          (dolist (file files)
            (when (file-readable-p file)
              (org-reminders--export-file file outbuf)))
          (with-current-buffer outbuf
            (write-region (point-min) (point-max) out-file nil 'silent)))
      (kill-buffer outbuf))))
