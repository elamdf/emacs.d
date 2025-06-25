;; system-specific paths
(defvar projects-dir
  (expand-file-name "~/Documents/projects/")
  "Root directory for my project-related Org files.")

;; slack
(require 'auth-source)             ;; probably not necessary
(setq auth-sources '((:source "~/.authinfo")))


(use-package slack
  :bind (("C-c S K" . slack-stop)
         ("C-c S c" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms)
         ("C-c S U" . slack-user-select)
         ("C-c S s" . slack-search-from-messages)
         ("C-c S J" . slack-jump-to-browser)
         ("C-c S j" . slack-jump-to-app)
         ("C-c S e" . slack-insert-emoji)
         ("C-c S E" . slack-message-edit)
         ("C-c S r" . slack-message-add-reaction)
         ("C-c S t" . slack-thread-show-or-create)
         ("C-c S g" . slack-message-redisplay)
         ("C-c S G" . slack-conversations-list-update-quick)
         ("C-c S q" . slack-quote-and-reply)
         ("C-c S Q" . slack-quote-and-reply-with-link)
         (:map slack-mode-map
               (("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-thread-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel)))
         (:map slack-message-buffer-mode-map
               (("C-c '" . slack-message-write-another-buffer)))
         (:map slack-message-compose-buffer-mode-map
               (("C-c '" . slack-message-send-from-buffer)))
         )
  :custom
  (slack-extra-subscribed-channels (mapcar 'intern (list "some-channel")))
  :config
  (progn (slack-register-team
     :name "MIT"
     :token (auth-source-pick-first-password
              :host "mit.slack.com" :user "token" :max 1)
     :cookie (auth-source-pick-first-password
                 :host "mit.slack.com" :user "cookie" :max 1)     
     :full-and-display-names t
     :default t
     :subscribed-channels nil ;; using slack-extra-subscribed-channels because I can change it dynamically
     )
   (slack-start)
      (expand-file-name "Slack.org" projects-dir))

  )


(use-package alert
  :after slack
  :init
  (alert-define-style
   'my/slack-alert-style :title
   "Make Org headings for messages I receive - Style"

   :notifier
   (lambda (info)
     (message info)
     (write-region
      (s-concat
       "* TODO "
       (plist-get info :title)
       " : "
       (format
        "%s %s :slack:"
        (plist-get info :title)
        (plist-get info :message))
       "\n"
       (format "\n <%s>" (format-time-string "%Y-%m-%d %H:%M"))
       "\n")
      nil
      (expand-file-name "Slack.org" projects-dir)
      t)))
  (setq alert-default-style 'message)
  (add-to-list 'alert-user-configuration
               '(((:category . "slack")) my/slack-alert-style nil)))

;; email

;; (use-package notmuch) ; use notmuch to view them
;; (use-package mbsync) ; use mbsync to fetch emails

;;                                         ; TODO https://kb.mit.edu/confluence/display/mitcontrib/Configure+Thunderbird+for+use+with+MIT+Microsoft+365+Mailboxes
;; ; need to use
; TODO figure out how to get STMP working on mit email 
