(setq inhibit-startup-message t)

(when (display-graphic-p)
(setq visible-bell t)
(scroll-bar-mode -1) ; no visual scrolbar
(tool-bar-mode -1) ; disable toolbar
(tooltip-mode -1) ; no tooltips
(set-fringe-mode 10) ; breathing room
(menu-bar-mode -1) ; no menu bar
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)
(load-theme 'tango-dark)
)



;; user funcs
(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "/bismuth"))

(require 'user)

(require 'bismuth)
(require 'brain-mode)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage));; install elpaca


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(load-library   "server")
(if (not (server-running-p))  (server-start))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)

(column-number-mode)
(which-key-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)
(setq switch-to-buffer-obey-display-actions t)


(use-package command-log-mode)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   '(("x" "Describe command here" tags "" nil)
     ("d" "Daily Agenda" ((agenda "" ((org-agenda-span 'day)))) nil)))
 '(org-agenda-files
   '("/Users/elamdf/Documents/projects/motifs.org"
     "/Users/elamdf/Documents/projects/ofot.org"
     "/Users/elamdf/Documents/projects/capture.org"
     "/Users/elamdf/Documents/projects/apl_photonics.org"))
 '(org-agenda-sorting-strategy
   '((agenda user-defined-up) (todo urgency-down category-keep)
     (tags urgency-down category-keep) (search category-keep)))
 '(org-agenda-span 'week)
 '(package-selected-packages
   '(command-log-mode company conda counsel counsel-projectile
                      doom-modeline doom-themes exec-path-from-shell
                      gptel helpful htmlize ivy ivy-rich ivy-todo
                      lsp-metals lsp-mode lsp-ui magit orderless
                      org-projectile org-zotxt ox-reveal projectile
                      rainbow-delimiters rust-mode swiper tree-sitter
                      tree-sitter-langs tree-sitter-yaml treemacs
                      yaml-mode yasnippet yasnippet-snippets zotxt))
 '(projectile-global-ignore-file-patterns '("\\#*"))
 '(projectile-indexing-method 'alien)
 '(projectile-project-search-path '(list ("~/bwrc" . 1) ("~/Documents" . 1)))
 '(safe-local-variable-values
   '((org-export-initial-scope . buffer) (eval require 'org-make-toc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(when (daemonp)
  (exec-path-from-shell-initialize))

(use-package company)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; TODO: make regex p the default for all search, I don't want fuzzy
(setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))) ;; fuzzy remains default for other commands




(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))
;; (setq counsel-find-file-at-point t)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package swiper :ensure t)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)))

;; keybindings
(setq mac-command-modifier 'meta) ; make cmd meta to save my thumb


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; tabs are stupid
(setq-default indent-tabs-mode nil)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")

  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(defun my/markdown-comment-tweak ()
  (setq-local comment-start nil))

(eval-after-load "markdown"
  (add-hook 'markdown-mode-hook #'my/markdown-comment-tweak))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
    (setq projectile-project-search-path '(list "~/bwrc" "~/Documents"))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; don't display backup files 
(setq counsel-find-file-ignore-regexp "\\~\\'")

;; conda


(use-package tramp

  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-completion-reread-directory-timeout nil)


  (tramp-default-remote-shell "/bin/bash")

  (tramp-encoding-shell "/bin/sh")

  (tramp-sh-extra-args '(("/bash\\'" . "-noediting -norc -noprofile")
		     ("/usr/bin/env" . "bash -noediting -norc -noprofile")))
  :config
  (add-to-list 'tramp-default-proxies-alist
	   '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist
	   '((regexp-quote (system-name)) nil nil))

  (with-eval-after-load 'tramp
(setq tramp-remote-path
      (append tramp-remote-path
	      '(tramp-own-remote-path))))
  )

(setq tramp-default-remote-shell "/bin/bash")

(use-package magit
  :commands magit-status)

(use-package forge
  :after magit
  )
(use-package pr-review
  :after magit
  )

(setq auth-sources '("~/.authinfo"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
    (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'

  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode)
(add-hook 'python-mode-hook 'lsp-deferred)
(use-package tree-sitter)
(use-package yaml-mode)
(use-package tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :custom
  (rust-indent-where-clause t)
  (rust-format-on-save t)
  (rust-format-show-buffer nil))
(use-package lsp-metals
  :ensure t
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))


;; -- calyx --
(use-package calyx-mode
  :straight (calyx-mode :host github :repo "sgpthomas/calyx-mode"))

;;; ----- Essential Org Mode Configuration -----

(setq org-ellipsis " ▾"
      org-startup-folded 'content
      org-cycle-separator-lines 2
      org-fontify-quote-and-verse-blocks t)

;; Indent org-mode buffers for readability
(add-hook 'org-mode-hook #'org-indent-mode)

;; Set up Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;;; --- Org ---
(setq org-directory "~/projects")
(setq org-default-notes-file (concat org-directory "/notes.org"))


(setq org-agenda-cmp-user-defined 'my/compare-todo-status)
;; Use org-tempo
(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("einit" . "src emacs-lisp :tangle emacs/init.el")
                  ("emodule" . "src emacs-lisp :tangle emacs/modules/dw-MODULE.el")))
    (add-to-list 'org-structure-template-alist item)))




;; --- keybindings ---
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c ;") #'replace-regexp)


; gptel
(global-set-key (kbd "C-c s") #'gptel-menu)
(global-set-key (kbd "C-c g") #'gptel)

; org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c m") #'my/create-meeting-notes-file)
;; bismuth
(global-set-key (kbd "C-c t") #'inline-cr-list-all-actionables)



;; disable arrow keys to learn real bindings faster
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))


(setq org-deadline-warning-days 1)
(setq org-use-fast-tag-selection t)

(setq org-tag-alist '(
  ("arch" . ?a)
  ("arch_dataflow" . ?a)
  ("arch_languages_compilers" . ?a)
  ("arch_modeling" . ?a)
  ("arch_tensor_accelerators" . ?a)
  ("courses" . ?c)
  ("courses_arch_170a" . ?c)
  ("courses_cmu_advanced_database_systems_lectures" . ?c)
  ("courses_ee_105" . ?c)
  ("courses_ee_142" . ?c)
  ("courses_ee_232" . ?c)
  ("courses_ee_240b" . ?c)
  ("courses_slavic_50" . ?c)
  ("courses_ugba_191i" . ?c)
  ("ic" . ?i)
  ("ic_cad" . ?i)
  ("ic_electronic_ic" . ?i)
  ("ic_hyperscale" . ?i)
  ("ic_hyperscale_smartnic" . ?i)
  ("ic_isscc_2025" . ?i)
  ("ic_photonic_ic" . ?i)
  ("ic_rtl_simulation" . ?i)
  ("ic_synthesis" . ?i)
  ("misc" . ?m)
  ("misc_bio" . ?m)
  ("misc_compilers" . ?m)
  ("misc_formal" . ?m)
  ("misc_misc_arch" . ?m)
  ("misc_misc_systems" . ?m)
  ("misc_misc_talk_slides" . ?m)
  ("misc_ml" . ?m)
  ("misc_neuro" . ?m)
  ("misc_phd_theses" . ?m)
  ("misc_philosophy" . ?m)
  ("misc_physics" . ?m)
  ("misc_psych" . ?m)
  ("references" . ?r)
  ("references_datsheets" . ?r)
  ("references_manuals" . ?r)
  ("references_pdk_docs" . ?r)
  ("references_reference_docs" . ?r)
  ("references_specs" . ?r)
  ("references_specs_errata" . ?r)
  ("references_ta" . ?r)
  ("references_ta_eecs_251b_ta_stuff" . ?r)
  ("references_textbooks" . ?r)
))



;; org capture
(setq org-capture-templates
      '(("r" "Read a Book" entry
         (file+headline "~/Documents/projects/capture.org" "Reading List")         
         "* READ %^{Title} by %^{Author} %^g: \n Entered on %U\n  %?")
        ("v" "Watch a Video" entry
         (file+headline "~/Documents/projects/capture.org" "Watch List")
         "* WATCH %^{Title} %^g:\n Link: %^{URL}\n  Entered on %U\n  %?")
        ("Z" "Read from Zotero" entry
         (file+headline "~/Documents/projects/capture.org" "Reading List")
         "* READ %(my/zotero-latest-capture-string)Entered on %U\n  %?")))
         ;; "%(my/zotero-latest-capture-string)\nEntered on %U\n")))


(setq org-refile-targets
      (mapcar (lambda (file) (cons file '(:maxlevel . 3)))
              (my/org-project-files)))

(use-package zotxt
  :ensure t
  :after org
  ;; :bind ("C-c z" . zotxt-cite)
  :config
  (require 'zotxt))

;; org todo

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "WATCH(v)" "READ(r)" "|" "DONE(d!)" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(("READ" . "dark green")
        ("WATCH" . "dark blue")))
(setq org-src-fontify-natively t)
(use-package htmlize)
(setq org-export-publishing-directory "./assets")

(setq org-agenda-custom-commands
      '(("d" "Daily Agenda"
         ((agenda "" ((org-agenda-span 'day)))))
      ("r" "Read/Watch List"
         todo "READ|WATCH"
         ((org-agenda-overriding-header "Things to Read or Watch")))
      ))


(use-package ox-reveal)

;; --- Conda ---
(use-package conda)
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)


;;; ----- Dired -----



(defun dw/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (hl-line-mode 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :config
  (setq dired-listing-switches "-alv"
        dired-omit-files "^\\..*~?$"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t))

(add-hook 'dired-mode-hook 'dired-omit-mode)

;; Make sure ripgrep is used everywhere
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --noheading")

;; snippets
(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets/org-mode" "~/.emacs.d/yasnippet-snippets/snippets")))
(use-package yasnippet-snippets)
(yas-global-mode)



;; to run things after filling in field
(defun yas/schedule-field-skip ()
  (add-hook 'post-command-hook 'yas/field-skip-once 'append 'local))

(defun yas/field-skip-once ()
  (condition-case err
      (yas/next-field)
    (error nil))
  (remove-hook 'post-command-hook 'yas/field-skip-once 'local))
(put 'list-timers 'disabled nil)

;; meeting notes templating

(add-hook 'find-file-hook #'my/show-random-org-quote)

;; gptel for local llm
(use-package gptel)

(setq
 gptel-model 'gemma3:4b
 gptel-backend (gptel-make-ollama "Gemma 3 4B"
                 :host "localhost:11434"
                 :stream t
                 :models '(gemma3:4b)))

(add-hook 'gptel-post-response-functions 'gptel-end-of-response)
(add-hook 'gptel-before-send-hook #'my/ensure-ollama-running)
;; make sure ollama is running before sending gptel commands


(defun my/advise-gptel-commands ()

  "Advise all `gptel-` commands to ensure Ollama is running first."
  (dolist (sym (apropos-internal "^gptel-" 'commandp))
    (advice-add sym :before #'my/ensure-ollama-running)))

(my/advise-gptel-commands)


;; bismuth
;; enable inline comments by default for some filetypes
;;;###autoload
(add-hook 'markdown-mode-hook #'inline-cr-mode)
;;;###autoload
(add-hook 'org-mode-hook #'inline-cr-mode)
;;;###autoload
(add-hook 'c-mode-hook #'inline-cr-mode)
