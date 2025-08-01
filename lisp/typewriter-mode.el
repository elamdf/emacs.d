;;; typewriter-mode.el --- A major mode to help draft things to be written on a typewriter -*- lexical-binding: t; -*-

(defvar-local typewriter--font nil)
(defvar-local typewriter--paper-size 'letter)
(defvar-local typewriter--chars-per-line 70)
(defvar-local typewriter--lines-per-page 10)

(defvar paper-dims-mm
  '(("a2_env" . (146.05 . 111.125))
    ("a4" . (215.9 . 296.926)))
  "Mapping of paper width, height names to mm width and height.")


(defvar font-dims-mm
  '(("Hermes 3000" . (2.56 . 4.27))) ; seems reasonable
  "The character (width, line height) of a monospace typewriter font in mm")

(defun line-separator-insert-overlays (n)
  (save-excursion
    (goto-char (point-min))
    (let ((line-number 1))
      (while (not (eobp))
        (let* ((ovs (overlays-in (line-beginning-position) (line-end-position))))
          (if (and (/= line-number 1) (zerop (% line-number n)))
              (if (null ovs)
                  (let* ((pos (line-beginning-position))
                         (ov (make-overlay pos pos)))
                    (overlay-put ov 'before-string (propertize "------------\n" 'face 'shadow)))
                )
            (dolist (ov ovs)
              (delete-overlay ov))
            )
          )
        (forward-line 1)
        (setq line-number (1+ line-number))))))

(defun line-separator--refresh (&rest _)
  (line-separator-insert-overlays typewriter--lines-per-page))

(defun typewriter--parse-params ()
  "Parse Org-style #+FONT and #+PAPER_SIZE headers in buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^#\\+\\([A-Z_]+\\):[ \t]*\\(.+\\)$" nil t)
        (let ((key (match-string 1))
              (val (string-trim (match-string 2))))
          (pcase key
            ("FONT" (setq typewriter--font (intern val)))
            ("PAPER_SIZE" (setq typewriter--paper-size (intern val)))))))))



(defun typewriter--apply-style ()
  "Apply compute and apply pagebreak overlays and enforce page width via auto-fill-mode."
  ;; (when typewriter--font
  ;;   (face-remap-add-relative 'default :family typewriter--font))
  (let ((line-size-mm (cdr (assoc (symbol-name typewriter--paper-size) paper-dims-mm))))
    (let ((char-size-mm (cdr (assoc (symbol-name typewriter--font) font-dims-mm))))
      (when (and line-size-mm char-size-mm)
        (setq typewriter--chars-per-line (floor (/ (car line-size-mm) (car char-size-mm))))
        (setq typewriter--lines-per-page (floor (/ (cdr line-size-mm) (cdr char-size-mm))))
        )))
  (set-fill-column typewriter--chars-per-line)
  (line-separator--refresh)
  (auto-fill-mode t)
  )

(add-hook 'after-change-functions #'(lambda (&rest _)
                                      (if (eq major-mode 'typewriter-mode) (line-separator--refresh))))


;;;###autoload
(define-derived-mode typewriter-mode text-mode "🪶 typewriter"
  "Major mode for drafting typewriter text."
  )

(add-hook 'typewriter-mode-hook
          (lambda ()
            (progn
              (typewriter--parse-params)
              (typewriter--apply-style))

            ))
(add-hook 'typewriter-mode-hook #'auto-fill-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.typewriter\\'" . typewriter-mode))

(provide 'typewriter-mode)
