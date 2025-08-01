;;; typewriter-mode.el --- Typewriter overlay minor mode for Org -*- lexical-binding: t; -*-

n(defvar-local typewriter--font nil)
(defvar-local typewriter--paper-size 'letter)

(defvar paper-width-mm
  '(("a2_env" . 146.05)
    ("a4" . 210))


  "Mapping of paper size names to mm width.")

(defvar font-width-mm
  '(("Hermes 3000" . 2.56)) ; seems reasonable
  "The character width of a monospace typewriter font in mm")

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
  "Apply fill-column and font settings."
  ;; (when typewriter--font
  ;;   (face-remap-add-relative 'default :family typewriter--font))
  (let ((mm-per-line (cdr (assoc (symbol-name typewriter--paper-size) paper-width-mm))))
    (let ((char-width-mm (cdr (assoc (symbol-name typewriter--font) font-width-mm))))
      (when (and mm-per-line char-width-mm)
        (set-fill-column (floor (/ mm-per-line char-width-mm))))))
  (auto-fill-mode t)
  )


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
