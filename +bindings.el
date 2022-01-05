;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; toggle flycheck on and off

(map! "C-z" nil)
(setq iedit-toggle-key-default nil)
(setq-default indent-tabs-mode nil)

;;(setq doom-localleader-alt-key "C-z")
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Not sure if i want this
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; Backups
;; Settled by doom in .local/cache/backup
;;
;; (defvar --backup-directory (concat user-emacs-directory "backups"))
;; (if (not (file-exists-p --backup-directory))
;;        (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t         ; backup of a file the first time it is saved.
      backup-by-copying t         ; don't clobber symlinks
      version-control t           ; version numbers for backup files
      vc-make-backup-files t      ; backup versioned files (git?)
      delete-old-versions t       ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6 ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
      )

;; No persistent undo history
;; (remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)

(use-package! windmove
  :config
  (windmove-default-keybindings)
  ;; Make windmove work in Org mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

;; To consider removing
(use-package! swiper
  :bind ("C-s" . swiper)
        ("C-r" . swiper))

(use-package! undo-fu
  :bind ("C-?" . undo-fu-only-redo))

;; From nov website
(use-package! nov
  :hook (nov-mode . variable-pitch-mode)
  :mode ("\\.\\(epub\\|mobi\\)\\'" . nov-mode)
  :config
  ;; Default Font
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.2))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  ;; Rendering
  (require 'justify-kp)
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
                 'my-nov-window-configuration-change-hook
                 t))
  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
        (let ((max-width (pj-line-width))
              buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
                (goto-char (line-end-position))
                (when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
                'my-nov-window-configuration-change-hook
                nil t)))
  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook))

(after! org-noter
  org-noter-doc-split-fraction '(0.57 0.43))

(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))

(use-package! kibit-helper
  :bind ("C-x C-'" . kibit-accept-proposed-change))

(use-package! clojure-essential-ref-nov
  :after cider
  :init
  (setq clojure-essential-ref-nov-epub-path "~/Dropbox/Books/Clojure/Clojure_The_Essential_Reference_v29_MEAP.epub")
  :bind (
         :map cider-mode-map
         ("C-c C-d C-r" . clojure-essential-ref)
         ("C-c C-d r" . clojure-essential-ref-nov)
         :map cider-repl-mode-map
         ("C-c C-d C-r" . clojure-essential-ref)))

(use-package! cider
  :bind ("C-c C-e" . cider-pprint-eval-defun-to-comment)
  :config
  ;; (setq nrepl-log-messages t)
  (setq nrepl-use-ssh-fallback-for-remote-hosts t)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  ;; (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  )

(after! clojure-mode
;;; (define-clojure-indent
  ;;   (PUT 2)
  ;;   (POST 2)
  ;;   (GET 2)
  ;;   (PATCH 2)
  ;;   (DELETE 2)
  ;;   (context 2)
  ;;   (for-all 2)
  ;;   (checking 3)
  ;;   (>defn :defn)
  ;;   (>defn- :defn)
  ;;   (match 1)
  ;;   (cond 0)
  ;;   (case 1)
  ;;   (describe 1)
  ;;   (it 2)
  ;;   (fn-traced :defn)
  ;;   (defn-traced :defn)
  ;;   (assert-match 1))
  (setq clojure-indent-style 'align-arguments)
  (add-to-list 'clojure-align-binding-forms "let-flow")
  ;; (setq clojure-indent-style 'always-align)
  ;; (setq clojure-indent-style 'always-indent)
  ;; (setq cider-default-cljs-repl 'shadow)
  (put '>defn 'clojure-doc-string-elt 2)
  (put '>defn- 'clojure-doc-string-elt 2)
  (put 'defsys 'clojure-doc-string-elt 2)
  (put 'defn-traced 'clojure-doc-string-elt 2)

  ;; (setq clojure-align-forms-automatically t)
  ;; See if i use below
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("time" . "java-time")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "clj-http.client")
          ("url" . "cemerick.url")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "cheshire.core")
          ("s" . "clojure.spec.alpha")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("rf.db" . "re-frame.db")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("w" . "wing.core")
          ("gen" . "clojure.spec.gen.alpha"))))

(use-package! emms
  :config
  (require 'emms-setup)
  (emms-all)
  ;; (emms-standard)
  (emms-default-players)
  ;; (emms-mode-line 1)
  ;; (emms-playing-time-disable-display)
  (setq emms-source-file-default-directory "~/Music/")

  ;; Added /usr/local/bin/emms-info-libtag
  (require 'emms-info-libtag)
  (require 'emms-volume)
  (setq emms-info-functions '(emms-info-libtag))
  ;; might not be working, had to copy to get working
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)

  (defun my-emms-play-url-at-point ()
    "Same as `emms-play-url' but with url at point."
    (interactive)
    (emms-play-url (url-get-url-at-point))))


(use-package! magit
  :bind ("C-c g" . magit-file-dispatch))

(use-package! dired-narrow
  :commands (dired-narrow-fuzzy)
  :init
  (map! :map dired-mode-map
        :desc "narrow" "/" #'dired-narrow-fuzzy))

(use-package! org
  :config
  (setq org-refile-targets (quote (("newgtd.org" :maxlevel . 1)
                                   ("someday.org" :level . 2))))
  ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
  (setq org-image-actual-width nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-plantuml-jar-path "/home/zackteo/Documents/plantuml.jar"))

;; (add-to-list 'org-latex-classes
;;              '("article"
;;                "\\documentclass[10pt,article,oneside]{memoir}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;              '("book"
;;                "\\documentclass[10pt]{memoir}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;              )

;; If image folder not created, stringp error
(use-package! org-download
  :commands
  org-download-dnd
  org-download-yank
  org-download-screenshot
  org-download-dnd-base64
  :init
  (map! :map org-mode-map
        "s-Y" #'org-download-screenshot
        "s-y" #'org-download-yank)
  (pushnew! dnd-protocol-alist
            '("^\\(?:https?\\|ftp\\|file\\|nfs\\):" . +org-qdragndrop-download-dnd-fn)
            '("^data:" . org-download-dnd-base64))
  (advice-add #'org-download-enable :override #'ignore)
  :config
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat "./images/"
                            (replace-regexp-in-string " " "_"
                                                      (downcase (file-name-base buffer-file-name)))))
           (filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  :config
  (setq org-download-screenshot-method
        (cond ((executable-find "maim")  "maim -u -s %s")
              ((executable-find "scrot") "scrot -s %s")))
  (setq org-download-method '+org/org-download-method))


(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))


(use-package! mathpix.el
  :commands (mathpix-screenshot)
  :init
  (setq mathpix-screenshot-method (cond ((executable-find "maim")  "maim -u -s %s")
                                        ((executable-find "scrot") "scrot -s %s"))
        mathpix-app-id ""
        mathpix-app-key "")
  (map! "C-x m" #'mathpix-screenshot))

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (shell . t)
   (emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org)
(require 'ob-clojure)

(setq org-babel-clojure-backend 'cider)
(require 'cider)

(after! format
  (set-formatter! 'clang-format
    '("clang-format"
      "-style={BasedOnStyle: Google, IndentWidth: 4, SortIncludes: false}"
      ("-assume-filename=%S" (or buffer-file-name mode-result "")))))

(setq-hook! 'c++-mode-hook +format-with 'clang-format)

;; (setq +format-on-save-enabled-modes
;;       '(not c++-mode))

;; (require 'lsp-java-boot)
;; to enable the lenses
;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)


;; (use-package! lsp-mode
;;   :hook ((clojure-mode . lsp)
;;          (clojurec-mode . lsp)
;;          (clojurescript-mode . lsp))
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   ;; (setenv "PATH" (concat
;;   ;;                  "/usr/local/bin" path-separator
;;   ;;                  (getenv "PATH")))
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))


;; telega
(set-fontset-font t 'unicode "Symbola" nil 'append)
;; (use-package! telega
;;   :init
;;   ;; (setq telega-chat-show-deleted-messages-for nil)
;;   )

;; For more intense debugging

;; (defun doom--backtrace ()
;;   (let* ((n 0)
;;          (frame (backtrace-frame n))
;;          (frame-list nil)
;;          (in-program-stack nil))
;;     (while frame
;;       (when in-program-stack
;;         (push (cdr frame) frame-list))
;;       (when (eq (elt frame 1) 'doom-debugger)
;;         (setq in-program-stack t))
;;       (setq n (1+ n)
;;             frame (backtrace-frame n)))
;;     (reverse frame-list)))

;; (defun doom-debugger (error data)
;;   "A custom debugger for `debugger'.
;; Writes the backtrace to another buffer, in case it is lost for whatever reason."
;;   (with-current-buffer (get-buffer-create "*rescued-backtrace*")
;;     (let ((standard-output (current-buffer)))
;;       (prin1 error)
;;       (prin1 data)
;;       (mapc #'print (doom--backtrace)))
;;     (kill-new (buffer-string))
;;     (message "Backtrace copied to your clipboard"))
;;   (debug error data))

;; (setq debugger #'doom-debugger)

;; (toggle-debug-on-error)
