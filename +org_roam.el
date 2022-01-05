;;; ~/.doom.d/+org_roam.el -*- lexical-binding: t; -*-

;; (setq org-roam-v2-ack t)

(use-package! org-roam
  :init
  (map! (:leader
         :prefix ("m" . "org-roam")
         "D" #'org-roam-demote-entire-buffer
         "f" #'org-roam-node-find
         "F" #'org-roam-ref-find
         "g" #'org-roam-graph
         "i" #'org-roam-node-insert
         "I" #'org-id-get-create
         "m" #'org-roam-buffer-toggle
         "M" #'org-roam-buffer-display-dedicated
         "n" #'org-roam-capture
         "r" #'org-roam-refile
         "R" #'org-roam-link-replace-all
         (:prefix ("d" . "by date")
          :desc "Goto previous note" "b" #'org-roam-dailies-goto-previous-note
          :desc "Goto date"          "d" #'org-roam-dailies-goto-date
          :desc "Capture date"       "D" #'org-roam-dailies-capture-date
          :desc "Goto next note"     "f" #'org-roam-dailies-goto-next-note
          :desc "Goto tomorrow"      "m" #'org-roam-dailies-goto-tomorrow
          :desc "Capture tomorrow"   "M" #'org-roam-dailies-capture-tomorrow
          :desc "Capture today"      "n" #'org-roam-dailies-capture-today
          :desc "Goto today"         "t" #'org-roam-dailies-goto-today
          :desc "Capture today"      "T" #'org-roam-dailies-capture-today
          :desc "Goto yesterday"     "y" #'org-roam-dailies-goto-yesterday
          :desc "Capture yesterday"  "Y" #'org-roam-dailies-capture-yesterday
          :desc "Find directory"     "-" #'org-roam-dailies-find-directory)
         (:prefix ("o" . "node properties")
          "a" #'org-roam-alias-add
          "A" #'org-roam-alias-remove
          "t" #'org-roam-tag-add
          "T" #'org-roam-tag-remove
          "r" #'org-roam-ref-add
          "R" #'org-roam-ref-remove))))

  ;; (map! :leader
  ;;       :prefix "m"
  ;;       :desc "org-roam" "l" #'org-roam-buffer-toggle
  ;;       :desc "org-roam-node-insert" "i" #'org-roam-node-insert
  ;;       :desc "org-roam-node-find" "f" #'org-roam-node-find
  ;;       :desc "org-roam-ref-find" "r" #'org-roam-ref-find
  ;;       :desc "org-roam-show-graph" "g" #'org-roam-show-graph
  ;;       :desc "org-roam-capture" "c" #'org-roam-capture
  ;;       :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  ;; (setq org-roam-directory (file-truename "~/Dropbox/org/roam/")
  ;;       org-roam-db-gc-threshold most-positive-fixnum
  ;;       org-id-link-to-org-use-id t)
  ;; (add-to-list 'display-buffer-alist
  ;;              '(("\\*org-roam\\*"
  ;;                 (display-buffer-in-direction)
  ;;                 (direction . right)
  ;;                 (window-width . 0.33)
  ;;                 (window-height . fit-window-to-buffer))))
  ;; :config
  ;; (setq org-roam-mode-sections
  ;;       (list #'org-roam-backlinks-insert-section
  ;;             #'org-roam-reflinks-insert-section
  ;;             ;; #'org-roam-unlinked-references-insert-section
  ;;             ))
  ;; (org-roam-setup)
  ;; (setq org-roam-capture-templates
  ;;       '(("d" "default" plain
  ;;          "%?"
  ;;          :if-new (file+head "${slug}.org"
  ;;                             "#+title: ${title}\n")
  ;;          :immediate-finish t
  ;;          :unnarrowed t)))
  ;; (setq org-roam-capture-ref-templates
  ;;       '(("r" "ref" plain
  ;;          "%?"
  ;;          :if-new (file+head "${slug}.org"
  ;;                             "#+title: ${title}\n")
  ;;          :unnarrowed t)))

;;   (setq org-roam-dailies-directory "daily/")
;;   (setq org-roam-dailies-capture-templates
;;         '(("d" "default" entry
;;            "* %?"
;;            :if-new (file+head "%<%Y-%m-%d>.org"
;;                               "#+title: %<%Y-%m-%d>\n"))))
;;   ;; (set-company-backend! 'org-mode '(company-capf))
;;   )


;; (defun string-filter (title)
;;   (let ((common-words '("the" "of" "a" "as"))
;;         (title-length 3)
;;         (title-word-list (split-string title "\_")))
;;     (let ((new-list (seq-remove (lambda (elt) (member elt common-words)) title-word-list)))
;;       (string-join (butlast new-list (- (length new-list) title-length)) "_"))))
;; (string-filter "irresistible_the_rise_of_addictive_technology_and_the_business_of_keeping_us_hooked")

;; (defun string-filter2 (title)
;;     (let ((list (split-string title "\_")))
;;       (string-join (butlast list (- (length list) 3)) "_")))
;;        (list (seq-remove (lambda (elt) (member elt (common-words))) title-word-list)))
;;           (string-join (butlast list (- (length list) 3)) "_")))

;; (defun my-cool-title-function (str)
;;   (string-filter (org-roam--title-to-slug str)))
;;(seq-remove (lambda (elt) (member elt '("the" "of" "and")))
;;            (split-string "irresistible_the_rise_of_addictive_technology_and_the_business_of_keeping_us_hooked" "\_"))

;; (use-package! org-roam
;;   :custom
;;   (org-roam-directory "~/Dropbox/org/roam/")
;;   (org-roam-index-file "index.org")
;;   :bind (:map org-roam-mode-map
;;          (("C-c m l" . org-roam)
;;           ("C-c m f" . org-roam-find-file)
;;           ("C-c m r" . org-roam-find-ref)
;;           ("C-c m d" . org-roam-find-directory)
;;           ("C-c m j" . org-roam-jump-to-index)
;;           ("C-c m b" . org-roam-switch-to-buffer)
;;           ("C-c m g" . org-roam-graph)
;;           ("C-c m c" . org-roam-capture))
;;          :map org-mode-map
;;          (("C-c m i" . org-roam-insert)))
;;   :config
;;   ;; (setq org-roam-title-to-slug-function  #'my-cool-title-function)
;;   ;; (setq org-roam-title-to-slug-function  #'org-roam--title-to-slug)
;;   ;; (setq org-roam-buffer-width 0.15)
;;   (setq org-roam-tag-sources '(prop last-directory))
;;   ;; (setq org-roam-title-include-subdirs t)
;;   (setq org-roam-capture-templates
;;         '(("d" "default" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+title: ${title}\n#+roam_alias: \n\n"
;;            :unnarrowed t)
;;           ("l" "literature" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+title: ${title}\n\n"
;;            :unnarrowed t)
;;           ("c" "concept" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "concepts/%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+title: ${title}\n#+roam_alias: \n\n"
;;            :unnarrowed t)
;;           ("p" "private" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "private/%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+title: ${title}\n\n"
;;            :unnarrowed t)
;;           ("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+roam_key: ${ref}\n#+roam_tags: website\n#+title: ${title}
;; - source :: ${ref}\n\n"
;;            :unnarrowed t)))

;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
;;            :head "#+roam_key: ${ref}\n#+roam_tags: website\n#+title: ${title}
;; - source :: ${ref}\n\n"
;;            :unnarrowed t))))

;;(use-package! org-roam-bibtex
;;  :requires bibtex-completion
;;  :hook (org-roam-mode . org-roam-bibtex-mode)
;;  :load-path "~/projects/org-roam-bibtex/"
;;  :bind (:map org-roam-bibtex-mode-map
;;         (("C-c m F" . orb-find-non-ref-file))
;;         :map org-mode-map
;;         (("C-c m t" . orb-insert-non-ref)
;;          ("C-c m a" . orb-note-actions)))
;;  :init
;;  :custom
;;  (orb-templates
;;   `(("r" "ref" plain
;;      (function org-roam-capture--get-point)
;;      ""
;;      :file-name "refs/${citekey}"
;;      :head ,(s-join "\n"
;;                     (list
;;                      (concat "#+title: "
;;                              orb-title-format)
;;                      "#+roam_key: ${ref}"
;;                      "#+created: %U"
;;                      "#+last_modified: %U\n\n"))
;;      :unnarrowed t)
;;     ("p" "ref + physical" plain
;;      (function org-roam-capture--get-point)
;;      ""
;;      :file-name "refs/${citekey}"
;;      :head ,(s-join "\n"
;;                     (list
;;                      (concat "#+title: "
;;                              orb-title-format)
;;                      "#+roam_key: ${ref}"
;;                      ""
;;                      "* Notes :physical:")))
;;     ("n" "ref + noter" plain
;;      (function org-roam-capture--get-point)
;;      ""
;;      :file-name "refs/${citekey}"
;;      :head ,(s-join "\n"
;;                     (list
;;                      (concat "#+title: "
;;                              orb-title-format)
;;                      "#+roam_key: ${ref}"
;;                      ""
;;                      "* Notes :noter:"
;;                      ":PROPERTIES:"
;;                      ":NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")"
;;                      ":NOTER_PAGE:"
;;                      ":END:"))))))

;; (use-package! org-roam-server
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-export-inline-images t
;;         org-roam-server-authenticate nil
;;         org-roam-server-label-truncate t
;;         org-roam-server-label-truncate-length 60
;;         org-roam-server-label-wrap-length 20))

;; (use-package! org-ref
;;   :config
;;   (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;         org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;;         org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;         org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"
;;         bibtex-completion-pdf-open-function
;;         (lambda (fpath)
;;           (start-process "open" "*open*" "open" fpath))))

;; bibtex-completion-pdf-open-function 'org-open-file))

;; (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;;       org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;;       org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
