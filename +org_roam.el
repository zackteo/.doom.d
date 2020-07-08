;;; ~/.doom.d/+org_roam.el -*- lexical-binding: t; -*-

(defun string-filter (title)
  (let ((common-words '("the" "of" "a" "as"))
        (title-length 3)
        (title-word-list (split-string title "\_")))
    (let ((new-list (seq-remove (lambda (elt) (member elt common-words)) title-word-list)))
      (string-join (butlast new-list (- (length new-list) title-length)) "_"))))

;(string-filter "irresistible_the_rise_of_addictive_technology_and_the_business_of_keeping_us_hooked")

(defun string-filter2 (title)
    (let ((list (split-string title "\_")))
      (string-join (butlast list (- (length list) 3)) "_")))

;        (list (seq-remove (lambda (elt) (member elt (common-words))) title-word-list)))
;    (string-join (butlast list (- (length list) 3)) "_")))

(defun my-cool-title-function (str)
  (string-filter (org-roam--title-to-slug str)))

;(seq-remove (lambda (elt) (member elt '("the" "of" "and")))
;            (split-string "irresistible_the_rise_of_addictive_technology_and_the_business_of_keeping_us_hooked" "\_"))

(use-package! org-roam
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  (org-roam-index-file "index.org")
  :bind (:map org-roam-mode-map
         (("C-c m l" . org-roam)
          ("C-c m f" . org-roam-find-file)
          ("C-c m r" . org-roam-find-ref)
          ("C-c m d" . org-roam-find-directory)
          ("C-c m j" . org-roam-jump-to-index)
          ("C-c m b" . org-roam-switch-to-buffer)
          ("C-c m g" . org-roam-graph)
          ("C-c m c" . org-roam-capture))
         :map org-mode-map
         (("C-c m i" . org-roam-insert)))
  :config
  ;(setq org-roam-title-to-slug-function  #'my-cool-title-function)
  ;(setq org-roam-title-to-slug-function  #'org-roam--title-to-slug)
  (setq org-roam-tag-sources '(prop last-directory))
  (setq org-roam-title-include-subdirs t)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_alias: \n\n"
           :unnarrowed t)
          ("l" "literature" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+roam_alias: \n\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n\n"
           :unnarrowed t)
          ("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+roam_key: ${ref}\n#+roam_tags: website\n#+title: ${title}
- source :: ${ref}\n\n"
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "literature/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+roam_key: ${ref}\n#+roam_tags: website\n#+title: ${title}
- source :: ${ref}\n\n"
           :unnarrowed t))))

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

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(use-package! org-ref
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "open" "*open*" "open" fpath))))
        ;bibtex-completion-pdf-open-function 'org-open-file))

(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
