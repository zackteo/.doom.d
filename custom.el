(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(emms-volume-change-function 'emms-volume-pulse-change)
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "hunspell")
 '(org-agenda-files '("~/Dropbox/org/todo.org"))
 '(org-file-apps
   '((auto-mode . emacs)
     (directory . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("\\.gif\\'" . "
(lambda (file link)
  (let ((my-image (create-image file))
        (tmpbuf (get-buffer-create \"*gif\")))
    (switch-to-buffer tmpbuf)
    (erase-buffer)
    (insert-image my-image)
    (image-animate my-image)))")))
 '(org-roam-directory "~/Dropbox/org/roam/")
 '(org-roam-index-file "index.org")
 '(package-selected-packages '(org-roam-server))
 '(safe-local-variable-values
   '((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")))
 '(warning-suppress-log-types '((emacs) (:warning)))
 '(warning-suppress-types '((emacs) (:warning))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
