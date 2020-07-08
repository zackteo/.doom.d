;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Zachary Teo"
      user-mail-address "zackteo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only nede these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-material)

;(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

;; PERSONAL CONFIG

;; Load personalised keybindings
(load! "+exwm")
(load! "+bindings")
(load! "+org_roam")

;; package for nov
(load! "justify-kp")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "en_GB")
 '(ispell-program-name "hunspell")
 '(org-agenda-files
   '("/home/zackteo/Dropbox/org/CS_Diary.org" "/home/zackteo/Dropbox/org/SUTD_Capstone.org" "/home/zackteo/Dropbox/org/Term5.org" "/home/zackteo/Dropbox/org/Test.org" "/home/zackteo/Dropbox/org/The Imitation of Christ [with Biographical Introduction] by Kempis Thomas (z-lib.org).org" "/home/zackteo/Dropbox/org/The Pragmatic Programmer Your Journey to Mastery, 20th Anniversary Edition by Andrew Hunt David Hurst Thomas (z-lib.org).org" "/home/zackteo/Dropbox/org/birthday.org" "/home/zackteo/Dropbox/org/birthday_new.org" "/home/zackteo/Dropbox/org/newgtd.org" "/home/zackteo/Dropbox/org/someday.org" "/home/zackteo/Dropbox/org/weeklyreview.org"))
 '(org-roam-directory "~/Dropbox/org/roam/")
 '(org-roam-index-file "index.org")
 '(package-selected-packages '(org-roam-server)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )