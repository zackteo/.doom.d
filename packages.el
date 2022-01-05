;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! xelb)
(package! exwm)
(package! buffer-move)
(package! exwm-edit)
(package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff" :branch "main"))
(package! exwm-firefox-core :recipe (:host github :repo "walseb/exwm-firefox-core" :branch "master"))

(package! emms)

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot" :branch "master" ))
;; (package! screenshot :pin "c3103f66cc9f7be9d55962ba129248f02586c3d6")

(package! page-break-lines)
(package! clojure-essential-ref-nov)

(package! epresent)

;; (unpin! org-roam)
;; (package! org-roam :recipe (:host github :repo "org-roam/org-roam" :branch "master"))

(package! kibit-helper)                 ; Clojure suggestions with logic

(package! org-roam-server)
(package! org-ref)
(package! ivy-bibtex)
(package! org-roam-bibtex)

(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))

(package! nov)
(package! dired-narrow)

(package! lsp-treemacs)

(package! aggressive-indent)

;; (package! 4clojure)

(package! hiccup-cli)

(package! telega
  :recipe (:host github :repo "zevlg/telega.el"))

(package! multi-vterm)

(package! vulpea :recipe (:host github :repo "d12frosted/vulpea" :branch "master"))

;; (package! ftable :recipe (:host github :repo "casouri/ftable" :branch "master"))
;; (package! valign :recipe (:host github :repo "casouri/valign" :branch "master"))


;; CIDER temp fix
(package! map :pin "bb50dba")
