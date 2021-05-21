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

;; https://org-roam.discourse.group/t/doom-emacs-how-to-upgrade-org-roam/764/9
;; I'm hoping that this gets me the new node-based org-roam features.
;; (unpin! org-roam company-org-roam)
;; (package! org-roam :pin "9b0a45f105a59719e8810bbde236d3d1d4a1c57d")

;; this didn't get me the v2 code!
;; (package! org-roam :recipe (:branch "v2"))
;; (package! google-this)

;; Try to figure out how to add a binding to google-this inside the
;; search keybinding prefix, which is impemented here:
;; /Users/greg/.emacs.d/modules/config/default/+emacs-bindings.el
;; (define-key [doom-prefix-map] (kbd "c") 'avy-goto-char)

;; -*- no-byte-compile: t; -*-
;; (package! ctrlf)
;;(package! dired-narrow)
;; (package! deadgrep)
;;(package! easy-kill)
;;(package! org-clock-convenience)
;;(package! company-posframe)
;;(package! org-roam
;;  :recipe (:host github :repo "org-roam/org-roam"))
;;(package! org-roam
;;  :recipe (:host github :repo "org-roam/org-roam" :branch "v2"))

(unpin! org-roam)
;;(package! org-roam
;;  :recipe (:host github :repo "gcoladon/org-roam"))

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; doing this because of: https://github.com/org-roam/org-roam-bibtex
;; When using org-roam via the `+roam` flag

;; why do neither of these work?
;; (unpin! org-mode)
;; (unpin! org)



;; When using bibtex-completion via the `biblio` module
(unpin! bibtex-completion helm-bibtex ivy-bibtex)


(package! pandoc)

;;(package! mathpix.el
;;  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! anki-editor)
;;  I don't know if this is needed or redundant. I'm thinking redundant.

;; this didn't work, trying the above
;; (after! org
;;   (package! ox-rst))

;;(package! gif-screencast
;;  :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
;;(package! modus-operandi-theme)
;;(package! outshine)
;;(package! org-download)
;;(package! ox-hugo
;;  :recipe (:host github :repo "jethrokuan/ox-hugo"))
;;(package! ox-texinfo+
;;  :recipe (:host github :repo "tarsius/ox-texinfo-plus"))
;;(package! nov
;;  :recipe (:type git :repo "https://depp.brause.cc/nov.el.git"))
;;(package! git-link)
;;(package! yaml-mode)
(package! org-roam-server
  :recipe (:host github :repo "org-roam/org-roam-server"))
;;(package! emmet-mode)
;;(package! citeproc-org)

(after! org-roam
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode 1))

(package! ox-rst
  :recipe (:host github :repo "msnoigrs/ox-rst"))

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))

(after! org-tranclusion
  ;; (add-to-list 'load-path "path/to/org-transclusion/")
  (require 'org-transclusion)
  ;; (define-key global-map (kbd "<f12>") #'org-transclusion-add-at-point)
  )
