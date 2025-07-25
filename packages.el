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

(package! pandoc)
;;(package! org-download)
;;(package! ox-hugo
;;  :recipe (:host github :repo "jethrokuan/ox-hugo"))

;; (package! org-roam
;;   :recipe (:host github :repo "org-roam/org-roam" :branch "master"))

;; (package! org-roam
;;   :recipe (:host github :repo "gcoladon/org-roam" :branch "v2"))

(package! ox-rst
  :recipe (:host github :repo "msnoigrs/ox-rst"))

;; (use-package rainbow-fart
;;   :straight (rainbow-fart :type git
;;                           :host github
;;                           :repo "DogLooksGood/rainbow-fart.el"
;;                           :files ("*.el" "voice")))

;; Try to get this working some time
;; (package! rainbow-fart
;;   :recipe (:host github :repo "DogLooksGood/rainbow-fart.el"))

;; :recipe (:host github :repo "org-roam/org-roam-bibtex" :branch "org-roam-v2"))

;; (package! helm-bibtex)

;; (package! org-transclusion
;;   :recipe (:host github :repo "nobiot/org-transclusion"))

;; (package! org-noter
;;   :recipe (:host github :repo "c1-g/org-noter-plus-djvu"))

(package! command-log-mode)

(package! jq-mode
  :recipe (:host github :repo "ljos/jq-mode"))

;; (unpin! org-roam)
;; removing this unpin on jul 21 2024, to see if that helps 

(package! org-roam-ui)

;; (package! takenote :recipe (:host github :repo "gcoladon/takenote"))

(package! org-tidy)

;; After seeing a demo at BALISP at Hacker Dojo, I want to try this out myself
(package! shell-maker
  :recipe (:host github :repo "xenodium/shell-maker"))
(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))
(package! ob-chatgpt-shell
  :recipe (:host github :repo "xenodium/ob-chatgpt-shell"))
