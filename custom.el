(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+org-roam-open-buffer-on-find-file nil t)
 '(bibtex-completion-additional-search-fields '("keywords" "primaryClass"))
 '(bibtex-completion-bibliography gpc/bib-file)
 '(bibtex-completion-display-formats
   '((t . "${author:20} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:31}")))
 '(bibtex-completion-library-path gpc/pdf-dir)
 '(bibtex-completion-notes-path (concat gpc/org-dir "roam-stem/"))
 '(bibtex-completion-pdf-field "file")
 '(deft-directory gpc/org-dir)
 '(deft-recursive t)
 '(deft-strip-summary-regexp ":PROPERTIES:
\\(.+
\\)+:END:
#\\+title: ")
 '(deft-use-filename-as-title t t)
 '(initial-frame-alist '((top . 1) (left . 1) (width . 150) (height . 40)))
 '(orb-preformat-keywords
   '(("citekey" . "=key=")
     "url" "file" "author-or-editor-abbrev" "keywords"))
 '(orb-switch-persp t)
 '(org-M-RET-may-split-line t)
 '(org-agenda-files gpc/org-agenda-files)
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down user-defined-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
 '(org-id-link-to-org-use-id t)
 '(org-id-locations-file "/Users/greg/org/.orgids")
 '(org-id-method 'ts)
 '(org-id-ts-format "%y%m%d_%H%M%S")
 '(org-insert-heading-respect-content nil)
 '(org-link-elisp-confirm-function 'y-or-n-p)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-noter-always-create-frame nil)
 '(org-noter-doc-split-fraction '(0.6 . 0.6))
 '(org-ref-default-bibliography (list gpc/bib-file))
 '(org-ref-notes-directory (concat gpc/org-dir "roam-stem/"))
 '(org-ref-pdf-directory gpc/pdf-dir)
 '(org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("r" "ref" plain "%?" :if-new
      (file+head "roam-stem/%<%y%m%d_>${slug}.org" "#+title: ${title}")
      :unnarrowed t)))
 '(org-roam-capture-templates
   '(("w" "work blank" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}

")
      :unnarrowed t)
     ("s" "stem blank" plain "%?" :if-new
      (file+head "roam-stem/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}

")
      :unnarrowed t)
     ("h" "home blank" plain "%?" :if-new
      (file+head "roam-personal/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}

")
      :unnarrowed t)
     ("p" "work person" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}

* Company :: 
* LinkedIn :: 
* Title :: 
* Location :: 
* Email :: 
* Phone :: 
* Notes
- ")
      :unnarrowed t)
     ("W" "work weekly" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}
#+TODO: TODO | DONE

%(gpc/gen-weekly monday-tv)
")
      :unnarrowed t)
     ("m" "work minimal" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}

* Tags :: 
* URLs :: 
* Locations :: 
* Notes
- ")
      :unnarrowed t)
     ("n" "ref+noter" plain "%?" :if-new
      (file+head "roam-stem/${citekey}.org" "#+TITLE: ${title}
#+ROAM_KEY: cite:${citekey}
#+ROAM_TAGS:

* ${title}
:PROPERTIES:
:URL: ${url}
:AUTHOR: ${author-or-editor-abbrev}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:
")
      :unnarrowed t)))
 '(org-roam-directory "/Users/greg/org/")
 '(org-roam-graph-edge-extra-config '(("color" . "#333333") ("dir" . "back")))
 '(org-roam-graph-exclude-matcher '("_weekly" "_exclude" "_monthly"))
 '(org-roam-graph-executable "neato")
 '(org-roam-graph-extra-config '(("overlap" . "false")))
 '(org-roam-graph-filetype "pdf")
 '(org-roam-graph-max-title-length 25)
 '(org-roam-graph-shorten-titles 'wrap)
 '(org-startup-folded nil)
 '(pdf-misc-print-program-executable "/usr/local/bin/gpr")
 '(projectile-project-search-path '("~/dl" "~/src"))
 '(save-place-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(org-transclusion-fringe ((t (:background "red"))))
 '(org-transclusion-source-fringe ((t (:background "green")))))
