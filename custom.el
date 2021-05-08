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
 '(bibtex-completion-notes-path gpc/org-dir)
 '(bibtex-completion-pdf-field "file")
 '(deft-directory gpc/org-dir)
 '(initial-frame-alist '((top . 1) (left . 1) (width . 150) (height . 40)))
 '(orb-preformat-keywords
   '(("citekey" . "=key=")
     "url" "file" "author-or-editor-abbrev" "keywords"))
 '(orb-switch-persp t)
 '(orb-templates
   '(("n" "ref+noter" plain #'org-roam-capture--get-point "" :file-name "roam-stem/${slug}" :head "#+TITLE: ${title}
#+ROAM_KEY: cite:${citekey}
#+ROAM_TAGS:

* ${title} %?
:PROPERTIES:
:ID: %<%y%m%d_%H%M%S>
:URL: ${url}
:AUTHOR: ${author-or-editor-abbrev}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:
")))
 '(org-M-RET-may-split-line t)
 '(org-agenda-files (list gpc/org-dir))
 '(org-id-link-to-org-use-id t)
 '(org-id-method 'ts)
 '(org-id-ts-format "%y%m%d_%H%M%S")
 '(org-link-elisp-confirm-function 'y-or-n-p)
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-noter-always-create-frame nil)
 '(org-noter-doc-split-fraction '(0.6 . 0.6))
 '(org-ref-default-bibliography (list gpc/bib-file))
 '(org-ref-notes-directory gpc/org-dir)
 '(org-ref-pdf-directory gpc/pdf-dir)
 '(org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))
 '(org-reverse-note-order t)
 '(org-roam-capture-templates
   '(("w" "blank work" plain #'org-roam-capture--get-point "%?" :file-name "roam-pilot/${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}
" :unnarrowed t)
     ("s" "blank stem" plain #'org-roam-capture--get-point "%?" :file-name "roam-stem/${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}
" :unnarrowed t)
     ("h" "blank home" plain #'org-roam-capture--get-point "%?" :file-name "roam-personal/${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}
" :unnarrowed t)
     ("p" "person" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}

* Company :: 
* LinkedIn :: 
* Title :: 
* Location :: 
* Email :: 
* Phone :: 
* Notes
- " :unnarrowed t)
     ("w" "weekly" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}
#+TODO: TODO | DONE
%?
%(gpc/gen-weekly monday-tv)
" :unnarrowed t)
     ("m" "minimal" plain #'org-roam-capture--get-point "%?" :file-name "${slug}" :head ":PROPERTIES:
:ID:       %<%y%m%d_%H%M%S>
:END:
#+title: ${title}

* Tags :: 
* URLs :: 
* Locations :: 
* Notes
- " :unnarrowed t)))
 '(org-roam-db-update-method 'immediate)
 '(org-roam-graph-edge-extra-config '(("color" . "#333333") ("dir" . "back")))
 '(org-roam-graph-exclude-matcher '("_weekly" "_exclude"))
 '(org-roam-graph-executable "neato")
 '(org-roam-graph-extra-config '(("overlap" . "false")))
 '(org-roam-graph-filetype "pdf")
 '(org-roam-graph-max-title-length 25)
 '(org-roam-graph-shorten-titles 'wrap)
 '(org-startup-folded nil)
 '(pdf-misc-print-program-executable "/usr/local/bin/gpr")
 '(projectile-project-search-path "~/src"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
