(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+org-roam-open-buffer-on-find-file nil t)
 '(anki-editor-org-tags-as-anki-tags nil)
 '(bibtex-completion-additional-search-fields '("keywords" "primaryClass"))
 '(bibtex-completion-display-formats
   '((t . "${author:20} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:31}")))
 '(bibtex-completion-pdf-field "file")
 '(deft-directory org-directory)
 '(deft-recursive t)
 '(deft-strip-summary-regexp ":PROPERTIES:\12\\(.+\12\\)+:END:\12#\\+title: ")
 '(deft-use-filename-as-title t t)
 '(helm-case-fold-search t)
 '(initial-frame-alist '((top . 1) (left . 1) (width . 150) (height . 40)))
 '(orb-preformat-keywords
   '(("citekey" . "=key=")
     "url" "file" "author-or-editor-abbrev" "keywords" "abstract" "author" "year"))
 '(orb-switch-persp t)
 '(org-M-RET-may-split-line t)
 '(org-agenda-file-regexp "\\`[^.].*monthly.*\\.org\\'")
 '(org-agenda-files gpc/org-agenda-files)
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down user-defined-down category-keep)
     (tags priority-down category-keep)
     (search category-keep)))
 '(org-capture-templates
   '(("C" "Chin up log" table-line
      (file+olp "~/org/roam/roam-stem/230621_pull_ups.org" "Goggins routine")
      "| %t | %^{reps} | %^{sets} | " :immediate-finish t :table-line-pos "I+1")
     ("P" "Push up log" table-line
      (file+olp "~/org/roam/roam-stem/230606_push_ups.org" "Goggins routine")
      "| %t | %^{reps} | %^{sets} | " :immediate-finish t :table-line-pos "I+1")
     ("L" "Lifting log" table-line
      (file+olp "~/org/roam/roam-personal/230324_powerlifting.org" "Logbook")
      "| %<%y-%m %b %a %0e> | %^{Lift|Press|Dead|Squat|Bench} | %^{1st} | %^{2nd} | %^{3rd} | %^{4th} | %^{5th} | %^{6th} | %^{Note} |" :table-line-pos "I+1")
     ("H" "Health journal" entry
      (file+olp+datetree gpc/health-journal-file)
      "* %U %?\12%i" :prepend t)
     ("T" "Tesla FSD journal" entry
      (file+olp+datetree "~/org/roam/roam-stem/250510_tesla_fsd_journal.org")
      "* %U %?\12%i" :prepend t)
     ("l" "Love journal" entry
      (file+olp+datetree gpc/love-journal-file)
      "* %U %?\12%i" :prepend t)
     ("I" "Idea" plain #'gpc/place-idea-in-notes-file "** IDEA %^{Idea}" :immediate-finish t)
     ("t" "Personal todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* [ ] %?\12%i\12%a" :prepend t)
     ("n" "Personal notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\12%i\12%a" :prepend t)
     ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\12%i" :prepend t)
     ("p" "Templates for projects")
     ("pt" "Project-local todo" entry
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\12%i\12%a" :prepend t)
     ("pn" "Project-local notes" entry
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\12%i\12%a" :prepend t)
     ("pc" "Project-local changelog" entry
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\12%i\12%a" :prepend t)
     ("o" "Centralized templates for projects")
     ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\12 %i\12 %a" :heading "Tasks" :prepend nil)
     ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\12 %i\12 %a" :prepend t :heading "Notes")
     ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\12 %i\12 %a" :prepend t :heading "Changelog")))
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
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
 '(org-ref-notes-directory (concat org-directory "roam-stem/"))
 '(org-refile-targets '((nil :maxlevel . 2) (org-agenda-files :maxlevel . 2)))
 '(org-reverse-note-order t)
 '(org-roam-capture-ref-templates
   '(("r" "ref" plain "%?" :if-new
      (file+head "roam-stem/%<%y%m%d_>${slug}.org" "#+title: ${title}")
      :unnarrowed t)))
 '(org-roam-capture-templates
   '(("w" "work" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}\12\12")
      :unnarrowed t)
     ("s" "stem" plain "%?" :if-new
      (file+head "roam-stem/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}\12#+TODO: TODO | DONE\12\12")
      :unnarrowed t)
     ("p" "personal" plain "%?" :if-new
      (file+head "roam-personal/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}\12#+TODO: TODO | DONE\12\12")
      :unnarrowed t)
     ("W" "work weekly" plain "%?" :if-new
      (file+head "roam-pilot/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}\12#+TODO: TODO WAIT | DONE\12\12%(gpc/gen-weekly monday-tv)\12")
      :unnarrowed t)
     ("P" "personal monthly" plain "%?" :if-new
      (file+head "roam-personal/%<%y%m%d_>${slug}.org" "#+TITLE: ${title}\12#+TODO: IDEA TODO NOW BLOCKED | DONE\12\12%(gpc/gen-monthly)\12")
      :unnarrowed t)
     ("n" "ref+noter" plain "%?" :if-new
      (file+head "roam-stem/${citekey}.org" "#+TITLE: %(car (split-string \"${author}\" \",\")) '%(substring \"${year}\" 2 4) - ${title}\12#+ROAM_KEY: cite:${citekey}\12#+ROAM_TAGS:\12\12* %(car (split-string \"${author}\" \",\")) '%(substring \"${year}\" 2 4) - ${title}\12:PROPERTIES:\12:URL: ${url}\12:AUTHOR: ${author-or-editor-abbrev}\12:NOTER_DOCUMENT: ${file}\12:NOTER_PAGE:\12:END:\12** Abstract\12${abstract}\12")
      :unnarrowed t)))
 '(org-roam-graph-edge-extra-config '(("color" . "#333333") ("dir" . "back")))
 '(org-roam-graph-exclude-matcher '("_weekly" "_exclude" "_monthly"))
 '(org-roam-graph-executable "neato")
 '(org-roam-graph-extra-config '(("overlap" . "false")))
 '(org-roam-graph-filetype "pdf")
 '(org-roam-graph-max-title-length 25)
 '(org-roam-graph-shorten-titles 'wrap)
 '(org-startup-folded nil)
 '(package-selected-packages '(geiser-mit nov impatient-mode))
 '(pdf-misc-print-program-executable "/usr/local/bin/gpr")
 '(projectile-project-search-path '("~/dl" "~/src"))
 '(save-place-mode nil)
 '(send-mail-function 'mailclient-send-it)
 '(warning-suppress-types '((org-element-cache) (org-element-cache))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(org-transclusion-fringe ((t (:background "red"))))
 '(org-transclusion-source-fringe ((t (:background "green")))))
