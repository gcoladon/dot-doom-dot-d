(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deft-directory "/Users/greg/org/roam/")
 '(org-agenda-files '("~/org/" "~/org/roam/"))
 '(org-id-link-to-org-use-id t)
 '(org-id-method 'ts)
 '(org-id-ts-format "%Y-%m%d-%H%M%S")
 '(org-log-done 'time)
 '(org-roam-capture-templates
   '(("b" "blank" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y_%m%d>-${slug}" :head ":PROPERTIES:
:ID:       %<%Y_%m%d_%H%M%S>
:END:
#+title: ${title}
" :unnarrowed t)
     ("p" "person" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y_%m%d>-${slug}" :head ":PROPERTIES:
:ID:       %<%Y_%m%d_%H%M%S>
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
     ("w" "weekly" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y_%m%d>-${slug}" :head ":PROPERTIES:
:ID:       %<%Y_%m%d_%H%M%S>
:END:
#+title: ${title}

* Sun Mar 
** Plan
** Meetings
** Notes
* Mon Mar 
** Plan
** Meetings
** Notes
* Tue Mar 
** Plan
** Meetings
** Notes
* Wed Mar 
** Plan
** Meetings
** Notes
* Thu Mar 
** Plan
** Meetings
** Notes
* Fri Mar 
** Plan
** Meetings
** Notes
* Sat Mar 
** Plan
** Meetings
** Notes
" :unnarrowed t)
     ("m" "minimal" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y_%m%d>-${slug}" :head ":PROPERTIES:
:ID:       %<%Y_%m%d_%H%M%S>
:END:
#+title: ${title}

* Tags :: 
* URLs :: 
* Locations :: 
* Notes
- " :unnarrowed t)))
 '(org-roam-db-update-method 'immediate)
 '(org-roam-graph-exclude-matcher '("-weekly" "-exclude"))
 '(org-roam-graph-viewer
   "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
 '(org-roam-index-file "2021_0321-exclude_index.org")
 '(+org-roam-open-buffer-on-find-file nil)
 '(org-startup-folded nil)
 '(projectile-project-search-path "~/src"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
