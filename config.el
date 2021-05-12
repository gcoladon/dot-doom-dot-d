;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Greg Coladonato")

(if (equal (replace-regexp-in-string "[\t|\n]" ""
                                  (shell-command-to-string "ifconfig en0 | grep ether"))
           "ether f0:18:98:9a:c9:2c ")
    (setq gpc/email "gcoladon@gmail.com"
          gpc/org-dir "~/org-roam/"
          gpc/org-agenda-files (list (concat gpc/org-dir "roam/roam-personal/")
                                     (concat gpc/org-dir "roam/roam-stem/"))
          gpc/pdf-dir "~/pdfs"
          ;; gpc/roam-dir "~/"
          ;; gpc/roam-dir "~/roam/"
          gpc/bib-file "~/dev/org/references.bib"
          org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")

  (setq gpc/email "greg@pilot.ai"
        gpc/org-dir "~/org/"
        gpc/org-agenda-files (list (concat gpc/org-dir "roam/roam-pilot/")
                                   (concat gpc/org-dir "roam/roam-stem/"))
        gpc/pdf-dir "~/pdfs"
        ;; gpc/roam-dir "~/org/roam"
        gpc/bib-file "~/org/references.bib"
        org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

(setq user-mail-address gpc/email
      org-directory gpc/org-dir)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; greg: apparently commenting this out would speed things up
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
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;;; I think I like the look of this.
;;; And I *almost* like how it works, but it changes point to the
;;; buffers instead of staying in the dired. Hmm, how to fix that?
;;; https://org-roam.discourse.group/t/org-roam-dailies-how-to-browse-through-previous-days/1018
;;; Dired
(defun my/check-org-roam-buffer-p (buf)
  "Return non-nil if BUF is org-roam-buffer that can be refleshed.
It also checks the following:
- `org-roam' is indeed loaded
- BUF is visiting an Org-roam file
- org-roam-buffer exists"
  (and (functionp #'org-roam--org-roam-file-p)
       (org-roam--org-roam-file-p (buffer-file-name buf))
       (not (eq (org-roam-buffer--visibility) 'none))))

;;;###autoload
(defun my/dired-display-next-file ()
  "In Dired directory, go to the next file, and open it.
            If `org-roam-mode' is active, update the org-roam-buffer."
  (interactive)
  (dired-next-line 1)
  (let ((buf (find-file-noselect (dired-get-file-for-visit))))
    (display-buffer buf t)
    (when (my/check-org-roam-buffer-p buf)
      (with-current-buffer buf
        (setq org-roam--current-buffer buf)
        (org-roam-buffer-update)))))

;;;###autoload
(defun my/dired-display-prev-file ()
  "In Dired directory, go to the previous file, and open it.
            If `org-roam-mode' is active, update the org-roam-buffer."
  (interactive)
  (dired-previous-line 1)
  (let ((buf (find-file-noselect (dired-get-file-for-visit))))
    (display-buffer buf t)
    (when (my/check-org-roam-buffer-p buf)
      (with-current-buffer buf
        (setq org-roam--current-buffer buf)
        (org-roam-buffer-update)))))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)
            ;; I don't use dired-subtree at the moment
            ;;(define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
            ;;(define-key dired-mode-map (kbd "<C-tab>") #'dired-subtree-cycle)
            (define-key dired-mode-map (kbd "<SPC>") #'my/dired-display-next-file)
            (define-key dired-mode-map (kbd "<down>") #'my/dired-display-next-file)
            (define-key dired-mode-map (kbd "<up>") #'my/dired-display-prev-file)))

(defun my/dired-in-side-buffer ()
  "Display Dired in a side window."
  (interactive)
  (let* ((dir (read-directory-name "Directory: "))
         (buf (dired-noselect dir)))
    (select-window
     (display-buffer-in-side-window buf
                                   '((side . left)
                                     (window-width . 30)
                                     (slot . -1)
                                     (window-parameters . ((mode-line-format . none))))))))

;; Let's see if I prefer this style of search interaction
;; (ctrlf-mode +1)

;; https://sachachua.com/blog/2015/08/org-mode-date-arithmetic/
;; Thanks Sacha!
(defun gc/org-roam-find-weekly (which)
  "Find file corresponding to the week beginning with WHICH"
  (setq monday-tv (org-read-date nil t which))
  (let ((monday-str (org-read-date nil nil which)))
    (org-roam-find-file
     (concat "Week of " monday-str)) nil nil t))

(defun gc/org-roam-monthly ()
  "Find the monthly-file for this month."
  (interactive)
  (setq
   next-first (org-read-date nil t "1")
   first-tv (org-read-date nil t "--m" nil next-first)
   first-str (org-read-date nil nil "--m" nil next-first))
  (org-roam-find-file (concat "Month of " (substring first-str 0 7))))

(defun gc/org-roam-weekly-this ()
  "Find the weekly-file for this week."
  (interactive)
  (gc/org-roam-find-weekly "-mon"))

(defun gc/org-roam-weekly-last ()
  "Find the weekly-file for last week."
  (interactive)
  (gc/org-roam-find-weekly "-2mon"))

(defun gc/org-roam-weekly-next ()
  "Find the weekly-file for next week."
  (interactive)
  (gc/org-roam-find-weekly "+mon"))

(defun gc/org-roam-weekly-next-next ()
  "Find the weekly-file for next week."
  (interactive)
  (gc/org-roam-find-weekly "+2mon"))

(defun gc/org-roam-graph-1 ()
  "Generate a graph that goes out a distance of one edge"
  (interactive)
  (org-roam-graph 1))

(defun gc/org-roam-graph-2 ()
  "Generate a graph that goes out a distance of two edges"
  (interactive)
  (org-roam-graph 2))

(defun gc/org-roam-graph-3 ()
  "Generate a graph that goes out a distance of three edges"
  (interactive)
  (org-roam-graph 3))

(defun gc/org-roam-graph-4 ()
  "Generate a graph that goes out a distance of four edges"
  (interactive)
  (org-roam-graph 4))

(defun gc/org-roam-graph-5 ()
  "Generate a graph that goes out a distance of five edges"
  (interactive)
  (org-roam-graph 5))


;; Why not, I use these functions all the time, a single chord makes sense
(global-set-key (kbd "s-r") 'org-roam-find-file)
(global-set-key (kbd "s-i") 'org-roam-insert)
(global-set-key (kbd "s-t") 'gc/org-roam-weekly-this)

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if buffer has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "\n *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display " -P-\n")
        ;;(overlay-put ov_this 'invisible t)
        ;; (overlay-put ov_this 'display (format "\n"))
        (overlay-put ov_this 'hidden-prop-drawer t)))))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (progn
        (org-show-properties)
        (put 'org-toggle-properties-hide-state 'state 'shown))
    (progn
      (org-hide-properties)
      (put 'org-toggle-properties-hide-state 'state 'hidden))))

;; Try to figure out where to put this so that it doesn't
;; get called for my org-files.
;; https://emacs.stackexchange.com/questions/14438/remove-hooks-for-specific-modes
;; But this didn't work did it!
;; (remove-hook 'before-save-hook 'ws-butler-before-save)
;; this might work though
;; (setq before-save-hook nil)

(map! :leader
      :desc "Toggle debug-on-error"       "t d" #'toggle-debug-on-error
      :desc "Toggle Frame Min/Max"        "t m" #'toggle-frame-maximized
      :desc "Toggle truncate lines"       "t t" #'toggle-truncate-lines
      :desc "Toggle overwrite mode"       "t o" #'overwrite-mode
      :desc "org-forward-heading-same-level" "n C-f" #'org-forward-heading-same-level
      :desc "Go to Greg's notes.org"      "n g" (lambda () (interactive) (find-file "~/dev/org/notes.org"))
      :desc "HTML-to-Org"                 "n h" (lambda () (interactive) (html2org-clipboard))
      ;; :desc "Search the elisp index"      "s e" (lambda () (interactive) (elisp-index-search))
      :desc "Bury buffer"                 "w y" #'bury-buffer)

(map! :leader
      ;;; <leader> r --- roam
      (:prefix-map ("r" . "gc/roam bindings")
       :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
       :desc "Capture"                       "c" #'org-roam-capture
       :desc "Find file"                     "f" #'org-roam-find-file
       :desc "Show graph"                    "g" #'org-roam-graph
       :desc "Insert"                        "i" #'org-roam-insert
       :desc "Insert last stored link"       "s" #'org-insert-last-stored-link
       :desc "Insert (skipping org-cap)"     "I" #'org-roam-insert-immediate
       :desc "Jump to Index"                 "j" #'org-roam-jump-to-index
       :desc "Toggle property visibility"    "p" #'org-toggle-properties

       :desc "gpc/get-arxiv"                 "x" #'gpc/get-arxiv
       :desc "doi-utils-get-bibtex-entry-pdf" "d" #'doi-utils-get-bibtex-entry-pdf

       :desc "orb-note-actions"              "a" #'orb-note-actions
       :desc "org-noter-create-skeleton"     "k" #'org-noter-create-skeleton
       :desc "org-noter"                     "n" #'org-noter
       :desc "helm-bibtex"                   "h" #'helm-bibtex
       :desc "org-ref-helm-insert-cite"      "H" #'helm-bibtex-with-notes

       :desc "This Monthly"                  "m" #'gc/org-roam-monthly
       :desc "This Weekly"                   "t" #'gc/org-roam-weekly-this
       :desc "Last Weekly"                   "l" #'gc/org-roam-weekly-last
       :desc "Next Weekly"                   "w" #'gc/org-roam-weekly-next
       :desc "Next-Next Weekly"              "W" #'gc/org-roam-weekly-next

       :desc "Graph of 1"                    "1" #'gc/org-roam-graph-1
       :desc "Graph of 2"                    "2" #'gc/org-roam-graph-2
       :desc "Graph of 3"                    "3" #'gc/org-roam-graph-3
       :desc "Graph of 4"                    "4" #'gc/org-roam-graph-4
       :desc "Graph of 5"                    "5" #'gc/org-roam-graph-5)
      )

;;(doom/increase-font-size 1)

;; following instructions from https://github.com/org-roam/org-roam-bibtex
(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (bibtex-set-dialect 'BibTeX)
  (helm-add-action-to-source
   "Edit org-noter Notes"
   'helm-bibtex-edit-notes
   helm-source-bibtex
   0))

(defun bibtex-autokey-wrapper (orig-fun &rest args)
    "Dynamically bind `bibtex-autokey-prefix-string' to current date."
    (let ((result
           (let ((bibtex-autokey-prefix-string (format-time-string "%y%m%d_")))
             (apply orig-fun args))))
      (substring result 0 (min 30 (length result)))))

  (advice-add 'bibtex-generate-autokey :around #'bibtex-autokey-wrapper)

  (defun gpc/orb-find-file-open-noter (file)
    (find-file file)
    (outline-next-visible-heading 1)
    (org-noter)
    (other-window 1)
    (org-roam)
    (outline-next-visible-heading -1))
  (defun gpc/orb-edit-notes (orig-fn &rest args)
    (let ((org-roam-find-file-function #'gpc/orb-find-file-open-noter))
      (apply orig-fn args)))
  (advice-add 'orb-edit-notes :around #'gpc/orb-edit-notes)

(defun gpc/mon_day ()
  (format-time-string "%m %d"))

(defun gpc/orb-my-slug (orig-fn title)
    "My version of Roam's title-to-slug to prefix data and chop legth"
    (let* ((today (format-time-string "%y%m%d"))
           (prefix (substring title 0 (min 6 (length title))))
           (prefixed_title (if (equal today prefix)
                               title
                             (concat today " " title)))
           (result (apply orig-fn (list prefixed_title)))
           (subs (substring result 0 (min 30 (length result)))))
      subs))
(advice-add 'org-roam--title-to-slug :around #'gpc/orb-my-slug)

 ;; (use-package! org-noter
 ;;    :after (:any org pdf-view)
 ;;    :config
 ;;    (setq org-noter-always-create-frame nil
 ;;          org-noter-auto-save-last-location t)
 ;;    (defun org-noter-init-pdf-view ()
 ;;      (pdf-view-fit-width-to-window))
 ;;      ;; (pdf-view-auto-slice-minor-mode)
 ;;    (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view))

(use-package! org-noter
    :after pdf-view
    :config
    (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window))

(defun gpc/get-arxiv ()
  "Use the defaults for all three variables, don't ask me!!"
  (interactive)
  (arxiv-get-pdf-add-bibtex-entry (arxiv-maybe-arxiv-id-from-current-kill)
                                  (car org-ref-default-bibliography)
                                  (concat org-ref-pdf-directory "/")))

(defun html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (setq cmd "osascript -e 'the clipboard as \"HTML\"' | perl -ne 'print chr foreach unpack(\"C*\",pack(\"H*\",substr($_,11,-3)))' | pandoc -f html -t json | pandoc -f json -t org --wrap=none")
  (kill-new (shell-command-to-string cmd))
  (yank))

(defun gpc/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))) "/DONE" 'file))

(setq org-archive-location (concat org-directory "/archive.org::datetree/"))


;; Check out all of Doom's templates and see if they are better than mine

  ;; (setq org-capture-templates
  ;;       '(("t" "Todo" entry (file+headline "" "Inbox")
  ;;          "* TODO %?\n  %i" :prepend t)
  ;;         ("T" "Todo w/backlink" entry (file+headline "" "Inbox")
  ;;          "* TODO %?\n  %i\n  %a" :prepend t)
  ;;         ("j" "Journal" entry (file+olp+datetree "journal.org" "Journal")
  ;;          "* %?\nEntered on %U\n  %i\n  %a")))


;; I think I want to install these. Is there a shortcut?
;; (use-package org-web-tools)
;; (setq org-web-tools-pandoc-sleep-time 1.0)
;; (global-set-key (kbd "s-w") 'org-web-tools-insert-web-page-as-entry)

;; Use this next time I need to write a real work memo

;; (add-to-list 'org-latex-classes
;;              '("memo"
;;                "\\documentclass[letterpaper,11pt]{texMemo}"
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq calc-gnuplot-default-device "qt")

;; I would like to figure out how to make this work again
;;
;;  (use-package popup)
  ;; (use-package google-translate
  ;;   :demand t
  ;;   :init
  ;;   (require 'google-translate)

  ;;   :functions (my-google-translate-at-point google-translate--search-tkk)
  ;;   :custom
  ;;   (google-translate-backend-method 'curl)
  ;;   :config
  ;;   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  ;;   (defun my-google-translate-at-point()
  ;;     "reverse translate if prefix"
  ;;     (interactive)
  ;;     (if current-prefix-arg
  ;;         (google-translate-at-point)
  ;;       (google-translate-at-point-reverse)))
  ;;   (setq google-translate-default-source-language "en"
  ;;         google-translate-default-target-language "it"))

  (defun gpc/fetch-tasks ()
    "Run the extractor from Sheets, Open the file, and also open the Sheet"
    (interactive)
    (shell-command "~/bin/tasks.sh")
    (find-file "~/dev/org/incoming.org")
    (browse-url "https://docs.google.com/spreadsheets/d/1T8qe2m4z9ViJ9W72PJb8COCmpptr5D5312vM6e32iPM/edit#gid=0"))

  (defun gpc/org-table-goto-beginning ()
    (interactive)
    (if (org-at-table-p)
        (goto-char (org-table-begin))
      (message "Can't go to beginning of table if not inside a table")))

  (defun gpc/org-table-goto-end ()
    (interactive)
    (if (org-at-table-p)
        (goto-char (org-table-end))
      (message "Can't go to end of table if not inside a table")))

  ;; (define-key org-mode-map (kbd "C-c C-g a") #'gpc/org-table-goto-beginning)
  ;; (define-key org-mode-map (kbd "C-c C-g e") #'gpc/org-table-goto-end)

  (defun gpc/clean-slate ()
    "Go to the top of the page and open just a bit."
    (interactive)
    (beginning-of-buffer)
    (org-shifttab 1)
    )

;;; ucs-cmds.el --- Create commands to insert Unicode chars. -*- lexical-binding:t -*-
;;
;; Filename: ucs-cmds.el
;; Description: Commands to create commands that insert Unicode chars.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2018, Drew Adams, all rights reserved.
;; Created: Tue Oct  4 07:32:20 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Feb 14 13:23:19 2018 (-0800)
;;           By: dradams
;;     Update #: 327
;; URL: https://www.emacswiki.org/emacs/download/ucs-cmds.el
;; Doc URL: https://www.emacswiki.org/emacs/UnicodeEncoding
;; Keywords: unicode, characters, encoding, commands, ucs-names
;; Compatibility: GNU Emacs: 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library defines three commands:
;;
;;  * `ucsc-define-char-insert-cmd' - Define a command to insert a
;;                                    Unicode character.
;;
;;  * `ucsc-make-commands' - Define such commands for all Unicode
;;                           chars whose names match a regexp.
;;
;;  * `ucsc-insert' - Insert a Unicode character and possibly define a
;;                    command to insert it.
;;
;;  You can also use `ucsc-define-char-insert-cmd' or
;;  `ucsc-make-commands' in Emacs-Lisp code (such as in your init
;;  file) to define character-inserting commands.
;;
;;  * The names of the character-inserting commands created are the
;;    same as the char names, except that they are lowercase and any
;;    `SPC' chars in the character name are replaced by hyphens (`-').
;;
;;  * You can use a numeric prefix argument with a character-inserting
;;    command to insert multiple copies of the given character.
;;
;;  The character-inserting commands are tailor-made to insert a given
;;  Unicode character.  You can bind such a command to a key sequence,
;;  effectively adding Unicode characters to your keyboard.
;;
;;  Command `ucsc-insert' is a replacement for vanilla command
;;  `insert-char' (called `ucs-insert' prior to Emacs 24), which Emacs
;;  binds by default to `C-x 8 RET' and which lets you type input to
;;  complete against a Unicode character name and then inserts that
;;  character.
;;
;;  The behavior and code of `ucsc-insert' are identical to those of
;;  `insert-char' (`ucs-insert') except for what happens when you use
;;  a negative prefix argument:
;;
;;  1. It acts as if the prefix-arg value was positive.  So a value of
;;     -3 inserts three copies of the character, just as 3 does.
;;
;;  2. In addition to inserting the character, it uses
;;     `ucsc-define-char-insert-cmd' to define a command that you can
;;     use thereafter to insert that character.
;;
;;  Whenever `insert-char' does anything (it does nothing for a
;;  negative prefix arg), `ucsc-insert' does the same thing.  Because
;;  of this, I recommend that you bind `ucsc-insert' to `C-x 8 RET' as
;;  a replacement for `insert-char'.  Put this in your init file:
;;
;;    (define-key global-map [remap insert-char] 'ucsc-insert)
;;
;;  If you need only a few such commands for inserting particular
;;  Unicode characters, and you do not want to look up their code
;;  points, then using `ucsc-insert' or `ucsc-define-char-insert-cmd'
;;  to define them interactively is sufficiently convenient.  But
;;  these commands, like `insert-char', can be a bit slow if you use
;;  completion, because there are many, *MANY* completion candidates.
;;
;;  You can use `ucsc-make-commands' to quickly create a whole set of
;;  such commands for characters whose names are similar.  The list of
;;  commands (symbols) is returned.
;;
;;  You provide a regexp as the argument to `ucsc-make-commands'.  It
;;  is matched against all Unicode character names (in `ucs-names').
;;  An insertion command is created for each of the characters whose
;;  name matches.
;;
;;  For example, here are some tests to try.  You need a Unicode font.
;;  One of these fonts might help:
;;
;;   (set-frame-font "DejaVu Sans Mono-10")
;;   (set-frame-font "DejaVu Sans 10")
;;   (set-frame-font "Arial Unicode MS")
;;
;;  Sample command creations:
;;
;;   (ucsc-make-commands "^math") ; Math symbols
;;   (ucsc-make-commands "latin") ; Latin alphabet characters
;;   (ucsc-make-commands "arabic")
;;   (ucsc-make-commands "^cjk")  ; Chinese, Japanese, Korean characters
;;   (ucsc-make-commands "^box drawings ")
;;   (ucsc-make-commands "^greek [a-z]+ letter") ; Greek characters
;;   (ucsc-make-commands "\\(^hangul\\|^circled hangul\\|^parenthesized hangul\\)")
;;
;;
;;  Icicles Can Help
;;  ----------------
;;
;;  Use of the commands created using `ucsc-define-char-insert-cmd',
;;  `ucsc-make-commands', and `ucsc-insert' is enhanced by `Icicles'
;;  (https://www.emacswiki.org/emacs/download/icicles.el).
;;
;;  When you enter the command name or code point interactively, you
;;  can take advantage of the more powerful completion of `Icicles',
;;  including regexp, substring (a subset of regexp), and various
;;  kinds of fuzzy matching.
;;
;;  More importantly, you can use progressive completion, to match
;;  parts of a candidate name in any order.  And you can "chip away at
;;  the non-elephant", removing whole sets of candidates that match
;;  patterns that you are *not* interested in.
;;
;;
;;
;;  Commands defined here:
;;
;;    `ucsc-define-char-insert-cmd', `ucsc-insert',
;;    `ucsc-make-commands'.
;;
;;  Non-interactive functions defined here:
;;
;;    `ucsc-char-name', `ucsc-char-names', `ucsc-get-a-hash-key',
;;    `ucsc-get-hash-keys'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/02/14 dadams
;;     Adapted to Emacs 26, where ucs-names is a hash table.
;;       Use lexical-binding.
;;       Added: ucsc-char-name, ucsc-char-names, ucsc-get-a-hash-key, ucsc-get-hash-keys.
;;       ucsc-define-char-insert-cmd: Use ucsc-char-name.
;;       ucsc-make-commands: Handle hash-table-p case for ucs-names.
;; 2016/11/18 dadams
;;     ucsc-make-commands: Changed from a macro to a command that returns the commands.
;; 2015/01/11 dadams
;;     Added: ucsc-define-char-insert-cmd.  Use it in ucsc-make-commands and ucsc-insert.
;;     ucsc-make-commands, ucsc-insert: Ensure non-nil ARG for non-interactive use.
;;                                      Mention hex and octal in doc string.
;;     Added autoload cookie for ucsc-insert.
;; 2012/12/15 dadams
;;     ucsc-insert: Raise error if CHARACTER is not characterp.
;; 2012/10/06 dadams
;;     ucsc-insert: Provided missing CHARACTER arg to insert-char.
;; 2012/06/01 dadams
;;     Added: ucsc-insert.
;;     Renamed ucs-make-commands to ucsc-make-commands.
;; 2012/01/14 dadams
;;     ucs-make-commands: Improve doc string.
;; 2011/10/04 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun ucsc-get-hash-keys (value hash-table &optional value-test-function)
  "Return a list of keys associated with VALUE in HASH-TABLE.
Optional arg VALUE-TEST-FUNCTION (default `equal') is the equality
predicate used to compare values."
  (setq value-test-function  (or value-test-function  #'equal))
  (let ((keys  ()))
    (maphash (lambda (key val)
               (when (funcall value-test-function val value)
                 (push key keys)))
             hash-table)
    keys))

(defun ucsc-get-a-hash-key (value hash-table &optional value-test-function)
  "Return a hash key associated with VALUE in HASH-TABLE.
If there is more than one such key then it is undefined which is
returned.
Optional arg VALUE-TEST-FUNCTION (default `equal') is the equality
predicate used to compare values."
  (setq value-test-function  (or value-test-function  #'equal))
  (catch 'get-a-hash-key
    (maphash (lambda (key val)
               (when (funcall value-test-function val value)
                 (throw 'get-a-hash-key key)))
             hash-table)
    nil))

(defun ucsc-char-names (character)
  "Return a list of the names for CHARACTER."
  (if (hash-table-p (ucs-names))
      (ucsc-get-hash-keys character (ucs-names))
    (mapcar #'car (ucs-names))))

(defun ucsc-char-name (character &optional prefer-old-name-p)
  "Return the name of CHARACTER, or nil if it has no name.
This is Unicode property `name' if there is one, or property
 `old-name' if not, or nil if neither.
Non-nil optional arg PREFER-OLD-NAME-P means reverse the priority,
 returning the old name if there is one."
  (if prefer-old-name-p
      (or (get-char-code-property character 'old-name)
          (get-char-code-property character 'name))
    (or (get-char-code-property character 'name)
        (get-char-code-property character 'old-name))))

;;;###autoload
(defun ucsc-define-char-insert-cmd (character &optional msgp)
  "Define a command that inserts CHARACTER.
You are prompted for the CHARACTER name or code point, just as for `insert-char'.
The command symbol is returned.

The command has the same name as the character itself, but lowercase
and with any spaces replaced by hyphens.

For example, if the character is named `GREEK CAPITAL LETTER DELTA',
then the command, which inserts one or more such chars, is named
`greek-capital-letter-delta'.

Non-interactively:
 CHARACTER is a character - it must satisfy `characterp'.
 MSGP non-nil means echo the name of the created command."
  (interactive (list (read-char-by-name "Unicode (name or hex): ") t))
  (let* ((char-name  (ucsc-char-name character))
         (cmd-name   (and char-name
                          (downcase (replace-regexp-in-string " " "-" char-name nil t))))
         (cmd        (and cmd-name  (intern cmd-name))))
    (unless char-name (error "No such Unicode character: `%s'" character)) ; Impossible?
    (eval `(defun ,cmd (arg)
             ,(format "Insert Unicode character `%s'.
This char has code point %d decimal \(%X hex, %o octal)."
                      char-name character character character)
             (interactive "*p")
             (unless arg (setq arg  1))
             (insert (make-string arg ,character))))
    (when msgp (message "Created command `%s'"
                        (downcase (replace-regexp-in-string " " "-" char-name nil t))))
    cmd))

;; Same as `insert-char' (aka `ucs-insert'), except:
;;
;; 1) A negative prefix arg has the same COUNT effect as a positive one.
;;
;; 2) A negative prefix arg also creates a command to insert the
;;    character.  The command name is the same as the character name,
;;    except it is lower case and uses hyphens instead of spaces.
;;
;; 3) Optional arg MSGP is added, and a confirmation message is shown.
;;
;; 4) Better error message if input CHARACTER is not a Unicode character.
;;
;;;###autoload
(defun ucsc-insert (character &optional count inherit msgp)
  "Insert COUNT copies of CHARACTER of the given Unicode code point.
Interactively, prompts for a Unicode character name or a hex number
using `read-char-by-name'.

You can type a few of the first letters of the Unicode name of
CHARACTER, and use completion.  If you type a substring of the Unicode
name preceded by an asterisk `*' and use completion, it will show all
the characters whose names include that substring, not necessarily at
the beginning of the name.

Also accepts as input CHARACTER a hexadecimal number of Unicode code
point or a number in hash notation, e.g. #o21430 for octal, #x2318 for
hex, or #10r8984 for decimal.

Optional third arg INHERIT (non-nil when called interactively), says
to inherit text properties from adjoining text, if those properties
are sticky.

If COUNT is negative:

1. Insert (- COUNT) copies (so -3 acts the same as 3).

2. Define a command that inserts CHARACTER having the same name as the
character itself, but lowercase and with any spaces replaced by
hyphens.  For example, if the character is named `GREEK CAPITAL LETTER
DELTA', then the command, which inserts one or more such chars, is
named `greek-capital-letter-delta'.

You can then bind the created command to a convenient key.

Interactively, or with non-nil MSGP arg, echo confirmation of the
command creation."
  (interactive
   (list (read-char-by-name "Unicode (name or hex): ")
     (prefix-numeric-value current-prefix-arg)
     t
         t))
  (unless (characterp character) ; Protect `insert-char' from low-level err.
    (error "No such Unicode character: `%s'" character))
  (let ((create-cmd-p  (< count 0)))
    (setq count  (abs count))
    (if (commandp 'insert-char) ; Emacs 24.  Handle the renaming this way.
        (insert-char character count inherit)
      (ucs-insert character count inherit))
    (when create-cmd-p (ucsc-define-char-insert-cmd character msgp))))

;;;###autoload
(defun ucsc-make-commands (regexp &optional msgp)
  "Create commands to insert Unicode characters whose names match REGEXP.
Letter case is ignored for matching.

The set of char names used is taken from `ucs-names'.  There are
,*many* such chars, so consider using a tighter regexp to limit the
number of commands created.

The commands created have the same names as the chars they insert,
except that `SPC' chars in the character names are replaced by
hyphens (`-'), and the command names are lowercase.

Return the commands created, as a list of symbols."
  (interactive (list (read-regexp "Regexp: ") t))
  (let ((cmds  ()))
    (if (hash-table-p (ucs-names))
        (maphash (lambda (key val)
                   (when (let ((case-fold-search  t)) (string-match-p (upcase regexp) key))
                     (push (ucsc-define-char-insert-cmd val) cmds)))
                 (ucs-names))
      (dolist (name.code  (ucs-names))
        (when (let ((case-fold-search  t)) (string-match-p (upcase regexp) (car name.code)))
          (push (ucsc-define-char-insert-cmd (cdr name.code)) cmds))))
    (when msgp (message "Created commands: %s" cmds))
    cmds))

;;;;;;;;;;;;;;;

(ucsc-make-commands "Latin.* letter [a-z]+ with macron$")
(define-prefix-command 'macron-map)
(global-set-key (kbd "C-x 8 l") 'macron-map)
(define-key macron-map (kbd "a") 'latin-small-letter-a-with-macron)
(define-key macron-map (kbd "e") 'latin-small-letter-e-with-macron)
(define-key macron-map (kbd "i") 'latin-small-letter-i-with-macron)
(define-key macron-map (kbd "o") 'latin-small-letter-o-with-macron)
(define-key macron-map (kbd "u") 'latin-small-letter-u-with-macron)
(define-key macron-map (kbd "A") 'latin-capital-letter-a-with-macron)
(define-key macron-map (kbd "E") 'latin-capital-letter-e-with-macron)
(define-key macron-map (kbd "I") 'latin-capital-letter-i-with-macron)
(define-key macron-map (kbd "O") 'latin-capital-letter-o-with-macron)
(define-key macron-map (kbd "U") 'latin-capital-letter-u-with-macron)

  ;; (global-set-key (kbd "s-t") 'my-google-translate-at-point)
  ;; (global-set-key (kbd "s-w") 'org-web-tools-insert-web-page-as-entry)
  ;; (global-set-key (kbd "s-C") 'gpc/clean-slate)

(defun gpc/makron ()
  "Replace un-macroned letter under point with the corresponding macronized letter"
  (interactive)
  (cond ((equal (char-after (point)) ?a)
         (delete-char 1) (insert ?ā))
        ((equal (char-after (point)) ?e)
         (delete-char 1) (insert ?ē))
        ((equal (char-after (point)) ?i)
         (delete-char 1) (insert ?ī))
        ((equal (char-after (point)) ?o)
         (delete-char 1) (insert ?ō))
        ((equal (char-after (point)) ?u)
         (delete-char 1) (insert ?ū))
        (t (insert-char 1 ?k))))

(defun gpc/cloze ()
  "Put '{{c1:' before the region an '}}' after the region, so I don't have to do the Clozing in Anki's UI"
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end)))
    (goto-char beg)
    (insert "{{c1::")
    (goto-char (+ end 6))
    (insert "}}")))

    ;; (global-set-key (kbd "k") 'gpc/makron)
    ;; (global-set-key (kbd "j") 'gpc/cloze)
    ;; ;; (global-set-key (kbd "j") 'gpc/cloze-mac)
    ;; (global-set-key (kbd "k") 'self-insert-command)
    ;; (global-set-key (kbd "j") 'self-insert-command)

;; this didn't have the desired effect, but I'm leaving it here for now
;;(defadvice! gpc/simplify-todos (&rest _)
;;  :after 'org-set-regexps-and-options (setq org-todo-sets '("TODO" "DONE")))

(defun gpc/gen-weekly (monday-tv)
  (mapconcat
   (lambda (num) (concat "* "
                         (format-time-string "%a %b %e" (time-add monday-tv (* num 24 60 60)))
                         "\n** Plan\n** Meetings\n** Notes\n"))
   (number-sequence -1 5)
   ""))

;; (gpc/gen-weekly (current-time))

(global-set-key (kbd "M-=") 'mark-whole-buffer)
