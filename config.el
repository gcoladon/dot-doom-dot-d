;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; don't forget -- config.el happens before custom.el

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Greg Coladonato")

;; So that org capturing doesn't insert the ID of the captured file at point
(setq org-capture-link-is-already-stored t)

;; trying to debug the cache problem
(setq org-element--cache-self-verify 'backtrace)
(setq org-element-use-cache nil)
(setq org-roam-v2-ack t)

(if (equal (replace-regexp-in-string "[\t|\n]" ""
                                     (shell-command-to-string "ifconfig en0 | grep ether"))
           "ether f0:18:98:9a:c9:2c ")
    (setq gpc/email "gcoladon@gmail.com"
          gpc/org-dir "~/org/"
          gpc/org-agenda-files (list (concat gpc/org-dir "roam-personal/")
                                     (concat gpc/org-dir "roam-stem/"))
          gpc/pdf-dir "~/pdfs"
          gpc/bib-file "~/pdfs/references.bib"
          org-roam-graph-viewer "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
          gpc/todays-notes-fn 'gc/org-roam-monthly)

  (setq gpc/email "greg@pilot.ai"
        gpc/org-dir "~/org/"
        gpc/org-agenda-files (list (concat gpc/org-dir "roam-pilot/")
                                   (concat gpc/org-dir "roam-stem/"))
        gpc/pdf-dir "~/pdfs"
        gpc/bib-file "~/pdfs/references.bib"
        org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        gpc/todays-notes-fn 'gc/org-roam-weekly-this))

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
;; (defun my/dired-display-prev-file ()
;;   "In Dired directory, go to the previous file, and open it.
;;             If `org-roam-mode' is active, update the org-roam-buffer."
;;   (interactive)
;;   (dired-previous-line 1)
;;   (let ((buf (find-file-noselect (dired-get-file-for-visit))))
;;     (display-buffer buf t)
;;     (when (my/check-org-roam-buffer-p buf)
;;       (with-current-buffer buf
;;         (setq org-roam--current-buffer buf)
;;         (org-roam-buffer-update)))))

;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (dired-hide-details-mode 1)
;;             ;; I don't use dired-subtree at the moment
;;             ;;(define-key dired-mode-map (kbd "<tab>") #'dired-subtree-toggle)
;;             ;;(define-key dired-mode-map (kbd "<C-tab>") #'dired-subtree-cycle)
;;             (define-key dired-mode-map (kbd "<SPC>") #'my/dired-display-next-file)
;;             (define-key dired-mode-map (kbd "<down>") #'my/dired-display-next-file)
;;             (define-key dired-mode-map (kbd "<up>") #'my/dired-display-prev-file)))

;; (defun my/dired-in-side-buffer ()
;;   "Display Dired in a side window."
;;   (interactive)
;;   (let* ((dir (read-directory-name "Directory: "))
;;          (buf (dired-noselect dir)))
;;     (select-window
;;      (display-buffer-in-side-window buf
;;                                     '((side . left)
;;                                       (window-width . 30)
;;                                       (slot . -1)
;;                                       (window-parameters . ((mode-line-format . none))))))))

;; Let's see if I prefer this style of search interaction
;; (ctrlf-mode +1)

;; https://sachachua.com/blog/2015/08/org-mode-date-arithmetic/
;; Thanks Sacha!
(defun gc/org-roam-find-weekly (which)
  "Find file corresponding to the week beginning with WHICH"
  (setq monday-tv (org-read-date nil t which))
  (let ((monday-str (org-read-date nil nil which)))
    (org-roam-node-find nil
     (concat "Week of " monday-str)) nil nil t))

(defun gc/org-roam-monthly ()
  "Find the monthly-file for this month."
  (interactive)
  (setq
   next-first (org-read-date nil t "1")
   first-tv (org-read-date nil t "--m" nil next-first)
   first-str (org-read-date nil nil "--m" nil next-first))
  (org-roam-node-find nil (concat "Month of " (substring first-str 0 7))))

;; (defun gc/org-roam-monthly-visit ()
;;   "Find the monthly-file for this month."
;;   (interactive)
;;   (setq
;;    next-first (org-read-date nil t "1")
;;    first-tv (org-read-date nil t "--m" nil next-first)
;;    first-str (org-read-date nil nil "--m" nil next-first))
;;   (org-roam-node-visit (concat "Month of " (substring first-str 0 7))))

(defun gc/org-roam-weekly-this ()
  "Find the weekly-file for this week."
  (interactive)
  (if (equal (format-time-string "%a" (current-time)) "Mon")
      (gc/org-roam-find-weekly "+0")
    (gc/org-roam-find-weekly "-mon")))

;; (defun gc/org-roam-weekly-next ()
;;   "Find the weekly-file for next week."
;;   (interactive)
;;   (gc/org-roam-find-weekly "+mon"))

;; Why not, I use these functions all the time, a single chord makes sense
(global-set-key (kbd "s-f") 'org-roam-node-find)
(global-set-key (kbd "s-i") 'org-roam-node-insert)
(global-set-key (kbd "s-t") 'gc/org-roam-weekly-this)
(global-set-key (kbd "s-r") gpc/todays-notes-fn)
(global-set-key (kbd "s-d") (cmd! (find-file "~/org/roam-stem/210820_dl_syllabus.org")))

;; (defun org-hide-properties ()
;;   "Hide all org-mode headline property drawers in buffer. Could be slow if buffer has a lot of overlays."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward
;;             "\n *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
;;       (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
;;         (overlay-put ov_this 'display " -P-\n")
;;         ;;(overlay-put ov_this 'invisible t)
;;         ;; (overlay-put ov_this 'display (format "\n"))
;;         (overlay-put ov_this 'hidden-prop-drawer t)))))

;; (defun org-show-properties ()
;;   "Show all org-mode property drawers hidden by org-hide-properties."
;;   (interactive)
;;   (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t))

;; (defun org-toggle-properties ()
;;   "Toggle visibility of property drawers."
;;   (interactive)
;;   (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
;;       (progn
;;         (org-show-properties)
;;         (put 'org-toggle-properties-hide-state 'state 'shown))
;;     (progn
;;       (org-hide-properties)
;;       (put 'org-toggle-properties-hide-state 'state 'hidden))))

;; Try to figure out where to put this so that it doesn't
;; get called for my org-files.
;; https://emacs.stackexchange.com/questions/14438/remove-hooks-for-specific-modes
;; But this didn't work did it!
;; (remove-hook 'before-save-hook 'ws-butler-before-save)
;; this might work though
;; (setq before-save-hook nil)

(map! :leader
      :desc "Count words region"          "l =" #'count-words-region
      :desc "Copy todos from email"       "l t" #'gpc/copy-todos-from-email
      :desc "org-mark-ring-goto"          "l g m" #'org-mark-ring-goto
      :desc "Flush lines"                 "l f" #'flush-lines
      :desc "Keep lines"                  "l k" #'keep-lines
      :desc "Insert $ around"             "i $" #'gpc/wrap-region-with-dollars
      :desc "Insert \textsc around"       "i \\" #'gpc/wrap-region-with-textsc
      :desc "Insert [$] around"           "i [" #'gpc/wrap-region-with-anki-latex
      :desc "Toggle fundamental-mode on"  "t u" #'fundamental-mode
      :desc "JSON pretty print buffer"    "t j" #'json-pretty-print-buffer
      :desc "Toggle debug-on-error"       "t d" #'toggle-debug-on-error
      :desc "Toggle Frame Min/Max"        "t m" #'toggle-frame-maximized
      :desc "Toggle truncate lines"       "t t" #'toggle-truncate-lines
      :desc "Toggle/set a variable"       "t v" #'set-variable
      :desc "Toggle overwrite mode"       "t o" #'overwrite-mode
      :desc "Toggle tbl-col-width"        "t c" #'org-table-toggle-column-width
      :desc "org-fwd-heading-same-level"  "n C-f" #'org-forward-heading-same-level
      :desc "Recover this file"           "f v" #'recover-this-file
      :desc "Go to Greg's notes.org"      "n g" (cmd! (find-file "~/dev/org/notes.org"))
      :desc "HTML-to-Org"                 "n h" (cmd! (html2org-clipboard))
      :desc "Bury buffer"                 "w y" #'bury-buffer)

(map! :leader
      ;;; <leader> r --- roam
      (:prefix-map ("r" . "gc/roam bindings")
       :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
       :desc "isbn-to-bibtex"                "B" #'isbn-to-bibtex
       :desc "Capture"                       "c" #'org-roam-capture
       :desc "Node find"                     "f" #'org-roam-node-find
       :desc "Show graph"                    "g" #'org-roam-graph
       :desc "Insert"                        "i" #'org-roam-node-insert
       :desc "org-roam-buffer-toggle"        "r" #'org-roam-buffer-toggle
       :desc "Insert last stored link"       "s" #'org-insert-last-stored-link
       :desc "Insert (skipping org-cap)"     "I" #'org-roam-insert-immediate
       :desc "Jump to Index"                 "j" #'org-roam-jump-to-index
       :desc "Toggle property visibility"    "p" #'org-toggle-properties
       :desc "org-ref-helm-insert-cite"      "H" #'helm-bibtex-with-notes
       :desc "Copy org subtree"              "t" #'org-copy-subtree))

;;(doom/increase-font-size 1)

;; (defun gpc/orb-find-file-open-noter (file)
;;   (find-file file)
;;   (outline-next-visible-heading 1)
;;   (org-noter)
;;   (other-window 1)
;;   (org-roam)
;;   (outline-next-visible-heading -1))

;; (defun gpc/orb-edit-notes (orig-fn &rest args)
;;   (let ((org-roam-find-file-function #'gpc/orb-find-file-open-noter))
;;     (apply orig-fn args)))
;; (advice-add 'orb-edit-notes :around #'gpc/orb-edit-notes)

(defun gpc/mon_day ()
  (format-time-string "%m %d"))

;; (use-package! org-noter
;;    :after (:any org pdf-view)
;;    :config
;;    (setq org-noter-always-create-frame nil
;;          org-noter-auto-save-last-location t)
;;    (defun org-noter-init-pdf-view ()
;;      (pdf-view-fit-width-to-window))
;;      ;; (pdf-view-auto-slice-minor-mode)
;;    (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view))

;; (use-package! org-transclusion
;;   :after-call org-roam-node-find)

;; (use-package! ox-rst
;;   :after-call org-roam-node-find)

;; :after-call org-roam-node-find
;; :after-call after-find-file

;; (use-package! projectile
;;   :after-call (pre-command-hook after-find-file dired-before-readin-hook))

;; might be needed to get the org-roam-server to work.
;; (after! org-roam
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode)
;;   (smartparens-global-mode 1))



;; Ask what's up with these errors lately
;; error in process sentinel: async-handle-result: Cannot open load file: No such file or directory, dash
;; error in process sentinel: Cannot open load file: No such file or directory, dash
;; (setq doi-utils-async-download nil)
;; Fixed by https://github.com/jkitchin/org-ref/issues/923

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

;; (defun gpc/fetch-tasks ()
;;   "Run the extractor from Sheets, Open the file, and also open the Sheet"
;;   (interactive)
;;   (shell-command "~/bin/tasks.sh")
;;   (find-file "~/dev/org/incoming.org")
;;   (browse-url "https://docs.google.com/spreadsheets/d/1T8qe2m4z9ViJ9W72PJb8COCmpptr5D5312vM6e32iPM/edit#gid=0"))

;; (defun gpc/org-table-goto-beginning ()
;;   (interactive)
;;   (if (org-at-table-p)
;;       (goto-char (org-table-begin))
;;     (message "Can't go to beginning of table if not inside a table")))

;; (defun gpc/org-table-goto-end ()
;;   (interactive)
;;   (if (org-at-table-p)
;;       (goto-char (org-table-end))
;;     (message "Can't go to end of table if not inside a table")))

;; (define-key org-mode-map (kbd "C-c C-g a") #'gpc/org-table-goto-beginning)
;; (define-key org-mode-map (kbd "C-c C-g e") #'gpc/org-table-goto-end)

(defun gpc/clean-slate ()
  "Go to the top of the page and open just a bit."
  (interactive)
  (beginning-of-buffer)
  (org-shifttab 1)
  )

;; I used these keystrokes for a GA homework or something?
;; (define-prefix-command 'algorithms-map)
;; (global-set-key (kbd "C-c l a") 'algorithms-map)
;; (define-key algorithms-map (kbd "1") (cmd! (insert "$L_{buy}$")))
;; (define-key algorithms-map (kbd "2") (cmd! (insert "$L_{sell}$")))
;; (define-key algorithms-map (kbd "3") (cmd! (insert "$R_{buy}$")))
;; (define-key algorithms-map (kbd "4") (cmd! (insert "$R_{sell}$")))


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
                         "\n** Plan\n** Meetings\n*** 9 AM Product Synch\n** Notes\n"))
   (number-sequence 0 4)
   ""))

;; (gpc/gen-weekly (current-time))

(global-set-key (kbd "M-=") 'mark-whole-buffer)

;; https://org-roam.discourse.group/t/prototype-transclusion-block-reference-with-emacs-org-mode/830/96?u=gcoladon

;; (advice-add #'org-roam-db-insert-file :after
;;             #'my/org-id-update-location-at-org-roam-db-insert-file))

;; (defun my/org-id-update-location-at-org-roam-db-insert-file ()
;;   "Update `org-id-locations-file' and hash table.
;; It's meant to be used with `advice-add' :after
;; `org-roam-db-insert-file'.  We can assume that this function is
;; run wihtin a buffer visiting a file being inserted, as
;; insert-file is run within `org-roam-with-file' macro."
;;     (when-let ((id (org-entry-get 1 "id")))
;;       (org-id-add-location id (buffer-file-name))))

(use-package! command-log-mode)

;;  this function sorts todos by the yymmdd timestamp in the roam filename
;;  so the todos in the most recently created files are at the top.
(defun <=> (a b)
 "Compare A and B, returning 1, 0, or -1"
  (cond ((= a b) 0)
        ((> a b) 1)
        ((< a b) -1)))

(defun gpc/org-agenda-cmp-user-defined (a b)
  "Compare two org-agenda-events by their org-roam file prefix datestamp"
  (let* ((regex "\\(2[0-9][0-9][0-9][0-9][0-9]\\)")
         (date-a (if (string-match regex a)
                     (string-to-number (or (match-string 1 a) "0"))
                   0))
         (date-b (if (string-match regex b)
                     (string-to-number (or (match-string 1 b) "0"))
                   0)))
    (<=> date-a date-b)))

(setq org-agenda-cmp-user-defined 'gpc/org-agenda-cmp-user-defined)

;; The next few forms implement the ability to go to a directory
;; in dired and press 'b' to migrate a PDF from one of my classes
;; into a sub-bibtex entry.
;;
;; I think I would also like to have a version that I use for
;; other people's research papers, which does not make them inbooks
;; but rather articles of their own. Some other time.


;; (define-key dired-mode-map "B" 'gpc/move-paper-to-bibtex-crossref)

;; This was useful when I was converting text into tex for
;; my CS 6515 GA assignments
(defun gpc/wrap-region-with-something (before after)
  "Put a dollar at point-min and also at point-max"
  (interactive)
  (save-excursion
    (goto-char (region-end))
    (insert after)
    (goto-char (region-beginning))
    (insert before))
  (forward-char (length after)))

(defun gpc/wrap-region-with-dollars ()
  "Put a dollar at point-min and also at point-max"
  (interactive)
  (gpc/wrap-region-with-something "$" "$"))

(defun gpc/wrap-region-with-anki-latex ()
  "Put a dollar at point-min and also at point-max"
  (interactive)
  (gpc/wrap-region-with-something "[$]" "[/$]"))

(defun gpc/wrap-region-with-textsc ()
  "Wrap an algorithm name for Anki"
  (interactive)
  (gpc/wrap-region-with-something "[$]\\textsc{" "}[/$]"))

;; https://www.emacswiki.org/emacs/ParenthesisMatching
;; Needed to do this to better natch
(modify-syntax-entry ?“ "(”")
(modify-syntax-entry ?” ")“")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(defun gpc/copy-todos-from-email ()
  (with-temp-buffer
    (clipboard-yank)
    (mark-whole-buffer)
    (flush-lines "gcoladon")
    (flush-lines "^$")
    (kill-ring-save (point-min) (point-max))))

(load! "ucs-cmds.el")
(load! "vulpea.el")
(load! "workflow.el")

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

(define-key dired-mode-map "b" 'gpc/move-pdfs-to-bibtex-crossref)
