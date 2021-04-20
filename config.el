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
          gpc/org-dir "~/dev/org/"
          gpc/pdf-dir "~/pdfs"
          gpc/roam-dir "~/dev/org/roam"
          gpc/bib-file "~/dev/org/references.bib")
  (setq gpc/email "greg@pilot.ai"
        gpc/org-dir "~/org/"
        gpc/pdf-dir "~/pdfs"
        gpc/roam-dir "~/org/roam"
        gpc/bib-file "~/org/references.bib"))

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

;; (map! :leader
;;         :prefix "nr"
;;         :desc "org-roam" "b" #'org-roam-buffer
;;         :desc "org-roam-node-insert" "i" #'org-roam-node-insert
;;         :desc "org-roam-node-find" "f" #'org-roam-node-find
;;         :desc "org-roam-ref-find" "r" #'org-roam-ref-find
;;         :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;         :desc "org-roam-capture" "c" #'org-roam-capture
;;         :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
;;

;; Let's see if I prefer this style of search interaction
;; (ctrlf-mode +1)

;; https://sachachua.com/blog/2015/08/org-mode-date-arithmetic/
;; Thanks Sacha!
(defun gc/org-roam-find-weekly (which)
  (org-roam-find-file
   (concat "Week of "
           (org-read-date nil nil which)) nil nil t))

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
(setq before-save-hook nil)

;;(define-advice bibtex-generate-autokey (:before (&rest _) my-date)
;;  "Set `bibtex-autokey-prefix-string' to desired date format."
;;  (setq bibtex-autokey-prefix-string (format-time-string "%y%m%d-")))

;;(bibtex-set-dialect 'BibTeX)
;;(setq bibtex-autokey-prefix-string (format-time-string "%y%m%d-"))

;; wow that worked!
(map! :leader
      ;;; <leader> n --- notes
      (:prefix-map ("r" . "gc/roam bindings")
         :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
         :desc "Capture"                       "c" #'org-roam-capture
         :desc "Find file"                     "f" #'org-roam-find-file
         :desc "Show graph"                    "g" #'org-roam-graph
         :desc "Insert"                        "i" #'org-roam-insert
         :desc "Org Roam"                      "r" #'org-roam
         :desc "Insert (skipping org-cap)"     "I" #'org-roam-insert-immediate
         :desc "Jump to Index"                 "j" #'org-roam-jump-to-index
         :desc "Toggle property visibility"    "p" #'org-toggle-properties

         ;; I prefer this function which spares me the need to confirm the defaults
         ;; :desc "arxiv-get-pdf-add-bibtex-entry" "x" #'arxiv-get-pdf-add-bibtex-entry
         :desc "gpc/get-arxiv"                 "x" #'gpc/get-arxiv

         :desc "orb-note-actions"              "a" #'orb-note-actions
         :desc "org-noter-create-skeleton"     "k" #'org-noter-create-skeleton
         :desc "org-noter"                     "n" #'org-noter
         :desc "helm-bibtex"                   "h" #'helm-bibtex
         :desc "org-ref-helm-insert-cite"      "H" #'helm-bibtex-with-notes

         :desc "This Weekly"                   "t" #'gc/org-roam-weekly-this
         :desc "Last Weekly"                   "l" #'gc/org-roam-weekly-last
         :desc "Next Weekly"                   "w" #'gc/org-roam-weekly-next
         :desc "Next-Next Weekly"              "W" #'gc/org-roam-weekly-next

         :desc "Graph of 1"                    "1" #'gc/org-roam-graph-1
         :desc "Graph of 2"                    "2" #'gc/org-roam-graph-2))

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
   0)
  (defun bibtex-autokey-wrapper (orig-fun &rest args)
    "Dynamically bind `bibtex-autokey-prefix-string' to current date."
    (let ((bibtex-autokey-prefix-string (format-time-string "%y%m%d_")))
      (apply orig-fun args)))
  (advice-add 'bibtex-generate-autokey :around #'bibtex-autokey-wrapper)
  )

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
