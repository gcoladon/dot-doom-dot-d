;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Greg Coladonato"
      user-mail-address "greg@pilot.ai")

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
(setq org-directory "~/org/")

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
         :desc "Jump to Index"                 "x" #'org-roam-jump-to-index

         :desc "This Weekly"                   "t" #'gc/org-roam-weekly-this
         :desc "Last Weekly"                   "l" #'gc/org-roam-weekly-last
         :desc "Next Weekly"                   "n" #'gc/org-roam-weekly-next
         :desc "Next-Next Weekly"              "N" #'gc/org-roam-weekly-next

         :desc "Graph of 1"                    "1" #'gc/org-roam-graph-1
         :desc "Graph of 2"                    "2" #'gc/org-roam-graph-2))
