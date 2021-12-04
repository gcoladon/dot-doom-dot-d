;;; workflow.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Search on arXiv"             "s a" #'gpc/search-on-arxiv
      :desc "Search on scopus"            "s C" #'scopus-basic-search
      :desc "gpc/get-arxiv"               "r x" #'gpc/get-arxiv
      :desc "doi-utils-get-bibtex-pdf"    "r d" #'doi-utils-get-bibtex-entry-pdf
      :desc "orb-note-actions"            "r a" #'orb-note-actions
      :desc "org-noter-create-skeleton"   "r k" #'org-noter-create-skeleton
      :desc "org-noter"                   "r n" #'org-noter
      :desc "helm-bibtex"                 "r h" #'helm-bibtex)

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

(defun gpc/trim-slug (orig-fun &rest args)
  "Trim the slug to 23 characters"
  (let ((full-slug (apply orig-fun args)))
         (substring full-slug 0 (min 23 (length full-slug)))))

(advice-add 'org-roam-node-slug :around #'gpc/trim-slug)

(defun gpc/search-on-arxiv (start end)
  "Get the region, add arxiv to it, and search for it"
  (interactive "r")
  (+lookup/online (concat "arxiv " (buffer-substring-no-properties start end)) "Google"))

(defun gpc/capture-noter-file (key)
  (setq gpc/save-templates org-roam-capture-templates)
  (setq gpc/temp org-roam-capture-templates)
  ;; Does this logic only work as long as the desired template
  ;; is the last one in the list? If so, I may need to enhance this
  ;; logic at some point if that ever fails to hold true.
  (while
      (and (not (eq nil gpc/temp))
           (not (equal "n" (caar gpc/temp))))
    (setq gpc/temp (cdr gpc/temp)))
  (setq org-roam-capture-templates gpc/temp)
  (bibtex-completion-edit-notes (list key))
  (setq org-roam-capture-templates gpc/save-templates))

(defun gpc/get-arxiv ()
  "Use the defaults for all three variables, don't ask me!!"
  (interactive)
  (require 'org-ref)
  (bibtex-set-dialect 'BibTeX)
  (arxiv-get-pdf-add-bibtex-entry (arxiv-maybe-arxiv-id-from-current-kill)
                                  gpc/bib-file
                                  (concat gpc/pdf-dir "/"))
  (gpc/capture-noter-file
   (save-window-excursion
     (find-file "~/pdfs/references.bib")
     (goto-char (point-max))
     (bibtex-beginning-of-entry)
     (re-search-forward bibtex-entry-maybe-empty-head)
     (if (match-beginning bibtex-key-in-head)
         (buffer-substring-no-properties
          (match-beginning bibtex-key-in-head)
          (match-end bibtex-key-in-head)))))
  (org-capture-finalize t)
  (end-of-buffer)
  (org-noter)
  ;; (other-window)
  ;; (org-noter-create-skeleton)
  )

(defun bibtex-autokey-wrapper (orig-fun &rest args)
  "Dynamically bind `bibtex-autokey-prefix-string' to current date."
  (let ((result
         (let ((bibtex-autokey-prefix-string (format-time-string "%y%m%d_")))
           (apply orig-fun args))))
    (substring result 0 (min 31 (length result)))))
  ;; (let* ((result
  ;;         (let ((bibtex-autokey-prefix-string (format-time-string "%y%m%d_")))
  ;;           (apply orig-fun args)))
  ;;        (draft
  ;;         (substring result 0 (min 31 (length result))))
  ;;        (len (length draft))
  ;;        (final (if (eq (substring draft (- len 1) len) "_")
  ;;                   (substring draft 0 (- len 1))
  ;;                 draft)))))

(advice-add 'bibtex-generate-autokey :around #'bibtex-autokey-wrapper)

(defun gpc/org-roam-protocol-get-pdf (info)
  "Process an org-protocol://roam-pdf?ref= style url with INFO.

It saves the PDF at the target location into ~/pdfs and also
creates a corresponding org-noter file

  javascript:location.href = \\='org-protocol://roam-pdf?template=r&url=\\='+ \\
        encodeURIComponent(location.href))"

  (interactive)
  (let ((url (plist-get info :url)))
    (unless url
      (user-error "No url provided"))
    (if (string-match-p "arxiv.org" url)
        (progn
          (kill-new url)
          (gpc/get-arxiv))
      (gpc/move-pdf-to-bibtex-standalone (plist-get info :url)))))

(after! org-protocol
  (use-package! org-roam-protocol)

  ;; This is my attempt at making a capture-PDF-into-org-roam utility,
  ;; to make it easier to capture research papers that I've been given a URL to

  (push '("org-roam-pdf"
          :protocol "roam-pdf"
          :function gpc/org-roam-protocol-get-pdf)
        org-protocol-protocol-alist))

(defun gpc/move-pdf-to-bibtex-standalone (pdf-url)
  "Try to simplify the incorporation of pdfs from a class into org-roam"
  (let* ((pdf-author (read-string "Author: "))
         (pdf-title (read-string "Title: "))
         (pdf-year (read-string "Year: "))
         (prefixed-fn (concat (format-time-string "%y%m%d_")
                              (downcase pdf-author)
                              (substring pdf-year 2)
                              ".pdf"))
         (bibtex-key (substring prefixed-fn 0 (- (length prefixed-fn) 4)))
         (dest-fn (concat gpc/pdf-dir "/" prefixed-fn)))
    (url-copy-file pdf-url dest-fn)
    (save-window-excursion
      (find-file gpc/bib-file)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert (concat "@article{" bibtex-key ",\n"
                      "  author          = {" pdf-author "},\n"
                      "  title           = {{" pdf-title "}},\n"
                      "  year            = {" pdf-year "},\n"
                      "  url             = {" pdf-url "}\n"
                      "}\n"))
      (save-buffer))
    (gpc/capture-noter-file bibtex-key)))


(use-package! org-noter
  :after pdf-view
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-fit-width-to-window))

(after! org-roam
  (org-roam-bibtex-mode))

(use-package! helm-bibtex)

;; following instructions from https://github.com/org-roam/org-roam-bibtex
(use-package! org-roam-bibtex
  ;; :after org-roam
  ;; :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  (require 'helm-bibtex)
  (bibtex-set-dialect 'BibTeX)
  (helm-add-action-to-source
   "Edit org-noter Notes"
   'helm-bibtex-edit-notes
   helm-source-bibtex
   0)
)

(defun gpc/move-pdfs-to-bibtex-crossref (key)
  "Choose an _oms_cs_ bibtex entry for which to add one or more
inbook entries via gpc/move-pdf-to-bibtex-crossref"
  (interactive
   (list
    (completing-read
     "Which class is this document for? "
     (seq-filter
      (lambda (elt) (or (string-match-p "_gatech_cs_" elt)
                        ;; I would have used stanford but tag got too long
                        (string-match-p "_stan_cs_" elt)))
      (mapcar
       (lambda (elt) (cdr (assoc "=key=" (cdr elt))))
       (seq-filter
        (lambda (elt) (equal "book" (cdr (assoc "=type=" (cdr elt)))))
        (bibtex-completion-candidates)))))))
  (setq org-capture-link-is-already-stored t)
  (seq-do
   (lambda (file) (gpc/move-pdf-to-bibtex-crossref file key))
   ;; I reverse so that when helm-bibtex loads them all up,
   ;; they are in the right order
   (reverse (dired-get-marked-files nil nil nil nil t))))

(defun gpc/move-pdf-to-bibtex-crossref (file crossref)
  "Try to simplify the incorporation of pdfs from a class into org-roam"
  (let* ((basename (file-name-nondirectory file))
         (suffixless (substring basename 0 (- (length basename) 4)))
         (title-guess (mapconcat 'identity
                                 (mapcar (lambda (word) (if (< (length word) 4)
                                                            (upcase word)
                                                          (capitalize word)))
                                         (split-string suffixless "_")) " "))
         (title (read-string "Preferred title for this document: " title-guess))
         (pref-basename (read-string "Preferred base filename: " suffixless))
         (trunc-basename (substring pref-basename 0 (min 23 (length pref-basename))))
         ;; I kind of would like to trim off trailing _ for aesthetic reasons
         (prefixed-fn (concat (format-time-string "%y%m%d_") trunc-basename ".pdf"))
         (key (substring prefixed-fn 0 (- (length prefixed-fn) 4)))
         (dest-fn (concat gpc/pdf-dir "/" prefixed-fn)))
    (dired-create-files #'dired-rename-file "Move" (list file)
                        (lambda (_from) dest-fn) t)
    (save-window-excursion
      (find-file gpc/bib-file)
      (goto-char (point-max))
      (when (not (looking-at "^")) (insert "\n"))
      (insert (concat "@inbook{" key ",\n"
                      ;; If you try to get away with a single {}, the caps get messed up
                      "  title           = \"{{" title "}}\",\n"
                      "  crossref        = {" crossref "}\n"
                      "}\n"))
      (save-buffer))
    (gpc/capture-noter-file key)))

;; Until I get it to work via org-protocol, use this!
;; (gpc/move-pdf-to-bibtex-standalone "https://jmlr.csail.mit.edu/papers/volume13/bergstra12a/bergstra12a.pdf")


;; (defun gpc/move-paper-to-bibtex-crossref ()
;;   "Try to simplify the incorporation of papers that are distributed
;;    by OMS classes into my own org-roam library

;; This function doesn't really work yet. I'm not even sure if it
;; has a real use case? I think maybe when I have a PDF that didn't come
;; from the internet, I could somehow use this in conjunction with the
;; one that pulls from the internet?

;; Work this out next time I try to incorporate a bunch of PDFs that I
;; download en masse from canvas or someplace."
;;   (interactive)
;;   (let* ((file (car (dired-get-marked-files nil nil nil nil t)))
;;          (author (read-string "Author: "))
;;          (year (read-string "Year: "))
;;          (title (read-string "Title: "))
;;          (basename (file-name-nondirectory file))
;;          (prefixed-fn (concat (format-time-string "%y%m%d_") basename))
;;          (key (substring prefixed-fn 0 (- (length prefixed-fn) 4)))
;;          (dest-fn (concat gpc/pdf-dir "/" prefixed-fn)))
;;     (dired-create-files #'dired-rename-file "Move" (list file)
;;                         (lambda (_from) dest-fn) t)
;;     (save-window-excursion
;;       (find-file gpc/bib-file)
;;       (goto-char (point-max))
;;       (when (not (looking-at "^")) (insert "\n"))
;;       (insert (concat "@inbook{" key ",\n"
;;                       "  title           = \"{" title "}\",\n"
;;                       "  crossref        = {" crossref "}\n"
;;                       "}\n"))
;;       (save-buffer))
;;     (setq gpc/save-templates org-roam-capture-templates)
;;     (setq gpc/temp org-roam-capture-templates)
;;     (while
;;         (and (not (eq nil gpc/temp))
;;              (not (equal "n" (caar gpc/temp))))
;;       (setq gpc/temp (cdr gpc/temp)))
;;     (setq org-roam-capture-templates gpc/temp)
;;     (bibtex-completion-edit-notes (list key))
;;     (setq org-roam-capture-templates gpc/save-templates)))

;; I think this may be more complicated than I hoped it would be
;;
;; (setq org-id-method 'ts
;;       orb-preformat-keywords
;;       '(("citekey" . "=key=")
;;         "url" "file" "author-or-editor-abbrev" "keywords" "abstract" "author" "year")
;;       org-id-link-to-org-use-id t
;;       org-id-ts-format "%y%m%d_%H%M%S")

;; (add-to-list 'org-roam-capture-templates '(("n" "ref+noter" plain "%?" :if-new
;;       (file+head "roam-stem/${citekey}.org" "#+TITLE: %(car (split-string \"${author}\" \",\")) '%(substring \"${year}\" 2 4) - ${title}
;; #+ROAM_KEY: cite:${citekey}
;; #+ROAM_TAGS:

;; * %(car (split-string \"${author}\" \",\")) '%(substring \"${year}\" 2 4) - ${title}
;; :PROPERTIES:
;; :URL: ${url}
;; :AUTHOR: ${author-or-editor-abbrev}
;; :NOTER_DOCUMENT: ${file}
;; :NOTER_PAGE:
;; :END:
;; ** Abstract
;; ${abstract}
;; ") :unnarrowed t)))
