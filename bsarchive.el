(require 'subr-x)

;; (setq bsarchive-catalogue-file "~/ComputerWork/PROJECTS/emacs-archive-manager/test-catalogue.org")
;; (setq bsarchive-database-file "~/ComputerWork/PROJECTS/emacs-archive-manager/test-archive.db")

;; (defun bsarchive-init-database (db-file)
;;   "Prompts user to choose a database file. Wipes any existing information in the
;; database and creates new tables with the following schema:

;; CREATE TABLE documents (url text, artwork_ids text, keywords text);
;; CREATE TABLE artwork(uid text, title text, date text, dimensions text, media text, description text, notes text, keywords text);

;;   (interactive "fChoose database file to initialise: ")"
;;   (if (yes-or-no-p "All existing information in database will be lost! Are you sure? ")
;;       (progn
;;         (let ((db (sqlite-open db-file)))
;;           ;; remove existing contents
;;           (sqlite-execute "DROP TABLE documents")
;;           (sqlite-execute "DROP TABLE artwork")
;;           ;; create new schema
;;           (sqlite-execute "CREATE TABLE documents (url text, artwork_ids text, keywords text);")
;;           (sqlite-execute "CREATE TABLE artwork(uid text, title text, date text, dimensions text, media text, description text, notes text, keywords text);")
;;           (message "... done")))
;;     (message "operation cancelled")))



;;;;;;;;;;;;;;;;;;;;;;;;; FILE METADATA/EXIF ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsarchive-exif-add-keyword (file-url kw)
  ;; -P = preserve file modification date/time
  (shell-command (concat "exiftool -P -keywords+=" kw " " file-url)))

(defun bsarchive-exif-get-keywords (file-url)
  "Returns the contents of the Keywords metadata tag as a list of strings."
  (split-string
   (shell-command-to-string (concat "exiftool -s3 -Keywords " file-url))
   "[ ,]+"))

(defun bsarchive-exif-get-uids (file-url)
  "Gets all archive UIDs from file metadata, or returns NIL if no archive
UIDs found."
  (let ((keywords (bsarchive-exif-get-keywords file-url))
        (uids '())
        (uid-regex "^bst_archive_uid_\\([0-9]+\\)$"))
    (while keywords
      (let ((kw (pop keywords)))
        ;; is this the UID?
        (when (string-match uid-regex kw)
          (push (match-string 1 kw) uids))))
    uids))



;;;;;;;;;;;;;;;;;;;;;;; CATALOGUE OF ARTWORKS ;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsarchive-create-new-catalogue-entry ()
  "Creates a new blank entry in the catalogue of artworks."
  (interactive)
  (find-file-existing bsarchive-catalogue-file)
  (goto-char (point-max))
  (insert "\n"
          "* ITEM\n"
          "** UID")
  (save-excursion
    (insert
          "\n"
          "** TITLE\n"
          "** DATE\n"
          "** DIMENSIONS\n"
          "** MEDIA\n"
          "** DESCRIPTION\n"
          "** NOTES\n"
          "** KEYWORDS\n")))

(defun bsarchive-catalogue-entry-at-point-to-plist ()
  "Returns the next catalogue entry in buffer as a plist where each
element heading is the property name and the contents of that
element is the value.

Returns nil if point is not on the ITEM heading line."
  (interactive)
  (save-excursion

    ;; TODO: check that point is on ITEM heading line - return NIL if not
    ;; (search-forward "* ITEM")

    (save-restriction
      (org-narrow-to-element)

      (let ((output '()))
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (headline)

            (let* ((items (car (cdr headline)))
                   (property-name (plist-get items :raw-value))
                   (begin  (plist-get items :contents-begin))
                   (end (plist-get items :contents-end))
                   (content ""))

              (if (and (integerp begin)
                       (integerp end))
                  (setq content (string-trim (substring-no-properties (buffer-substring begin end)))))
              
              (setq output (plist-put output property-name (format "%s" content))))))
        ;; return
        output))))

(defun bsarchive-slurp-artwork-catalogue ()
  "Updates the archive database with any changes made to the artwork
catalogue org document."
  (interactive)
  (let ((db (sqlite-open bsarchive-database-file)))
    ;; delete whole contents of table from database!
    (sqlite-execute db "DELETE FROM artwork")
    ;; process the catalogue org-file without showing it
    (with-current-buffer (find-file-noselect bsarchive-catalogue-file)
      (goto-char (point-min))
      (while (search-forward "* ITEM" nil t)
        (let ((properties (bsarchive-catalogue-entry-at-point-to-plist)))
          (sqlite-execute db (concat "INSERT INTO artwork VALUES('"
                                     (plist-get properties "UID" 'string-equal) "', '"
                                     (plist-get properties "TITLE" 'string-equal) "', '"
                                     (plist-get properties "DATE" 'string-equal) "', '"
                                     (plist-get properties "DIMENSIONS" 'string-equal) "', '"
                                     (plist-get properties "MEDIA" 'string-equal) "', '"
                                     (plist-get properties "DESCRIPTION" 'string-equal) "', '"
                                     (plist-get properties "NOTES" 'string-equal) "', '"
                                     (plist-get properties "KEYWORDS" 'string-equal) "');")))))))



;;;;;;;;;;;;;;;;;;;;;;;;;; ARCHIVE DATABASE ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsarchive-insert-read-only-line (str)
  "Inserts a line of read-only text, followed by a non-read-only newline."
  (insert (concat (propertize str 'read-only t 'face 'header-line) "\n")))

(defun bsarchive-entry-buffer-init (file-urls)
  "Displays the archive entry buffer and prompts user to add files to DB."
  (bsarchive-insert-read-only-line "ADD FILES TO ARCHIVE\n\nFILES (one per line):")
  (while file-urls
    (insert (concat (pop file-urls) "\n")))
  (insert "\n")
  (bsarchive-insert-read-only-line "ARTWORK IDS  (comma-separated, on a single line):")
  (save-excursion
    (insert "\n")
    (insert "\n")
    (bsarchive-insert-read-only-line "KEYWORDS (comma-separated, on a single line):")
    (insert "\n")
    (insert "\n")
    (bsarchive-insert-read-only-line "Use bsarchive-entry-buffer-submit to create these records in DB")))

(defun bsarchive-entry-buffer-submit ()
  "Submits information in the archive-entry buffer, creating new database records."
  (interactive)
  (let ((file-urls '())
        (artwork-ids "")
        (keywords ""))
    
    (save-excursion
      (goto-char (point-min))
      
      (let ((urls-begin (progn
                          (text-property-search-forward 'read-only)
                          (+ 1 (point))))
            (ids-begin (progn
                          (text-property-search-forward 'read-only)
                          (+ 1 (point))))
            (urls-end (- (line-beginning-position) 1))
            (kws-begin (progn
                          (text-property-search-forward 'read-only)
                          (+ 1 (point)))))

        ;; get file urls
        (narrow-to-region urls-begin urls-end)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (if (not (string-empty-p line))
                (push line file-urls)))
          (next-line))
        (widen)
        
        ;; get artwork IDs
        (goto-char ids-begin)
        (setq artwork-ids (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

        ;; get keywords
        (goto-char kws-begin)
        (setq keywords (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))

    (switch-to-buffer "*bsarchive-entry-submitted*")

    ;; enter records into DB
    (let ((db (sqlite-open bsarchive-database-file))
          (first-url (car file-urls))
          (rest (cdr file-urls))
          (report-strings '()))
      (while first-url
        ;; add new record to the database
        (sqlite-execute db (concat "INSERT INTO documents VALUES('" first-url "', '" artwork-ids "', '" keywords "');"))
        (let* ((uid (car (car (sqlite-execute db (concat "SELECT rowid FROM documents WHERE url='" first-url "';")))))
               (entry (sqlite-execute db (format "SELECT * FROM documents WHERE rowid=%s;" uid))))
          ;; add uid to file metadata
          (bsarchive-exif-add-keyword first-url (concat "bst_archive_uid_" (number-to-string uid)))
          ;; make report string
          (push (format "UID: %s\n%s\n\n" uid entry) report-strings))
        (setq first-url (car rest))
        (setq rest (cdr rest)))

      ;; report
    
      (insert "RECORDS ENTERED INTO DATABASE:\n\n")

      (while report-strings
        (insert (pop report-strings)))

      (sqlite-close db))))

(defun bsarchive-add-files (file-urls)
  "displays the archive-entry buffer for the files."
  (switch-to-buffer "*bsarchive-entry*")
  (bsarchive-entry-buffer-init file-urls))

(defun bsarchive-add-single-file (file-url)
  (interactive "fchoose a file to add to the archive database: ")
  (bsarchive-add-files (list file-url)))

(defun bsarchive-dired-add-files ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (bsarchive-add-files files)
      (message "no files marked"))))

(defun bsarchive-get-rowid-for-file (file-url)
  "Use this to test whether a file has already been added to the archive."
  (interactive "fchoose a file: ")
  (let* ((absolute-url (file-truename file-url))
         (ids nil)
         (db (sqlite-open bsarchive-database-file))
         ;; (db-urls (car (sqlite-select db "SELECT url FROM documents;"))))
         (db-urls (sqlite-select db "SELECT url FROM documents;")))
    ;; compare absolute urls
    (while db-urls
      (let ((next-url (car (pop db-urls))))
      ;; (let ((next-url (pop db-urls)))
        (when (string-equal absolute-url (file-truename next-url))
          (push
           (sqlite-select db (format "SELECT rowid FROM documents WHERE url=\"%s\";" next-url))
           ids))))
    
         ;; (ids (sqlite-select db (format "SELECT rowid FROM documents WHERE url=\"%s\";" file-url))))
    (sqlite-close db)
    (message "Found ids: %s" ids)
    ids))

(defun bsarchive-dired-mark-files-in-archive (dir)
  "Show directory in a dired buffer and mark all files which are
already added to the archive."
  (interactive "Dchoose directory: ")
  (let ((files (directory-files dir))
        (count 0))
    (while files
      (let ((f (pop files)))
        ;; (when (and (file-exists-p f)
        ;;            (not (file-directory-p)))
        (when (file-exists-p f)
          ;; (when (not (bsarchive-get-rowid-for-file f))
          (when (bsarchive-get-rowid-for-file f)
            (dired-mark-files-regexp (format "%s" (file-name-nondirectory f)))
            (setf count (+ 1 count))))))
    (message "%s files in archive" count)))

(defun bsarchive-dired-mark-files-not-in-archive (dir)
  "Show directory in a dired buffer and mark all files which are NOT
already added to the archive."
  (interactive "Dchoose directory: ")
  (let ((files (directory-files dir))
        (count 0))
    (while files
      (let ((f (pop files)))
        (when (and (file-exists-p f)
                   (not (file-directory-p f)))
        ;; (when (file-exists-p f)
          ;; (when (not (bsarchive-get-rowid-for-file f))
          (when (not (bsarchive-get-rowid-for-file f))
            (dired-mark-files-regexp (format "%s" (file-name-nondirectory f)))
            (setf count (+ 1 count))))))
    (message "%s files not in archive" count)))

(defun bsarchive-show-info-on-files (file-urls)
  (switch-to-buffer "*bsarchive-review*")
    (while file-urls
      (let* ((url (pop file-urls))
             (archive-ids (bsarchive-exif-get-uids url))
             (keywords '())
             (art-ids '()))
        
        (bsarchive-insert-read-only-line (concat "FILE: " url))
        (insert (format "ARCHIVE UIDs: %s\n\n" archive-ids))

        (when archive-ids
          (let ((db (sqlite-open bsarchive-database-file))
                (uid (car archive-ids)))

            (insert (format "ARCHIVE ENTRY: %s\n"
                            (sqlite-execute
                             db
                             (concat "SELECT * FROM documents WHERE rowid=" uid ";"))))

            (setq keywords
                  (sqlite-select db (concat "SELECT keywords FROM documents WHERE rowid=" uid ";")))

            (insert (format "KEYWORDS: %s\n" keywords))

            (setq art-ids
                  (sqlite-select db (concat "SELECT artwork_ids FROM documents WHERE rowid=" uid ";")))

            (insert (format "ARTWORK UIDs: %s\n\n" art-ids))
            
            (setq art-ids (pop art-ids))
            (while art-ids
              (let* ((id (pop art-ids))
                     (artwork-title '())
                     (artwork-date '())
                     (artwork-media '())
                     (artwork-description '())
                     (artwork-notes '())
                     (artwork-keywords '()))

                (bsarchive-insert-read-only-line "ARTWORK CATALOGUE ENTRY:")
                (insert (format "UID: %s\n" id))

                (setq artwork-title
                      (sqlite-select db (concat "SELECT title FROM artwork WHERE uid='" id "';")))
                (insert (format "TITLE: %s\n" artwork-title))

                (setq artwork-date
                      (sqlite-select db (concat "SELECT date FROM artwork WHERE uid='" id "';")))
                (insert (format "DATE: %s\n" artwork-date))

                (setq artwork-media
                      (sqlite-select db (concat "SELECT media FROM artwork WHERE uid='" id "';")))
                (insert (format "MEDIA: %s\n" artwork-media))

                (setq artwork-description
                      (sqlite-select db (concat "SELECT description FROM artwork WHERE uid='" id "';")))
                (insert (format "DESCRIPTION: %s\n" artwork-description))

                (setq artwork-notes
                      (sqlite-select db (concat "SELECT notes FROM artwork WHERE uid='" id "';")))
                (insert (format "NOTES: %s\n" artwork-notes))

                (setq artwork-keywords
                      (sqlite-select db (concat "SELECT keywords FROM artwork WHERE uid='" id "';")))
                (insert (format "KEYWORDS: %s\n" artwork-keywords)))))))))

(defun bsarchive-dired-show-file-info ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (if files
        (bsarchive-show-info-on-files files)
      (message "no files marked"))))

(defun bsarchive-show-images-of-artwork (artwork-uid)
  (interactive "sFind images for artwork UID: ")
  (let* ((db (sqlite-open bsarchive-database-file))
         (files (sqlite-select db (concat "SELECT url FROM documents WHERE artwork_ids LIKE '" artwork-uid "';"))))
    (switch-to-buffer "*bsarchive-image-results*")
    (while files
      (insert (concat (car (pop files)) "\n")))
    (iimage-mode t)))

(defun bsarchive-show-images-of-artwork-dired (artwork-uid)
  "Lists all images matching artwork UID in a dired buffer."
  (interactive "sFind images for artwork UID: ")
  (let* ((db (sqlite-open bsarchive-database-file))
         (db-output (sqlite-select db (concat "SELECT url FROM documents WHERE artwork_ids LIKE '" artwork-uid "';")))
         (files nil))
    (while db-output
      (let ((next-output (pop db-output)))
        (push (car next-output) files)))
    (dired files)))
