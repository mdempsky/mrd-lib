(require 'url)
(require 'mrd-url)

(defconst mrd-org-cmd-handler-alist
  '(("capture" . mrd-org-cmd-capture)
    ("store-link" . mrd-org-cmd-store-link)))

(defun mrd-org-cmd-capture (args)
  "Process a mrd-org-cmd:capture?... style URL."
  (let ((uri (assoc "href" args))
	(title (assoc "title" args))
	(text (assoc "text" args)))
    (if (and uri title)
	(mrd-org-cmd-do-capture (cadr uri) (cadr title)
				     (if text (cadr text) nil)))))

(defun mrd-org-cmd-do-capture (uri title text)
  (if (boundp 'org-stored-links)
      (push (list uri title) org-stored-links))
  (let ((orglink (org-make-link-string uri title))
	(scheme (url-type (url-generic-parse-url uri))))
    (kill-new orglink)
    (org-store-link-props :type scheme
			  :link uri
			  :description title
			  :annotation orglink
			  :initial text)
    (raise-frame)
    (org-capture)))

(defun mrd-org-cmd-store-link (args)
  "Process a mrd-org-cmd:store-link?href=...&title=... style URL."
  (let ((uri (assoc "href" args))
	(title (assoc "title" args)))
    (if (and uri title)
	(mrd-org-cmd-do-store-link (cadr uri) (cadr title)))))

(defun mrd-org-cmd-do-store-link (uri title)
  (if (boundp 'org-stored-links)
      (push (list uri title) org-stored-links))
  (kill-new uri)
  (message "`%s' to insert new org-link, `%s' to insert `%s'"
	   (substitute-command-keys"\\[org-insert-link]")
	   (substitute-command-keys"\\[yank]")
	   uri)
  nil)

(defun mrd-org-cmd-open (url)
  (let* ((data (mrd-url-parse-path (url-filename url)))
	 (path (car data))
	 (args (cdr data))
	 (handler (assoc path mrd-org-cmd-handler-alist)))
    (message "path: %s, args: %s" path args)
    (if handler
	(funcall (cdr handler) args)
      (message "No handler for path \"%s\"." path))))

(provide 'mrd-org-cmd)
