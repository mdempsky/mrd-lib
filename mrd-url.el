(require 'url)

(defconst mrd-url-scheme-alist
  '(("mrd-org-cmd" . mrd-org-cmd-open)))

(defun mrd-url-open (urls)
  "Open a list of URLs."
  (dolist (url urls)
    (message "URL: %s" url)
    (let* ((url (url-generic-parse-url url))
	   (scheme (url-type url))
	   (handler (assoc scheme mrd-url-scheme-alist)))
      (if handler
	  (funcall (cdr handler) url)
	(message "No handler for scheme \"%s\"." scheme)))))

(defun mrd-url-parse-path (path)
  (save-match-data
    (if (string-match (regexp-quote "?") path)
	(cons (substring path 0 (match-beginning 0))
	      (url-parse-query-string (substring path (match-end 0))))
      (cons path nil))))

(provide 'mrd-url)
