;;; redditting-mode.el --- nuff said

;; Author: JeremyC <yngccc@gmail.com>

;;; Commentary:

;;; Code:

(require 'url)
(require 'json)

(defconst redditting-listing-kind "Listing")
(defconst redditting-more-kind "more")
(defconst redditting-comment-kind "t1")
(defconst redditting-account-kind "t2")
(defconst redditting-link-kind "t3")
(defconst redditting-message-kind "t4")
(defconst redditting-subreddit-kind "t5")
(defconst redditting-award-kind "t6")
(defconst redditting-promo-kind "t8")

(defconst redditting-url "http://www.reddit.com")
(defconst redditting-account-info-url (concat redditting-url "/api/me.json"))
(defconst redditting-login-url (concat redditting-url "/api/login"))

(defun redditting-kind (json)
  (cdr (assq 'kind json)))

(defun redditting-kindp (json kind)
  (string= (redditting-kind json) kind))

;; subreddit
(defun redditting-subreddit-url (subreddit &optional order)
  (let ((main-url (concat redditting-url "/r/" subreddit "/")))
    (concat main-url order ".json")))

(defun redditting-json (url)
  (set-buffer (url-retrieve-synchronously url))
  (json-read-from-string (substring (buffer-string) (string-match "^$" (buffer-string)))))
  
(defun redditting-subreddit-property (subreddit property)
  (let ((subreddit (redditting-json (redditting-subreddit-url subreddit))))
    (cdr (assq property (cdr (assq 'data subreddit))))))

(defun redditting-subreddit-listing-property (subreddit property)
  (let ((listing (redditting-subreddit-property subreddit 'children)))
    (mapcar (lambda (elem) (cdr (assq property (cdr (assq 'data elem)))))
	    listing)))

;; comments
(defun redditting-subreddit-comments-url (subreddit article &optional sort)
  (let ((main-url (concat redditting-url "/r/" subreddit "/comments/")))
    (concat main-url article ".json?sort=" sort)))

(defun redditting-article-comments (subreddit article &optional sort)
  (let ((comments (redditting-json (redditting-subreddit-comments-url subreddit article sort))))
    comments))
    
;; tests
(redditting-article-comments "programming" "20qojw")
(redditting-subreddit-listing-property "programming" 'id)
(redditting-subreddit-listing-property "programming" 'title)
(redditting-subreddit-listing-property "programming" 'num_comments)
(redditting-subreddit-listing-property "programming" 'ups)
(redditting-subreddit-listing-property "programming" 'downs)
(redditting-subreddit-listing-property "programming" 'score)

(defun redditting-display-subreddit (&optional subreddit)
  (let* ((subreddit (if subreddit subreddit "all"))
	 (titles (redditting-subreddit-listing-property subreddit 'title))
	 (scores (redditting-subreddit-listing-property subreddit 'score)))
    (set-buffer "*redditting-mode*")
    (mapc #'insert (mapcar (lambda (title) (concat title "\n")) titles))
    (mapc #'insert (mapcar (lambda (score) (concat (number-to-string score) "\n")) scores))))

(defun redditting-mode ()
  (interactive)
  (with-current-buffer (get-buffer-create "*redditting-mode*")
    (switch-to-buffer-other-window "*redditting-mode*")
    (read-only-mode -1)
    (erase-buffer)
    (redditting-display-subreddit)
    (read-only-mode 1)))

(provide 'redditting-mode)

;;; redditting-mode.el ends here
