;;; redditting-mode.el --- nuff said

;; Author: JeremyC <yngccc@gmail.com>

;;; Commentary:

;;; Code:

(require 'url)
(require 'json)

(defconst redditting-url "http://www.reddit.com")
(defconst redditting-account-info-url (concat redditting-url "/api/me.json"))
(defconst redditting-login-url (concat redditting-url "/api/login"))

(defconst redditting-listing-kind "Listing")
(defconst redditting-more-kind "more")
(defconst redditting-comment-kind "t1")
(defconst redditting-account-kind "t2")
(defconst redditting-link-kind "t3")
(defconst redditting-message-kind "t4")
(defconst redditting-subreddit-kind "t5")
(defconst redditting-award-kind "t6")
(defconst redditting-promo-kind "t8")

(defun redditting-get-kind (json)
  (cdr (assq 'kind json)))

(defun redditting-is-kind (json kind)
  (string= (redditting-get-kind json) kind))

(defun redditting-subreddit-url (subreddit &optional order)
  (let* ((main-url (concat redditting-url "/r/" subreddit "/")))
    (concat main-url order ".json")))

(defun redditting-get-subreddit (subreddit &optional order)
  (let ((buffer (url-retrieve-synchronously (redditting-subreddit-url subreddit order))))
    (set-buffer buffer)
    (json-read-from-string (substring (buffer-string) (string-match "^$" (buffer-string))))))

(defun redditting-get-subreddit-property (subreddit property)
  (let ((subreddit (redditting-get-subreddit subreddit)))
    (cdr (assq property (cdr (assq 'data subreddit))))))

(redditting-get-subreddit-property "programming" 'children)

(defun redditting-get-subreddit-listing-property (subreddit property)
  (let ((listing (redditting-get-subreddit-property subreddit 'children)))
    (mapcar (lambda (elem) (cdr (assq property (cdr (assq 'data elem)))))
	    listing)))

;; tests
(redditting-get-subreddit-listing-property "programming" 'id)
(redditting-get-subreddit-listing-property "programming" 'title)
(redditting-get-subreddit-listing-property "programming" 'num_comments)
(redditting-get-subreddit-listing-property "programming" 'ups)
(redditting-get-subreddit-listing-property "programming" 'downs)
(redditting-get-subreddit-listing-property "programming" 'score)

(defun redditting-mode ()
  (error "work in progress"))

(provide 'redditting-mode)

;;; redditting-mode.el ends here

