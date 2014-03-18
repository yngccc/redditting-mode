;;; reddit.el --- reddit in emacs

;; Author: JeremyC <yngccc@gmail.com>

;;; Commentary:

;;; Code:

(require 'cl)
(require 'url)
(require 'json)

(setq lexical-binding t)
(setq debug-on-error t)

(defvar reddit-url "http://www.reddit.com")

(defvar reddit-account-info-url (concat reddit-url "/api/me.json"))
(defvar reddit-login-url (concat reddit-url "/api/login"))

(defun reddit-subreddit-url (subreddit &optional order)
  (let* ((main-url (concat reddit-url "/r/" subreddit "/")))
    (concat main-url order ".json")))

(defvar reddit-listing-kind "Listing")
(defvar reddit-more-kind "more")
(defvar reddit-comment-kind "t1")
(defvar reddit-account-kind "t2")
(defvar reddit-link-kind "t3")
(defvar reddit-message-kind "t4")
(defvar reddit-subreddit-kind "t5")
(defvar reddit-award-kind "t6")
(defvar reddit-promo-kind "t8")

(defun reddit-get-kind (json)
  (cdr (assq 'kind json)))

(defun reddit-is-kind (json kind)
  (string= (reddit-get-kind json) kind))

(defun reddit-get-subreddit (subreddit &optional order)
  (let ((buffer (url-retrieve-synchronously (reddit-subreddit-url subreddit order))))
    (set-buffer buffer)
    (json-read-from-string (substring (buffer-string) (string-match "^$" (buffer-string))))))

(defun reddit-get-subreddit-property (subreddit property)
  (let ((subreddit (reddit-get-subreddit subreddit)))
    (cdr (assq property (cdr (assq 'data subreddit))))))

(reddit-get-subreddit-property "programming" 'children)

(defun reddit-get-subreddit-listing-property (subreddit property)
  (let ((listing (reddit-get-subreddit-property subreddit 'children)))
    (mapcar (lambda (elem) (cdr (assq property (cdr (assq 'data elem)))))
	    listing)))

(reddit-get-subreddit-listing-property "programming" 'id)
(reddit-get-subreddit-listing-property "programming" 'title)
(reddit-get-subreddit-listing-property "programming" 'ups)
(reddit-get-subreddit-listing-property "programming" 'downs)
(reddit-get-subreddit-listing-property "programming" 'score)

;;; reddit.el ends here

