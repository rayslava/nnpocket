;;; nnpocket.el --- Pocket (https://getpocket.com) backend for Gnus
;; Copyright (C) 2017 Slava Barinov

;; Author: Slava Barinov <rayslava@gmail.com>
;; Keywords: pocket list
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The package is a Gnus backend for Pocket service

;;; -*- lexical-binding: t; -*-
;;; Code:

(require 'nnheader)
(require 'gnus)
(require 'nnmh)
(require 'nnml)
(require 'nnoo)
(require 'nnrss)
(require 'json)
(require 'web)
(require 's)
(eval-when-compile (require 'cl))

(nnoo-declare nnpocket)
(nnoo-define-basics nnpocket)
(gnus-declare-backend "nnpocket" 'news 'address)

(defvoo nnpocket-directory nil
  "Where nnpocket will look for groups."
  nnml-current-directory nnmh-current-directory)

(defvoo nnpocket-directory (nnheader-concat gnus-directory "nnpocket/")
  "Where nnpocket will save its files.")

(defvoo nnpocket-fetch-partial-articles nil
  "If non-nil, nnpocket will fetch partial articles.")

(defvoo nnpocket-status-string "")

(defconst api-server "https://getpocket.com/v3" "Pocket API server")

(defconst auth-server "https://getpocket.com" "Pocket Auth server")

(defconst request-endpoint   "/oauth/request" "Pocket API server")

(defconst authorize-endpoint "/oauth/authorize" "Pocket API server")

(defconst articles-endpoint "/get" "Pocket API server")

(defconst consumer-key "70702-7b3f302ffbab1b8e1e7410a9" "OAuth consumer key token")

(defvar nnpocket--access-code "" "OAuth access code")

(defvar nnpocket--access-token "" "OAuth access token")

(defvar nnpocket--headlines nil "List of downloaded articles")

(defvar nnpocket--article-map nil
  "Property list of association lists.
The properties are group name strings.  Values are association
lists of Pocket IDs  to article numbers.")

(defvar nnpocket--id-map nil "Property list of GNUS ids vs Pocket ids")

(defvar nnpocket--last-article-id 0 "Pocket ID of last article nnpocket knows about.")

(defun nnpocket-decode-gnus-group (group)
  (decode-coding-string group 'utf-8))

(defun nnpocket-encode-gnus-group (group)
  (encode-coding-string group 'utf-8))

(defun parse-articles (json)
  "Parse `json' answer from Pocket server and create appropriate `nnpocket--headlines'"
  (let ((articles (alist-get 'list (json-read-from-string json))))
    (nnpocket--update-headlines articles)
    (nnpocket--update-article-map)))

(defun pocket-request-nnpocket--access-code ()
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'consumer_key consumer-key query-data)
    (puthash 'redirect_uri "nnpocket:authorizationFinished" query-data)
    (web-http-post
     (lambda (con header data)
       (setf nnpocket--access-code (s-chop-prefix "code=" data)))
     :url (concat api-server request-endpoint)
     :data query-data)))

(defun pocket-request-nnpocket--access-token ()
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'code nnpocket--access-code query-data)
    (puthash 'consumer_key consumer-key query-data)
    (web-http-post
     (lambda (con header data)
       (setf nnpocket--access-token (s-chop-prefix "access_token="
						   (car (s-split "&" data))))
       (message "Received data: %s" data))
     :url (concat api-server authorize-endpoint)
     :data query-data)))

;;; Login sequence
(defun pocket-login ()
  ;; Get `nnpocket--access-code'
  (pocket-request-nnpocket--access-code)
  ;; Start listener proects sfor oauth
  (make-network-process :name "pocket-auth-server"
			:buffer "*pocket-auth-server*"
			:server 't
			:service 22334
			:family 'ipv4
			:filter 'pocket-auth-filter
			:sentinel 'pocket-auth-sentinel)

  ;; Get user approval
  (browse-url (concat "https://getpocket.com/auth/authorize" authorize-endpoint
		      "?request_token=" nnpocket--access-code
		      "&redirect_uri=" (url-hexify-string "http://localhost:22334/"))))

(defun pocket-auth-filter (proc chunk)
  (process-send-string proc "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n\nAuthentication succeeded")
  (process-send-eof proc)
  ;; Get `nnpocket--access-token'
  (pocket-request-nnpocket--access-token)
  (delete-process "pocket-auth-server"))

(defun pocket-auth-sentinel (proc msg)
  (pocket-auth-log (format "Request <%s> from %s" msg proc)))

(defun pocket-auth-log (string &optional client)
  "If a *echo-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*pocket-auth-server*")
      (with-current-buffer "*pocket-auth-server*"
	(goto-char (point-max))
	(insert (current-time-string)
		(if client (format " %s:" client) " ")
		string)
	(or (bolp) (newline)))))

;;; Get count
(defun pocket-get-list ()
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'consumer_key consumer-key query-data)
    (puthash 'access_token nnpocket--access-token query-data)
    (puthash 'count "20" query-data)
    (puthash 'detailType "single" query-data)
    (web-http-post
     (lambda (con header data)
       (parse-articles data))
     :url (concat api-server "/get")
     :data query-data)))

(deffoo nnpocket-retrieve-headers (articles &optional group server fetch-old)
  "Read list of articles"
  (when group
    (setq group (nnpocket-decode-gnus-group group)))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (dolist (article articles)
      (let* ((articles (lax-plist-get nnpocket--article-map group))
	     (pocket-id (car (rassoc article articles)))
	     (header (alist-get (intern (number-to-string pocket-id)) nnpocket--headlines)))
	(insert
	 (format "%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%S\n"
		 article
		 (alist-get 'resolved_title (cdr header))
		 (url-host (url-generic-parse-url (alist-get 'given_url (cdr header))))
		 (format-time-string "%a, %d %b %Y %T %z"
				     (seconds-to-time (string-to-number (alist-get 'time_updated (cdr header)))))
		 (format "<%s@%s.nnpocket>" (alist-get 'item_id (cdr header)) "nnpocket")
		 ""
		 -1
		 -1
		 ""
		 nil))))
    'nov))

(deffoo nnpocket-request-article (article &optional group server to-buffer)
  (when group
    (setq group (nnpocket-decode-gnus-group group)))
  (let* ((destination (or to-buffer nntp-server-buffer))
	 (articles (lax-plist-get nnpocket--article-map group))
	 (pocket-id (car (rassoc article articles))))
    (let ((article (alist-get (intern (number-to-string pocket-id)) nnpocket--headlines)))
      (with-current-buffer destination
	(erase-buffer)
	(insert (format "Newgroups: %s\nSubject: %s\nFrom: %s\nDate: %s\n\n"
			"Pocket"
			(alist-get 'resolved_title article)
			(url-host (url-generic-parse-url (alist-get 'given_url article)))
			(format-time-string "%a, %d %b %Y %T %z"
					    (seconds-to-time (string-to-number (alist-get 'time_updated article))))))
	(let ((start (point)))
	  (insert (alist-get 'excerpt article))
	  (w3m-region start (point))))))
  (cons article buffer))

(deffoo nnpocket-close-group (group &optional server)
  t)

(deffoo nnpocket-open-server (server &optional defs)
  (if (nnpocket-server-opened server)
      t
    (when (not nnpocket--access-code)
      (pocket-login))
    (nnpocket--read-headlines)
    (nnpocket--read-article-map)))

(deffoo nnpocket-server-opened (&optional server)
  (not (string-empty-p nnpocket--access-token)))

(deffoo nnpocket-request-close ()
  t)

(deffoo nnpocket-request-group (group &optional server fast info)
  (setq group (nnpocket-decode-gnus-group group))
  (when (eql nnpocket--headlines nil)
    (pocket-get-list))
  (message "Inserting: %s" fast)
  (if fast
      t
    (with-current-buffer nntp-server-buffer
      (let* ((article-ids (mapcar #'cdr (lax-plist-get nnpocket--article-map group)))
	     (total-articles (length article-ids)))
	(erase-buffer)
	(insert (format "211 %d %d %d \"%s\"\n"
			total-articles
			(apply 'min article-ids)
			(apply 'max article-ids)
			(nnpocket-encode-gnus-group group)))
	t))))

(deffoo nnpocket-request-list (&optional server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let*  ((article-ids (mapcar #'cdr (lax-plist-get nnpocket--article-map "pocket"))))
      (if article-ids
	  (insert (format "\"%s\" %d %d y\n"
			  "pocket"
			  (apply 'max article-ids)
			  (apply 'min article-ids)))
	(insert (format "\"%s\" 0 1 y\n" "pocket"))))
    t))

(defun nnpocket--update-single-article-map (article-id group)
  "Add ARTICLE-ID in GROUP to 'nnpocket--article-map'."
  (if (not (lax-plist-get nnpocket--article-map group))
      (setq nnpocket--article-map
	    (lax-plist-put nnpocket--article-map group `((,article-id . 1))))
    (let ((mapping (lax-plist-get nnpocket--article-map group)))
      (unless (assoc article-id mapping)
	(let* ((last-artno (cdar mapping))
	       (next-artno (+ 1 (or last-artno 0)))
	       (mapping (cons `(,article-id . ,next-artno) mapping)))
	  (setq nnpocket--article-map
		(lax-plist-put nnpocket--article-map group mapping)))))))

(defun nnpocket--update-article-map ()
  "Update 'nnpocket--article-map' with new articles in 'nnpocket--headlines'."
  (dolist (headline (mapcar 'cdr nnpocket--headlines))
    (let* ((article-id (string-to-number (alist-get 'item_id headline)))
	   (group "pocket"))
      (when (> article-id nnpocket--last-article-id)
	(nnpocket--update-single-article-map article-id group))))
  (setq nnpocket--last-article-id (apply 'max
					 (mapcar
					  (lambda (e)
					    (string-to-number (symbol-name (car e))))
					  nnpocket--headlines)))
  (nnpocket--write-article-map)
  (nnpocket--write-headlines))

(defun nnpocket--update-headlines (articles)
  (dolist (new articles)
    (when (not (assoc (car new) nnpocket--headlines))
      (setq nnpocket--headlines
	    (append nnpocket--headlines (list new))))))

(defun nnpocket--read-vars (&rest vars)
  "Read VARS from local file in 'nnpocket-directory'.
Sets the variables VARS'."
  (dolist (var vars)
					;(setf (symbol-value var) nil)
    (let* ((name (symbol-name var))
	   (file (nnpocket-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (coding-system-for-read mm-universal-coding-system))
      (when (file-exists-p file)
	(load file nil t t)))))

(defun nnpocket--write-vars (&rest vars)
  "Write VARS from memory to local file in 'nnpocket-directory'.
Assumes the variables VARS are set."
  (gnus-make-directory nnpocket-directory)
  (dolist (var vars)
    (let* ((name (symbol-name var))
	   (file (nnpocket-make-filename name))
	   (file-name-coding-system nnmail-pathname-coding-system)
	   (coding-system-for-write mm-universal-coding-system))
      (with-temp-file (nnpocket-make-filename name)
	(insert (format ";; -*- coding: %s; -*-\n"
			mm-universal-coding-system))
	(let ((value (symbol-value var)))
	  (if (listp value)
	      (gnus-prin1 `(setq ,var ',value))
	    (gnus-prin1 `(setq ,var ,value))))
	(insert "\n")))))

(defun nnpocket-make-filename (name)
  "Build filename based on NAME in 'nnpocket-directory'."
  (expand-file-name
   (nnrss-translate-file-chars
    (concat name ".el"))
   nnpocket-directory))

(defun nnpocket--read-article-map ()
  "Read articles mapping file in 'nnpocket-directory'.
Sets the variables 'nnpocket--headlines and
'nnpocket--last-article-id'."
  (nnpocket--read-vars 'nnpocket--article-map 'nnpocket--last-article-id))

(defun nnpocket--write-article-map ()
  "Write article map from memory to local file in 'nnpocket-directory'.
Assumes the variables 'nnpocket--headlines' and
'nnpocket--last-article-id' are set."
  (nnpocket--write-vars 'nnpocket--article-map 'nnpocket--last-article-id))

(defun nnpocket--read-headlines ()
  "Read headlines from local file in 'nnpocket-directory'.
Sets the variables 'nnpocket--headlines'."
  (nnpocket--read-vars 'nnpocket--headlines))

(defun nnpocket--write-headlines ()
  "Write headlines from memory to local file in 'nnpocket-directory'.
Assumes the variable 'nnpocket--headlines' is set."
  (nnpocket--write-vars 'nnpocket--headlines))

(provide 'nnpocket)
;;; nnpocket.el ends here
