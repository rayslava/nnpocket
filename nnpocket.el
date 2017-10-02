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
(require 'nnmh)
(require 'nnml)
(require 'nnoo)
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

(defvoo nnpocket-nov-is-evil nil
  "*Non-nil means that nnpocket will never retrieve NOV headers."
  nnml-nov-is-evil)

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

(defvar nnpocket--article-map nil "List of downloaded articles")

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
    (puthash 'count "5" query-data)
    (puthash 'detailType "single" query-data)
    (web-http-post
     (lambda (con header data)
       (setf nnpocket--article-map
	     (alist-get 'list (json-read-from-string data))))
     :url (concat api-server "/get")
     :data query-data)))

(deffoo nnpocket-retrieve-headers (articles &optional group server fetch-old)
  "Read list of articles"
  (let* ((headers nnpocket--article-map))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (header headers)
	(insert
	 (format "%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\t%s\t%S\n"
		 (car header)
		 (alist-get 'resolved_title (cdr header))
		 (url-host (url-generic-parse-url (alist-get 'given_url (cdr header))))
		 (format-time-string "%a, %d %b %Y %T %z"
				     (seconds-to-time (string-to-number (alist-get 'time_updated (cdr header)))))
		 (format "<%s@%s.nnttrss>" (alist-get 'item_id (cdr header)) "nnpocket")
		""
		-1
		-1
		""
		nil))))
    'nov))

(deffoo nnpocket-request-article (article &optional group server to-buffer)
  (let ((destination (or to-buffer nntp-server-buffer))
	(article (alist-get (intern (number-to-string article)) nnpocket--article-map)))
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
	(w3m-region start (point)))))
    (cons article buffer))

(deffoo nnpocket-close-group (group &optional server)
  t)

(deffoo nnpocket-open-server (server &optional defs)
  (if (nnpocket-server-opened server)
      t
    (pocket-login)))

(deffoo nnpocket-server-opened (&optional server)
  (not (string-empty-p nnpocket--access-token)))

(deffoo nnpocket-request-close ()
  t)

(deffoo nnpocket-request-group (group &optional server fast info)
  (when (eql nnpocket--article-map nil)
    (pocket-get-list))
  (if fast
      t
    (let* ((id 1)
	   (article-ids (mapcar 'car nnpocket--article-map))
	   (article-id-nums (mapcar (lambda (e) (string-to-number (symbol-name e))) article-ids))
	   (total-articles (length nnpocket--article-map)))
      (format "211 %d %d %d \"%s\"\n"
	      total-articles
	      (apply 'min article-id-nums)
	      (apply 'max article-id-nums)
	      (nnimap-encode-gnus-group group)))))

(deffoo nnpocket-request-list (&optional server)
  (nnpocket-open-server "pocket")
  (when (eql nnpocket--article-map nil)
    (pocket-get-list))
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let*  ((article-ids (mapcar 'car nnpocket--article-map))
	    (article-id-nums (mapcar (lambda (e) (string-to-number (symbol-name e))) article-ids))
	    (total-articles (length nnpocket--article-map)))
      (if article-ids
	  (insert (format "\"%s\" %d %d y\n"
			  "pocket"
			  (apply 'max article-id-nums)
			  (apply 'min article-id-nums)))
	(insert (format "\"%s\" 0 1 y\n" "pocket"))))
    t))

(defvar nnpocket--sid nil
  "Current session id, if any, set after successful login.")

(defvar nnpocket--api-level nil
  "API version level, increased with each API functionality change.")

(defvar nnpocket--server-version nil
  "Server version number.")

(defvar nnpocket--headlines nil
  "List of all headline propertly lists.")

(defvar nnpocket--last-article-id 0
  "Internal server ID of last article nnpocket knows about.")

(defvar nnpocket--article-map nil
  "Property list of association lists.")

(defvar nnpocket--feeds nil
  "List of all feed property lists.")

(provide 'nnpocket)
;;; nnpocket.el ends here
