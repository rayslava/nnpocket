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
(require 'oauth2)
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

(defvoo nnpocket-directory (nnheader-concat gnus-directory "ttrss/")
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

(defvar access-code "" "OAuth access code")

(defvar access-token "" "OAuth access token")

(defvar articles nil)

(defun pocket-request-access-code ()
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'consumer_key consumer-key query-data)
    (puthash 'redirect_uri "nnpocket:authorizationFinished" query-data)
    (web-http-post
     (lambda (con header data)
       (setf access-code (s-chop-prefix "code=" data)))
     :url (concat api-server request-endpoint)
     :data query-data)))

(defun pocket-request-access-token ()
  (let ((query-data (make-hash-table :test 'equal)))
    (puthash 'code access-code query-data)
    (puthash 'consumer_key consumer-key query-data)
    (web-http-post
     (lambda (con header data)
       (setf access-token (s-chop-prefix "access_token="
					 (car (s-split "&" data))))
       (message "Received data: %s" data))
     :url (concat api-server authorize-endpoint)
     :data query-data)))

(pocket-login)

;;; Login sequence
(defun pocket-login ()
  ;; Get `access-code'
  (pocket-request-access-code)
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
		      "?request_token=" access-code
		      "&redirect_uri=" (url-hexify-string "http://localhost:22334/"))))


(defun pocket-auth-filter (proc chunk)
  (process-send-string proc "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n\nAuthentication succeeded")
  (process-send-eof proc)
  ;; Get `access-token'
  (pocket-request-access-token)
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
    (puthash 'access_token access-token query-data)
    (puthash 'count "3" query-data)
    (puthash 'detailType "single" query-data)
    (web-http-post
     (lambda (con header data)
       (setf articles data))
     :url (concat api-server "/get")
     :data query-data)))

(pocket-get-list)

(defun nnpocket-retrieve-headers (articles &optional group server fetch-old)
  "Read list of articles"
  (pocket-get-list)
  (let ((arts (json-read-from-string articles)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (article articles)
	(insert (nnpocket--format-header article group))))
    'nov)
  )

(print (json-read-from-string articles))

(deffoo nnttrss-retrieve-headers (articles &optional group server fetch-old)

(nnpocket-retrieve-headers 5)

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

;;; Interface functions.

(nnoo-define-basics nnpocket)

(deffoo nnpocket-open-server (server &optional defs)
  (setq nnpocket-directory
	(or (cadr (assq 'nnpocket-directory defs))
	    server))
  (unless (assq 'nnpocket-directory defs)
    (push `(nnpocket-directory ,server) defs))
  (push `(nnpocket-current-group
	  ,(file-name-nondirectory
	    (directory-file-name nnpocket-directory)))
	defs)
  (push `(nnpocket-top-directory
	  ,(file-name-directory (directory-file-name nnpocket-directory)))
	defs)
  (nnoo-change-server 'nnpocket server defs))

(nnoo-map-functions nnpocket
  (nnpocket-retrieve-headers 0 nnpocket-current-group 0 0)
  (nnmh-request-article 0 nnpocket-current-group 0 0)
  (nnmh-request-group nnpocket-current-group 0 0)
  (nnmh-close-group nnpocket-current-group 0))

(nnoo-import nnpocket
  (nnmh
   nnmh-status-message
   nnmh-request-list
   nnmh-request-newgroups))

(provide 'nnpocket)
;;; nnpocket.el ends here
