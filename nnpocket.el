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

(defvoo nnpocket-address nil
  "Address of the tt-rss server.")

(defvoo nnpocket-user nil
  "Username to use for authentication to the tt-rss server.")
(defvoo nnpocket-password nil
  "Password to use for authentication to the tt-rss server.")
(defvoo nnpocket-directory (nnheader-concat gnus-directory "ttrss/")
  "Where nnpocket will save its files.")
(defvoo nnpocket-fetch-partial-articles nil
  "If non-nil, nnpocket will fetch partial articles.")
(defvoo nnpocket-status-string "")

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
  (nnml-retrieve-headers 0 nnpocket-current-group 0 0)
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
