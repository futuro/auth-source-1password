;;; auth-source-1password.el --- 1password integration for auth-source -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Dominick LoBraico
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Dominick LoBraico <auth-source-1password@lobrai.co>
;; Created: 2023-04-09
;; URL: https://github.com/dlobraico

;; Package-Requires: ((emacs "24.4"))

;; Version: 0.0.1

;;; Commentary:
;; This package adds 1password support to auth-source by calling the op CLI.
;; Heavily inspired by the auth-source-gopass package
;; (https://github.com/triplem/auth-source-gopass)

;;; Code:
(require 'auth-source)

(defgroup auth-source-1password nil
  "1password auth source settings."
  :group 'auth-source
  :tag "auth-source-1password"
  :prefix "1password-")

(defcustom auth-source-1password-vault "Personal"
  "1Password vault to use when searching for secrets."
  :type 'string
  :group 'auth-source-1password)

(defcustom auth-source-1password-executable "op"
  "Executable used for 1password."
  :type 'string
  :group 'auth-source-1password)

(defcustom auth-source-1password-construct-secret-reference 'auth-source-1password--1password-construct-query-path
  "Function to construct the query path in the 1password store."
  :type 'function
  :group 'auth-source-1password)

(defun auth-source-1password--1password-construct-query-path (_backend _type host user _port)
  "Construct the full entry-path for the 1password entry for HOST and USER.
Usually starting with the `auth-source-1password-vault', followed
by host and user."
  (mapconcat #'identity (list auth-source-1password-vault host user) "/"))

;; TODO: The current approach of requiring the hostname be the item name is both limiting, and also
;; makes the UX worse for me in 1Password, so I'd like to move to something else, though I'm not
;; exactly sure what yet.
;;
;; 1Password's CLI doesn't have a `search' command, and as such I'm rolling a couple ideas around in
;; my head.
;; 1. Fetch every item in a vault as JSON and do filtering after the fact.
;; 2. Define a tagging specification for items in 1Password that are made up of the search spec
;;    fields, and apply those to the item I care about
(cl-defun auth-source-1password-search (&rest spec
                                           &key backend type host user port
                                           &allow-other-keys)
  "Search 1password for the specified user and host.
SPEC, BACKEND, TYPE, HOST, USER and PORT are required by auth-source."
  (if (executable-find auth-source-1password-executable)
      (let ((got-secret
             (string-trim
              (shell-command-to-string
               (format "%s item get %s --vault %s --fields label=%s --reveal"
                       auth-source-1password-executable
                       (shell-quote-argument host)
                       (shell-quote-argument auth-source-1password-vault)
                       (shell-quote-argument user))))))
        (list (list :user user
                    :secret got-secret)))
    ;; If not executable was found, return nil and show a warning
    (warn "`auth-source-1password': Could not find executable '%s' to query 1password" auth-source-1password-executable)))

;;;###autoload
(defun auth-source-1password-enable ()
  "Enable the 1password auth source."
  (add-to-list 'auth-sources '1password)
  (auth-source-forget-all-cached))

(defvar auth-source-1password-backend
  (auth-source-backend
   :source "."
   :type 'password-store
   :search-function #'auth-source-1password-search))

(defun auth-source-1password-backend-parse (entry)
  "Create a 1password auth-source backend from ENTRY."
  (when (eq entry '1password)
    (auth-source-backend-parse-parameters entry auth-source-1password-backend)))

(if (boundp 'auth-source-backend-parser-functions)
    (add-hook 'auth-source-backend-parser-functions #'auth-source-1password-backend-parse)
  (advice-add 'auth-source-backend-parse :before-until #'auth-source-1password-backend-parse))

(provide 'auth-source-1password)
;;; auth-source-1password.el ends here
