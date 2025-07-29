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
(require 'cl-lib)
(require 'url)
(require 'dash)

(defgroup auth-source-1password nil
  "1password auth source settings."
  :group 'auth-source
  :tag "auth-source-1password"
  :prefix "1password-")

(defcustom 1pass-vault "Personal"
  "1Password vault to use when searching for secrets."
  :type 'string
  :group 'auth-source-1password)

(defcustom 1pass-executable "op"
  "Executable used for 1password."
  :type 'string
  :group 'auth-source-1password)

(defun 1pass--list-items-in-vault (vault)
  (-> "%s item list --vault %s --format json"
      (format 1pass-executable
              (shell-quote-argument 1pass-vault))
      (shell-command-to-string)
      ;; We keep the default behavior for array conversion, as `json-serialize' -- used when getting
      ;; vault item details -- and friends will try to treat the array as a plist, which would
      ;; create the wrong type of JSON element (and also it throws an exception/warning).
      (json-parse-string :object-type 'plist :array-type 'array)))

(defun 1pass--do-debug (&rest msg)
  "Call `auth-source-do-debug' with MSG and a prefix."
  (apply #'auth-source-do-debug
         (cons (concat "auth-source-1password: " (car msg))
               (cdr msg))))

(defun 1pass--do-trivia (&rest msg)
  "Call `auth-source-do-trivia' with MSG and a prefix."
  (apply #'auth-source-do-trivia
         (cons (concat "auth-source-1password: " (car msg))
               (cdr msg))))

(defun 1pass--update-plist-property (plist property f)
  "TODO: write a docstring"
  (plist-put plist property (funcall f (plist-get plist property))))

(defun auth-source-1password--item-has-host? (item host)
  (->> (plist-get item :urls)
       (--some? (string-equal host (plist-get it :href)))))

(defun auth-source-1password--all-items-for-host (items host)
  (->> items
       (--filter (auth-source-1password--item-has-host? it host))))

(defun auth-source-1password--first-item-for-host (items host)
  (->> items
       (--first (auth-source-1password--item-has-host? it host))))

;; TODO: update this to also take a list of `op item list' item plists (turned into JSON first)
;; N.B. the `op item get' command only takes a list of json objects on stdin
(defun auth-source-1password--op-get-item (item-id)
  (-> (format "%s item get %s --vault %s --format json"
              auth-source-1password-executable
              (shell-quote-argument item-id)
              (shell-quote-argument auth-source-1password-vault))
      (shell-command-to-string)
      (json-parse-string :object-type 'plist :array-type 'list)
      (auth-source-1password--obfuscate-concealed-fields)))

(defun auth-source-1password--concealed-field? (field-plist)
  (string-equal "CONCEALED" (plist-get field-plist :type)))

(defun auth-source-1password--obfuscate-field-value (field-plist)
  (plist-put field-plist :value
             (let* ((v (auth-source--obfuscate (plist-get field-plist :value))))
               (lambda ()
                 (auth-source--deobfuscate v)))))

(defun auth-source-1password--obfuscate-concealed-fields (item-plist)
  (auth-source-1password--update-plist-property
   item-plist
   :fields
   (lambda (fields)
     (-map-when 'auth-source-1password--concealed-field?
                'auth-source-1password--obfuscate-field-value
                fields))))

(defun auth-source-1password--do-debug (&rest msg)
  "Call `auth-source-do-debug' with MSG and a prefix."
  (apply #'auth-source-do-debug
         (cons (concat "auth-source-1password: " (car msg))
               (cdr msg))))

;; (->>
;;  (-> (auth-source-1password--op-get-item "vjg2k2a3vrkynyqnwm4mkrlpqu")
;; (auth-source-1password--update-plist-property
;;       :fields
;;       (lambda (fields)
;;         (-map-when 'auth-source-1password--concealed-field?
;;                    'auth-source-1password--obfuscate-field-value
;;                    fields)))
;;      (plist-get :fields))
;;  (-filter 'auth-source-1password--concealed-field?)
;;  (--map (plist-get it :value))
;;  (-map 'funcall)
;;  )

;; (defvar foobarbaz nil)

;; (->> (auth-source-1password--obfuscate-field-value '(:value "foobarbaz"))
;;      (auth-source--aput 'foobarbaz 'ttt)
;;      )

;; (lambda (x) "foo")

;; (-> (auth-source-1password--list-items-in-vault auth-source-1password-vault)
;;     (auth-source-1password--first-item-for-host "smtp.gmail.com")
;;     (plist-get :id)
;;     (auth-source-1password--op-get-item))

;; (-> (-first #'auth-source-1password--concealed-field?
;;          (-> (auth-source-1password--op-get-item "vjg2k2a3vrkynyqnwm4mkrlpqu")
;;              (plist-get :fields)))
;;     (plist-get :value)
;;     (funcall))

;; TODO: The current approach of requiring the hostname be the item name is both limiting, and also
;; makes the UX worse for me in 1Password, so I'd like to move to something else, though I'm not
;; exactly sure what yet.
;;
;; 1Password's CLI doesn't have a `search' command, and as such I'm rolling a couple ideas around in
;; my head.
;; 1. Fetch every item in a vault as JSON and do filtering after the fact.
;; 2. Define a tagging specification for items in 1Password that are made up of the search spec
;;    fields, and apply those to the item I care about

;; (->> (auth-source-1password--list-items-in-vault auth-source-1password-vault)
;;      (--filter (string-equal-ignore-case (plist-get it :category) "login")))

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
(defun 1pass-enable ()
  "Enable the 1password auth source."
  (add-to-list 'auth-sources '1password)
  (auth-source-forget-all-cached))

(defvar 1pass-backend
  (auth-source-backend
   :source "."
   :type '1password
   :search-function #'1pass-search))

(defun 1pass-backend-parse (entry)
  "Create a 1password auth-source backend from ENTRY."
  (when (eq entry '1password)
    (auth-source-backend-parse-parameters entry 1pass-backend)))

(if (boundp 'auth-source-backend-parser-functions)
    (add-hook 'auth-source-backend-parser-functions #'1pass-backend-parse)
  (advice-add 'auth-source-backend-parse :before-until #'1pass-backend-parse))

(provide 'auth-source-1password)
;;; auth-source-1password.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("1pass-" . "auth-source-1password-"))
;; End:
