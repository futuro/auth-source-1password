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
(require 'seq)

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

(defun 1pass--item-has-host? (item host)
  (->> (plist-get item :urls)
       (seq-some (lambda (elt) (string-equal host (plist-get elt :href))))))

(defun 1pass--all-items-for-host (items host)
  (seq-into (->> items
                 (seq-filter (lambda (elt) (1pass--item-has-host? elt host))))
            'vector))

(defun 1pass--first-item-for-host (items host)
  (->> items
       (--first (1pass--item-has-host? it host))))

(defun 1pass--concealed-field? (field-plist)
  (string-equal "CONCEALED" (plist-get field-plist :type)))

(defun 1pass--obfuscate-field-value (field-plist)
  (plist-put field-plist :value
             (let* ((v (auth-source--obfuscate (plist-get field-plist :value))))
               (lambda ()
                 (auth-source--deobfuscate v)))))

(defun 1pass--obfuscate-concealed-fields (item-plist)
  (1pass--update-plist-property
   item-plist
   :fields
   (lambda (fields)
     (-map-when '1pass--concealed-field?
                '1pass--obfuscate-field-value
                fields))))

(defun 1pass--op-get-item (item-id)
  (-> (format "%s item get %s --vault %s --format json"
              1pass-executable
              (shell-quote-argument item-id)
              (shell-quote-argument 1pass-vault))
      (shell-command-to-string)
      (json-parse-string :object-type 'plist :array-type 'list)
      (1pass--obfuscate-concealed-fields)))

(defun 1pass--json-parse-buffer (&rest args)
  (condition-case error-or-result
      (apply #'json-parse-buffer args)
    (json-parse-error
     (1pass--do-debug "Attempted to parse invalid JSON; returning nil.")
     nil)
    (json-end-of-file
     (1pass--do-debug
      "parsed all json output from 1password; returning nil")
     nil)
    (t
     (1pass--do-debug
      "Received error %S while trying to parse 1password-cli vault items json; returning nil"
      (car error-or-result))
     nil)
    (:success error-or-result)))

(defun 1pass--op-get-items (items)
  (with-temp-buffer
    (json-insert items)
    (call-shell-region
     (point-min) (point-max)
     ;; I don't know exactly why, but the `1password-cli' doesn't recognize that
     ;; we're passing it data on stdin when we use just `call-process' to pass the
     ;; json objects in. Instead of digging in to why that is -- whether it's an
     ;; issue with `1password-cli', with how emacs spawns the process, or something
     ;; else -- I've opted to use a shell command with a pipe, as I know that
     ;; works.
     (format "cat | %s item get - --format json" 1pass-executable)
     t ;; Delete the text in range
     t ;; Replace it with the output of the command
     )
    (let (found-items '())
      ;; Move the point into place for `json-parse-buffer'
      (goto-char (point-min))
      ;; Here we're changing the array conversion to use a list, since we don't need to re-serialize
      ;; the vault items, and working with lists is easier.
      (while-let ((parsed-item (1pass--json-parse-buffer
                                :object-type 'plist :array-type 'list)))
        (setq found-items
              (cons (1pass--obfuscate-concealed-fields parsed-item) found-items)))
      found-items)))

(defun 1pass--username-field? (field-plist)
  (string-equal "username" (plist-get field-plist :id)))

(defun 1pass--field-username-matches? (field-plist username)
  (string-equal username (plist-get field-plist :value)))

(defun 1pass--item-has-username? (item-plist username)
  (->> (plist-get item-plist :fields)
       (--some (and (1pass--username-field? it)
                    (1pass--field-username-matches? it username)))))

(defun 1pass--credential-field? (field-plist)
  (string-equal "credential" (plist-get field-plist :id)))

(defun 1pass--item-has-credential-field? (item-plist)
  (-some #'1pass--credential-field?
         (plist-get item-plist :fields)))

(defun 1pass--first-credential-value (item-plist)
  (let ((credential-plist (->> (plist-get item-plist :fields)
                               (-first #'1pass--credential-field?))))
    (plist-get credential-plist :value)))

(defun 1pass--make-keyword (o)
  (cond
   ((keywordp o)
    o)
   ((stringp o)
    (make-symbol (concat ":" o)))
   (t
    (user-error "Argument `o' should be either a keyword or a string, received arg of type %S"
                (type-of o)))))

(defun 1pass--extract-item-fields (item-plist)
  "Given an ITEM-PLIST as returned by `1pass--op-get-item', return a single
plist mapping the field labels to their respective field values."
  (--mapcat (list (1pass--make-keyword (plist-get it :label)) (plist-get it :value))
            (plist-get item-plist :fields)))

(defun 1pass--merge-plists (&rest plists)
  "Create a single property list from all plists in PLISTS.
The process starts by copying the first list, and then setting properties
from the other lists.  Settings in the last list are the most significant
ones and overrule settings in the other lists."
  (let ((rtn (copy-sequence (pop plists)))
        p v ls)
    (while plists
      (setq ls (pop plists))
      (while ls
        (setq p (pop ls) v (pop ls))
        (setq rtn (plist-put rtn p v))))
    rtn))

(cl-defun 1pass-search (&rest spec
                              &key backend type host user port
                              &allow-other-keys)
  "Search 1password for the specified user and host.
SPEC, BACKEND, TYPE, HOST, USER and PORT are required by auth-source."

  (cl-assert (executable-find 1pass-executable)
             t "auth-source-1password: Could not find 1password-cli executable %S")

  (1pass--do-trivia
   "got spec: %S" spec)
  (1pass--do-debug
   "searching in vault %S for entries matching host=%S, user=%S, port=%S"
   1pass-vault host (or user "") (or port ""))
  (let* ((vault-items (1pass--list-items-in-vault 1pass-vault))
         (_ (1pass--do-debug "searching through %d vault items" (length vault-items)))

         (all-items-for-host (-filter #'1pass--item-has-credential-field?
                                      (-> vault-items
                                          (1pass--all-items-for-host host)
                                          (1pass--op-get-items))))
         (_ (1pass--do-debug "found %d items with credential fields for host %S"
                             (length all-items-for-host)
                             host))

         (first-item-for-host (-first-item all-items-for-host))
         (first-item-for-user (--first (1pass--item-has-username? it user)
                                       all-items-for-host))

         (best-match (cond
                      (first-item-for-user
                       (1pass--do-debug
                        "Returning first item matching host %S and username %S" host user)
                       first-item-for-user)

                      (first-item-for-host
                       (1pass--do-debug
                        "Couldn't find any items matching both host %S and username %S" host user)
                       (1pass--do-debug
                        "Returning first item matching host %S" host)
                       first-item-for-host)

                      (t
                       (1pass--do-debug
                        "Could not find any items for host %S; returning nil" host)
                       nil)))
         (extracted-item-fields (1pass--extract-item-fields best-match))
         (first-credential (1pass--first-credential-value best-match)))

    (when best-match
      (1pass--merge-plists
       extracted-item-fields
       (list :host host
             :user user
             :port port
             :secret first-credential)))))

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
