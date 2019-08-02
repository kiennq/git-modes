;;; git-utils.el --- various utilities  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Contains code from GNU Emacs https://www.gnu.org/software/emacs,
;; released under the GNU General Public License version 3 or later.

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library defines several utility functions used by several
;; other libraries which cannot depend on one another (because
;; circular dependencies are not good).  Luckily most (all) of these
;; functions have very little (nothing) to do with Git, so we not only
;; have to do this, it even makes sense.

;; Unfortunately there are also some options which are used by several
;; libraries which cannot depend on one another, they are defined here
;; too.

;;; Code:

(require 'cl-lib)
(require 'dash)

(eval-when-compile
  (require 'subr-x))

(require 'crm)

(eval-when-compile (require 'ido))
(declare-function ido-completing-read+ "ido-completing-read+"
                  (prompt collection &optional predicate
                          require-match initial-input
                          hist def inherit-input-method))
(declare-function Info-get-token "info" (pos start all &optional errorstring))

(eval-when-compile (require 'vc-git))
(declare-function vc-git--run-command-string "vc-git" (file &rest args))

(eval-when-compile (require 'which-func))
(declare-function which-function "which-func" ())

;;; Options

(defcustom git-completing-read-function 'git-builtin-completing-read
  "Function to be called when requesting input from the user.

If you have enabled `ivy-mode' or `helm-mode', then you don't
have to customize this option; `git-builtin-completing-read'
will work just fine.  However, if you use Ido completion, then
you do have to use `git-ido-completing-read', because Ido is
less well behaved than the former, more modern alternatives.

If you would like to use Ivy or Helm completion with Magit but
not enable the respective modes globally, then customize this
option to use `ivy-completing-read' or
`helm--completing-read-default'.  If you choose to use
`ivy-completing-read', note that the items may always be shown in
alphabetical order, depending on your version of Ivy."
  :group 'git-essentials
  :type '(radio (function-item git-builtin-completing-read)
                (function-item git-ido-completing-read)
                (function-item ivy-completing-read)
                (function-item helm--completing-read-default)
                (function :tag "Other function")))

(defcustom git-dwim-selection
  '((forge-browse-dwim        nil t)
    (forge-browse-commit      nil t)
    (forge-browse-branch      nil t)
    (forge-browse-remote      nil t)
    (forge-browse-issue       nil t)
    (forge-browse-pullreq     nil t)
    (forge-edit-topic-title   nil t)
    (forge-edit-topic-state   nil t)
    (forge-edit-topic-labels  nil t)
    (forge-edit-topic-marks   nil t)
    (forge-edit-topic-assignees nil t)
    (forge-edit-topic-review-requests nil t)
    (forge-pull-pullreq       nil t)
    (forge-visit-issue        nil t)
    (forge-visit-pullreq      nil t))
  "When not to offer alternatives and ask for confirmation.

Many commands by default ask the user to select from a list of
possible candidates.  They do so even when there is a thing at
point that they can act on, which is then offered as the default.

This option can be used to tell certain commands to use the thing
at point instead of asking the user to select a candidate to act
on, with or without confirmation.

The value has the form ((COMMAND nil|PROMPT DEFAULT)...).

- COMMAND is the command that should not prompt for a choice.
  To have an effect, the command has to use the function
  `git-completing-read' or a utility function which in turn uses
  that function.

- If the command uses `git-completing-read' multiple times, then
  PROMPT can be used to only affect one of these uses.  PROMPT, if
  non-nil, is a regular expression that is used to match against
  the PROMPT argument passed to `git-completing-read'.

- DEFAULT specifies how to use the default.  If it is t, then
  the DEFAULT argument passed to `git-completing-read' is used
  without confirmation.  If it is `ask', then the user is given
  a chance to abort.  DEFAULT can also be nil, in which case the
  entry has no effect."
  :package-version '(magit . "2.12.0")
  :group 'git-commands
  :type '(repeat
          (list (symbol :tag "Command") ; It might not be fboundp yet.
                (choice (const  :tag "for all prompts" nil)
                        (regexp :tag "for prompts matching regexp"))
                (choice (const  :tag "offer other choices" nil)
                        (const  :tag "require confirmation" ask)
                        (const  :tag "use default without confirmation" t)))))

;;; User Input

(defvar helm-completion-in-region-default-sort-fn)
(defvar ivy-sort-functions-alist)

(defvar git-completing-read--silent-default nil)

(defun git-completing-read (prompt collection &optional
                                   predicate require-match initial-input
                                   hist def fallback)
  "Read a choice in the minibuffer, or use the default choice.

This is the function that Magit commands use when they need the
user to select a single thing to act on.  The arguments have the
same meaning as for `completing-read', except for FALLBACK, which
is unique to this function and is described below.

Instead of asking the user to choose from a list of possible
candidates, this function may instead just return the default
specified by DEF, with or without requiring user confirmation.
Whether that is the case depends on PROMPT, `this-command' and
`git-dwim-selection'.  See the documentation of the latter for
more information.

If it does use the default without the user even having to
confirm that, then `git-completing-read--silent-default' is set
to t, otherwise nil.

If it does read a value in the minibuffer, then this function
acts similarly to `completing-read', except for the following:

- COLLECTION must be a list of choices.  A function is not
  supported.

- If REQUIRE-MATCH is nil and the user exits without a choice,
  then nil is returned instead of an empty string.

- If REQUIRE-MATCH is non-nil and the users exits without a
  choice, an user-error is raised.

- FALLBACK specifies a secondary default that is only used if
  the primary default DEF is nil.  The secondary default is not
  subject to `git-dwim-selection' — if DEF is nil but FALLBACK
  is not, then this function always asks the user to choose a
  candidate, just as if both defaults were nil.

- \": \" is appended to PROMPT.

- PROMPT is modified to end with \" (default DEF|FALLBACK): \"
  provided that DEF or FALLBACK is non-nil, that neither
  `ivy-mode' nor `helm-mode' is enabled, and that
  `git-completing-read-function' is set to its default value of
  `git-builtin-completing-read'."
  (setq git-completing-read--silent-default nil)
  (if-let ((dwim (and def
                      (nth 2 (-first (pcase-lambda (`(,cmd ,re ,_))
                                       (and (eq this-command cmd)
                                            (or (not re)
                                                (string-match-p re prompt))))
                                     git-dwim-selection)))))
      (if (eq dwim 'ask)
          (if (y-or-n-p (format "%s %s? " prompt def))
              def
            (user-error "Abort"))
        (setq git-completing-read--silent-default t)
        def)
    (unless def
      (setq def fallback))
    (let ((command this-command)
          (reply (funcall git-completing-read-function
                          (concat prompt ": ")
                          (if (and def (not (member def collection)))
                              (cons def collection)
                            collection)
                          predicate
                          require-match initial-input hist def)))
      (setq this-command command)
      (if (string= reply "")
          (if require-match
              (user-error "Nothing selected")
            nil)
        reply))))

(defun git--completion-table (collection)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity))
      (complete-with-action action collection string pred))))

(defun git-builtin-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (unless (or (bound-and-true-p helm-mode)
              (bound-and-true-p ivy-mode))
    (setq prompt (git-prompt-with-default prompt def))
    (setq choices (git--completion-table choices)))
  (cl-letf (((symbol-function 'completion-pcm--all-completions)
             #'git-completion-pcm--all-completions))
    (let ((ivy-sort-functions-alist nil))
      (completing-read prompt choices
                       predicate require-match
                       initial-input hist def))))

(defun git-prompt-with-default (prompt def)
  (if (and def (> (length prompt) 2)
           (string-equal ": " (substring prompt -2)))
      (format "%s (default %s): " (substring prompt 0 -2) def)
    prompt))


;;; Text Utilities

(defun git-delete-line ()
  "Delete the rest of the current line."
  (delete-region (point) (1+ (line-end-position))))

;;; Missing from Emacs


;;; Kludges for Emacs Bugs


;; `completion-pcm--all-completions' reverses the completion list.  To
;; preserve the order of our pre-sorted completions, we'll temporarily
;; override it with the function below.  bug#24676
(defun git-completion-pcm--all-completions (prefix pattern table pred)
  (if (completion-pcm--pattern-trivial-p pattern)
      (all-completions (concat prefix (car pattern)) table pred)
    (let* ((regex (completion-pcm--pattern->regex pattern))
           (case-fold-search completion-ignore-case)
           (completion-regexp-list (cons regex completion-regexp-list))
           (compl (all-completions
                   (concat prefix
                           (if (stringp (car pattern)) (car pattern) ""))
                   table pred)))
      (if (not (functionp table))
          compl
        (let ((poss ()))
          (dolist (c compl)
            (when (string-match-p regex c) (push c poss)))
          ;; This `nreverse' call is the only code change made to the
          ;; `completion-pcm--all-completions' that shipped with Emacs 25.1.
          (nreverse poss))))))

;;; Bitmaps

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'git-fringe-bitmap+
    [#b00000000
     #b00011000
     #b00011000
     #b01111110
     #b01111110
     #b00011000
     #b00011000
     #b00000000])
  (define-fringe-bitmap 'git-fringe-bitmap-
    [#b00000000
     #b00000000
     #b00000000
     #b01111110
     #b01111110
     #b00000000
     #b00000000
     #b00000000])

  (define-fringe-bitmap 'git-fringe-bitmap>
    [#b01100000
     #b00110000
     #b00011000
     #b00001100
     #b00011000
     #b00110000
     #b01100000
     #b00000000])
  (define-fringe-bitmap 'git-fringe-bitmapv
    [#b00000000
     #b10000010
     #b11000110
     #b01101100
     #b00111000
     #b00010000
     #b00000000
     #b00000000])

  (define-fringe-bitmap 'git-fringe-bitmap-bold>
    [#b11100000
     #b01110000
     #b00111000
     #b00011100
     #b00011100
     #b00111000
     #b01110000
     #b11100000])
  (define-fringe-bitmap 'git-fringe-bitmap-boldv
    [#b10000001
     #b11000011
     #b11100111
     #b01111110
     #b00111100
     #b00011000
     #b00000000
     #b00000000])
  )

;;; _
(provide 'git-utils)
;;; git-utils.el ends here
