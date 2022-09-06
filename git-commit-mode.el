;;; git-commit-mode.el --- Edit Git commit messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Authors: Jonas Bernoulli <jonas@bernoul.li>
;;	Sebastian Wiesner <lunaryorn@gmail.com>
;;	Florian Ragwitz <rafl@debian.org>
;;	Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "25.1") (dash "20180910"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package assists the user in writing good Git commit messages.

;; When Git requests a commit message from the user, it does so by
;; having her edit a file which initially contains some comments,
;; instructing her what to do, and providing useful information, such
;; as which files were modified.  These comments, even when left
;; intact by the user, do not become part of the commit message.  This
;; package ensures these comments are propertizes as such and further
;; prettifies them by using different faces for various parts, such as
;; files.

;; Finally this package highlights style errors, like lines that are
;; too long, or when the second line is not empty.  It may even nag
;; you when you attempt to finish the commit without having fixed
;; these issues.  The style checks and many other settings can easily
;; be configured:
;;
;;   M-x customize-group RET git-commit RET

;;; Code:
;;;; Dependencies

(require 'dash)
(require 'log-edit)
(require 'ring)

(eval-when-compile (require 'recentf))

;;;; Declarations

(defvar diff-default-read-only)
(defvar flyspell-generic-check-word-predicate)
(defvar font-lock-beg)
(defvar font-lock-end)

;;; Options
;;;; Variables

(defgroup git-commit nil
  "Edit Git commit messages."
  :prefix "git-commit-"
  :link '(info-link "(magit)Editing Commit Messages")
  :group 'tools)

(defcustom git-commit-setup-hook
  '(git-commit-save-message
    git-commit-setup-changelog-support
    git-commit-turn-on-auto-fill
    git-commit-propertize-diff
    bug-reference-mode)
  "Hook run at the end of `git-commit-setup'."
  :group 'git-commit
  :type 'hook
  :options '(git-commit-save-message
             git-commit-setup-changelog-support
             git-commit-turn-on-auto-fill
             git-commit-turn-on-flyspell
             git-commit-propertize-diff
             bug-reference-mode))

(defcustom git-commit-summary-max-length 68
  "Column beyond which characters in the summary lines are highlighted.

The highlighting indicates that the summary is getting too long
by some standards.  It does in no way imply that going over the
limit a few characters or in some cases even many characters is
anything that deserves shaming.  It's just a friendly reminder
that if you can make the summary shorter, then you might want
to consider doing so."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

(defcustom git-commit-known-pseudo-headers
  '("Signed-off-by" "Acked-by" "Modified-by" "Cc"
    "Suggested-by" "Reported-by" "Tested-by" "Reviewed-by")
  "A list of Git pseudo headers to be highlighted."
  :group 'git-commit
  :safe (lambda (val) (and (listp val) (-all-p 'stringp val)))
  :type '(repeat string))

;;;; Faces

(defgroup git-commit-faces nil
  "Faces used for highlighting Git commit messages."
  :prefix "git-commit-"
  :group 'git-commit
  :group 'faces)

(defface git-commit-summary
  '((t :inherit font-lock-type-face))
  "Face used for the summary in commit messages."
  :group 'git-commit-faces)

(defface git-commit-overlong-summary
  '((t :inherit font-lock-warning-face))
  "Face used for the tail of overlong commit message summaries."
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line
  '((t :inherit font-lock-warning-face))
  "Face used for non-whitespace on the second line of commit messages."
  :group 'git-commit-faces)

(defface git-commit-keyword
  '((t :inherit font-lock-string-face))
  "Face used for keywords in commit messages.
In this context a \"keyword\" is text surrounded be brackets."
  :group 'git-commit-faces)

(define-obsolete-face-alias 'git-commit-note
  'git-commit-keyword "Git-Commit 2.91.0")

(defface git-commit-pseudo-header
  '((t :inherit font-lock-string-face))
  "Face used for pseudo headers in commit messages."
  :group 'git-commit-faces)

(defface git-commit-known-pseudo-header
  '((t :inherit font-lock-keyword-face))
  "Face used for the keywords of known pseudo headers in commit messages."
  :group 'git-commit-faces)

(defface git-commit-comment-branch-local
  '((t :inherit font-lock-variable-name-face))
  "Face used for names of local branches in commit message comments."
  :group 'git-commit-faces)

(define-obsolete-face-alias 'git-commit-comment-branch
  'git-commit-comment-branch-local "Git-Commit 2.12.0")

(defface git-commit-comment-branch-remote
  '((t :inherit font-lock-variable-name-face))
  "Face used for names of remote branches in commit message comments.
This is only used if Magit is available."
  :group 'git-commit-faces)

(defface git-commit-comment-detached
  '((t :inherit git-commit-comment-branch-local))
  "Face used for detached `HEAD' in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-heading
  '((t :inherit git-commit-known-pseudo-header))
  "Face used for headings in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-file
  '((t :inherit git-commit-pseudo-header))
  "Face used for file names in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-action
  '((t :inherit bold))
  "Face used for actions in commit message comments."
  :group 'git-commit-faces)

;;; Keymap

(defvar git-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") 'git-commit-ack)
    (define-key map (kbd "C-c C-i") 'git-commit-suggested)
    (define-key map (kbd "C-c C-m") 'git-commit-modified)
    (define-key map (kbd "C-c C-o") 'git-commit-cc)
    (define-key map (kbd "C-c C-p") 'git-commit-reported)
    (define-key map (kbd "C-c C-r") 'git-commit-review)
    (define-key map (kbd "C-c C-s") 'git-commit-signoff)
    (define-key map (kbd "C-c C-t") 'git-commit-test)
    (define-key map (kbd "C-c M-s") 'git-commit-save-message)
    map)
  "Key map used by `git-commit-mode'.")

;;; Hooks

;;;###autoload
(defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-commit-filename-regexp))

;;;###autoload
(defun git-commit-setup-check-buffer ()
  (and buffer-file-name
       (string-match-p git-commit-filename-regexp buffer-file-name)
       (git-commit-setup)))

(defvar git-commit-mode)

(defun git-commit-file-not-found ()
  ;; cygwin git will pass a cygwin path (/cygdrive/c/foo/.git/...),
  ;; try to handle this in window-nt Emacs.
  (--when-let
      (and (or (string-match-p git-commit-filename-regexp buffer-file-name)
               (and (boundp 'git-rebase-filename-regexp)
                    (string-match-p git-rebase-filename-regexp
                                    buffer-file-name)))
           (and (string-match "\\`[a-z]:/\\(cygdrive/\\)?\\([a-z]\\)/\\(.*\\)"
                              buffer-file-name)
                (concat (match-string 2 buffer-file-name) ":/"
                        (match-string 3 buffer-file-name))))
    (when (file-accessible-directory-p (file-name-directory it))
      (let ((inhibit-read-only t))
        (insert-file-contents it t)
        t))))

(when (eq system-type 'windows-nt)
  (add-hook 'find-file-not-found-functions #'git-commit-file-not-found))

(defconst git-commit-usage-message "\
Type \\[with-editor-finish] to finish, \
\\[with-editor-cancel] to cancel, and \
\\[git-commit-prev-message] and \\[git-commit-next-message] \
to recover older messages")

;;;###autoload
(defun git-commit-setup ()
  ;; Pretend that git-commit-mode is a major-mode,
  ;; so that directory-local settings can be used.
  (let ((buffer-file-name nil)         ; trick hack-dir-local-variables
        (major-mode 'git-commit-mode)) ; trick dir-locals-collect-variables
    (hack-dir-local-variables)
    (hack-local-variables-apply))
  (git-commit-mode t)
  (make-local-variable 'log-edit-comment-ring-index)
  (git-commit-mode 1)
  (git-commit-setup-font-lock)
  (when (boundp 'save-place)
    (setq save-place nil))
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "\\`\\(\\'\\|\n[^\n]\\)")
      (open-line 1)))
  (with-demoted-errors "Error running git-commit-setup-hook: %S"
    (run-hooks 'git-commit-setup-hook))
  (set-buffer-modified-p nil))

(define-minor-mode git-commit-mode
  "Auxiliary minor mode used when editing Git commit messages.
This mode is only responsible for setting up some key bindings.
Don't use it directly, instead enable `global-git-commit-mode'."
  :lighter "")

(put 'git-commit-mode 'permanent-local t)

(defun git-commit-setup-changelog-support ()
  "Treat ChangeLog entries as unindented paragraphs."
  (setq-local fill-indent-according-to-mode t)
  (setq-local paragraph-start (concat paragraph-start "\\|\\*\\|(")))

(defun git-commit-turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (setq-local comment-auto-fill-only-comments nil)
  (turn-on-auto-fill))

(defun git-commit-turn-on-flyspell ()
  "Unconditionally turn on Flyspell mode.
Also prevent comments from being checked and
finally check current non-comment text."
  (require 'flyspell)
  (turn-on-flyspell)
  (setq flyspell-generic-check-word-predicate
        'git-commit-flyspell-verify)
  (let ((end)
        (comment-start-regex (format "^\\(%s\\|$\\)" comment-start)))
    (save-excursion
      (goto-char (point-max))
      (while (and (not (bobp)) (looking-at comment-start-regex))
        (forward-line -1))
      (unless (looking-at comment-start-regex)
        (forward-line))
      (setq end (point)))
    (flyspell-region (point-min) end)))

(defun git-commit-flyspell-verify ()
  (not (= (char-after (line-beginning-position))
          (aref comment-start 0))))

;;; History

(defun git-commit-save-message ()
  "Save current message to `log-edit-comment-ring'."
  (interactive)
  (--when-let (git-commit-buffer-message)
    (unless (ring-member log-edit-comment-ring it)
      (ring-insert log-edit-comment-ring it))))

(defun git-commit-buffer-message ()
  (let ((flush (concat "^" comment-start))
        (str (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward (concat flush " -+ >8 -+$") nil t)
        (delete-region (line-beginning-position) (point-max)))
      (goto-char (point-min))
      (flush-lines flush)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (setq str (buffer-string)))
    (unless (string-match "\\`[ \t\n\r]*\\'" str)
      (when (string-match "\\`\n\\{2,\\}" str)
        (setq str (replace-match "\n" t t str)))
      (when (string-match "\n\\{2,\\}\\'" str)
        (setq str (replace-match "\n" t t str)))
      str)))

;;; Headers

(defun git-commit-ack (name mail)
  "Insert a header acknowledging that you have looked at the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Acked-by" name mail))

(defun git-commit-modified (name mail)
  "Insert a header to signal that you have modified the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Modified-by" name mail))

(defun git-commit-review (name mail)
  "Insert a header acknowledging that you have reviewed the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Reviewed-by" name mail))

(defun git-commit-signoff (name mail)
  "Insert a header to sign off the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Signed-off-by" name mail))

(defun git-commit-test (name mail)
  "Insert a header acknowledging that you have tested the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Tested-by" name mail))

(defun git-commit-cc (name mail)
  "Insert a header mentioning someone who might be interested."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Cc" name mail))

(defun git-commit-reported (name mail)
  "Insert a header mentioning the person who reported the issue."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Reported-by" name mail))

(defun git-commit-suggested (name mail)
  "Insert a header mentioning the person who suggested the change."
  (interactive (git-commit-read-ident))
  (git-commit-insert-header "Suggested-by" name mail))

(defun git-commit-self-ident ()
  (list (or (getenv "GIT_AUTHOR_NAME")
            (getenv "GIT_COMMITTER_NAME")
            (ignore-errors (car (process-lines "git" "config" "user.name")))
            user-full-name
            (read-string "Name: "))
        (or (getenv "GIT_AUTHOR_EMAIL")
            (getenv "GIT_COMMITTER_EMAIL")
            (getenv "EMAIL")
            (ignore-errors (car (process-lines "git" "config" "user.email")))
            (read-string "Email: "))))

(defun git-commit-read-ident ()
  (list (read-string "Name: ")
        (read-string "Email: ")))

(defun git-commit-insert-header (header name email)
  (setq header (format "%s: %s <%s>" header name email))
  (save-excursion
    (goto-char (point-max))
    (cond ((re-search-backward "^[-a-zA-Z]+: [^<]+? <[^>]+>" nil t)
           (end-of-line)
           (insert ?\n header)
           (unless (= (char-after) ?\n)
             (insert ?\n)))
          (t
           (while (re-search-backward (concat "^" comment-start) nil t))
           (unless (looking-back "\n\n" nil)
             (insert ?\n))
           (insert header ?\n)))
    (unless (or (eobp) (= (char-after) ?\n))
      (insert ?\n))))

;;; Font-Lock

(defvar-local git-commit-need-summary-line t
  "Whether the text should have a heading that is separated from the body.

For commit messages that is a convention that should not
be violated.  For notes it is up to the user.  If you do
not want to insist on an empty second line here, then use
something like:

  (add-hook \\='git-commit-setup-hook
            (lambda ()
              (when (equal (file-name-nondirectory (buffer-file-name))
                           \"NOTES_EDITMSG\")
                (setq git-commit-need-summary-line nil))))")

(defun git-commit-summary-regexp ()
  (if git-commit-need-summary-line
      (concat
       ;; Leading empty lines and comments
       (format "\\`\\(?:^\\(?:\\s-*\\|%s.*\\)\n\\)*" comment-start)
       ;; Summary line
       (format "\\(.\\{0,%d\\}\\)\\(.*\\)" git-commit-summary-max-length)
       ;; Non-empty non-comment second line
       (format "\\(?:\n%s\\|\n\\(.+\\)\\)?" comment-start))
    "\\(EASTER\\) \\(EGG\\)"))

(defun git-commit-extend-region-summary-line ()
  "Identify the multiline summary-regexp construct.
Added to `font-lock-extend-region-functions'."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at (git-commit-summary-regexp))
        (let ((summary-beg (match-beginning 0))
              (summary-end (match-end 0)))
          (when (or (< summary-beg font-lock-beg summary-end)
                    (< summary-beg font-lock-end summary-end))
            (setq font-lock-beg (min font-lock-beg summary-beg))
            (setq font-lock-end (max font-lock-end summary-end))))))))

(defvar-local git-commit--branch-name-regexp nil)

(defconst git-commit-comment-headings
  '("Changes to be committed:"
    "Untracked files:"
    "Changed but not updated:"
    "Changes not staged for commit:"
    "Unmerged paths:"
    "Author:"
    "Date:"))

(defconst git-commit-font-lock-keywords-1
  '(;; Pseudo headers
    (eval . `(,(format "^\\(%s:\\)\\( .*\\)"
                       (regexp-opt git-commit-known-pseudo-headers))
              (1 'git-commit-known-pseudo-header)
              (2 'git-commit-pseudo-header)))
    ("^[-a-zA-Z]+: [^<]+? <[^>]+>"
     (0 'git-commit-pseudo-header))
    ;; Summary
    (eval . `(,(git-commit-summary-regexp)
              (1 'git-commit-summary)))
    ;; - Keyword [aka "text in brackets"] (overrides summary)
    ("\\[.+?\\]"
     (0 'git-commit-keyword t))
    ;; - Non-empty second line (overrides summary and note)
    (eval . `(,(git-commit-summary-regexp)
              (2 'git-commit-overlong-summary t t)
              (3 'git-commit-nonempty-second-line t t)))))

(defconst git-commit-font-lock-keywords-2
  `(,@git-commit-font-lock-keywords-1
    ;; Comments
    (eval . `(,(format "^%s.*" comment-start)
              (0 'font-lock-comment-face)))
    (eval . `(,(format "^%s On branch \\(.*\\)" comment-start)
              (1 'git-commit-comment-branch-local t)))
    (eval . `(,(format "^%s \\(HEAD\\) detached at" comment-start)
              (1 'git-commit-comment-detached t)))
    (eval . `(,(format "^%s %s" comment-start
                       (regexp-opt git-commit-comment-headings t))
              (1 'git-commit-comment-heading t)))
    (eval . `(,(format "^%s\t\\(?:\\([^:\n]+\\):\\s-+\\)?\\(.*\\)" comment-start)
              (1 'git-commit-comment-action t t)
              (2 'git-commit-comment-file t)))))

(defconst git-commit-font-lock-keywords-3
  `(,@git-commit-font-lock-keywords-2
    ;; More comments
    (eval
     ;; Your branch is ahead of 'master' by 3 commits.
     ;; Your branch is behind 'master' by 2 commits, and can be fast-forwarded.
     . `(,(format
           "^%s Your branch is \\(?:ahead\\|behind\\) of '%s' by \\([0-9]*\\)"
           comment-start git-commit--branch-name-regexp)
         (1 'git-commit-comment-branch-local t)
         (2 'git-commit-comment-branch-remote t)
         (3 'bold t)))
    (eval
     ;; Your branch is up to date with 'master'.
     ;; Your branch and 'master' have diverged,
     . `(,(format
           "^%s Your branch \\(?:is up-to-date with\\|and\\) '%s'"
           comment-start git-commit--branch-name-regexp)
         (1 'git-commit-comment-branch-local t)
         (2 'git-commit-comment-branch-remote t)))
    (eval
     ;; and have 1 and 2 different commits each, respectively.
     . `(,(format
           "^%s and have \\([0-9]*\\) and \\([0-9]*\\) commits each"
           comment-start)
         (1 'bold t)
         (2 'bold t)))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-2
  "Font-Lock keywords for Git-Commit mode.")

(defun git-commit-setup-font-lock ()
  (let ((table (make-syntax-table (syntax-table))))
    (when comment-start
      (modify-syntax-entry (string-to-char comment-start) "." table))
    (modify-syntax-entry ?#  "." table)
    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?`  "." table)
    (set-syntax-table table))
  (setq-local comment-start
              (or (ignore-errors
                    (car (process-lines "git" "config" "core.commentchar")))
                  "#"))
  (setq-local comment-start-skip (format "^%s+[\s\t]*" comment-start))
  (setq-local comment-end-skip "\n")
  (setq-local comment-use-syntax nil)
  (setq-local git-commit--branch-name-regexp "\\([^']*\\)")
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'git-commit-extend-region-summary-line
            t t)
  (font-lock-add-keywords nil git-commit-font-lock-keywords))

(defun git-commit-propertize-diff ()
  (require 'diff-mode)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (beginning-of-line)
      (let ((buffer (current-buffer)))
        (insert
         (with-temp-buffer
           (insert
            (with-current-buffer buffer
              (prog1 (buffer-substring-no-properties (point) (point-max))
                (delete-region (point) (point-max)))))
           (let ((diff-default-read-only nil))
             (diff-mode))
           (let (font-lock-verbose font-lock-support-mode)
             (if (fboundp 'font-lock-ensure)
                 (font-lock-ensure)
               (with-no-warnings
                 (font-lock-fontify-buffer))))
           (let (next (pos (point-min)))
             (while (setq next (next-single-property-change pos 'face))
               (put-text-property pos next 'font-lock-face
                                  (get-text-property pos 'face))
               (setq pos next))
             (put-text-property pos (point-max) 'font-lock-face
                                (get-text-property pos 'face)))
           (buffer-string)))))))

;;; Elisp Text Mode

;;;###autoload
(define-derived-mode git-commit-elisp-text-mode text-mode "ElText"
  "Major mode for editing commit messages of elisp projects.
This is intended for use as `git-commit-major-mode' for projects
that expect `symbols' to look like this.  I.e. like they look in
Elisp doc-strings, including this one.  Unlike in doc-strings,
\"strings\" also look different than the other text."
  (setq font-lock-defaults '(git-commit-elisp-text-mode-keywords))
  (git-commit-setup))

(defvar git-commit-elisp-text-mode-keywords
  `((,(concat "[`‘]\\(" lisp-mode-symbol-regexp "\\)['’]")
     (1 font-lock-constant-face prepend))
    ("\"[^\"]*\"" (0 font-lock-string-face prepend))))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons git-commit-filename-regexp 'git-commit-elisp-text-mode))

;;; _
(provide 'git-commit-mode)
;;; git-commit-mode.el ends here
