;;; git-rebase-mode.el --- Edit Git rebase files  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Package-Requires: ((emacs "25.1") (dash "2.14.1"))

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

;; This package assists the user in editing the list of commits to be
;; rewritten during an interactive rebase.

;; You should probably also read the `git-rebase' manpage.

;;; Code:

(require 'dash)
(require 'eieio)
(require 'git-utils)

(and (require 'async-bytecomp nil t)
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(eval-when-compile (require 'recentf))

;;; Options
;;;; Variables

(defgroup git-rebase nil
  "Edit Git rebase sequences."
  :link '(info-link "(magit)Editing Rebase Sequences")
  :group 'tools)

(defcustom git-rebase-auto-advance t
  "Whether to move to next line after changing a line."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-show-instructions t
  "Whether to show usage instructions inside the rebase buffer."
  :group 'git-rebase
  :type 'boolean)

;;;; Faces

(defgroup git-rebase-faces nil
  "Faces used by Git-Rebase mode."
  :group 'faces
  :group 'git-rebase)

(defface magit-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the sha1 part of the log output."
  :group 'magit-faces)

(defface git-rebase-hash '((t (:inherit magit-hash)))
  "Face for commit hashes."
  :group 'git-rebase-faces)

(defface magit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'magit-faces)

(defface git-rebase-label '((t (:inherit magit-refname)))
  "Face for labels in label, merge, and reset lines."
  :group 'git-rebase-faces)

(defface git-rebase-description nil
  "Face for commit descriptions."
  :group 'git-rebase-faces)

(defface git-rebase-killed-action
  '((t (:inherit font-lock-comment-face :strike-through t)))
  "Face for commented commit action lines."
  :group 'git-rebase-faces)

(defface git-rebase-comment-hash
  '((t (:inherit git-rebase-hash :weight bold)))
  "Face for commit hashes in commit message comments."
  :group 'git-rebase-faces)

(defface git-rebase-comment-heading
  '((t :inherit font-lock-keyword-face))
  "Face for headings in rebase message comments."
  :group 'git-rebase-faces)

(defface git-rebase-keyword
  '((t :inherit font-lock-string-face))
  "Face for parts of commit messages inside brackets."
  :group 'git-rebase-faces)

;;; Keymaps

(defvar git-rebase-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "p") 'git-rebase-pick)
    (define-key map (kbd "b") 'git-rebase-break)
    (define-key map (kbd "e") 'git-rebase-edit)
    (define-key map (kbd "l") 'git-rebase-label)
    (define-key map (kbd "MM") 'git-rebase-merge)
    (define-key map (kbd "Mt") 'git-rebase-merge-toggle-editmsg)
    (define-key map (kbd "m") 'git-rebase-edit)
    (define-key map (kbd "f") 'git-rebase-fixup)
    (define-key map (kbd "q") 'undefined)
    (define-key map (kbd "r") 'git-rebase-reword)
    (define-key map (kbd "w") 'git-rebase-reword)
    (define-key map (kbd "s") 'git-rebase-squash)
    (define-key map (kbd "t") 'git-rebase-reset)
    (define-key map (kbd "x") 'git-rebase-exec)
    (define-key map (kbd "z") 'git-rebase-noop)
    (define-key map [remap undo] 'git-rebase-undo)
    map)
  "Keymap for Git-Rebase mode.")

(defvar git-rebase-command-descriptions
  '((undo                         . "undo last change")))

;;; Commands

(defun git-rebase-pick ()
  "Use commit on current line."
  (interactive)
  (git-rebase-set-action "pick"))

(defun git-rebase-reword ()
  "Edit message of commit on current line."
  (interactive)
  (git-rebase-set-action "reword"))

(defun git-rebase-edit ()
  "Stop at the commit on the current line."
  (interactive)
  (git-rebase-set-action "edit"))

(defun git-rebase-squash ()
  "Meld commit on current line into previous commit, edit message."
  (interactive)
  (git-rebase-set-action "squash"))

(defun git-rebase-fixup ()
  "Meld commit on current line into previous commit, discard its message."
  (interactive)
  (git-rebase-set-action "fixup"))

(defvar-local git-rebase-comment-re nil)

(defvar git-rebase-short-options
  '((?b . "break")
    (?e . "edit")
    (?f . "fixup")
    (?l . "label")
    (?m . "merge")
    (?p . "pick")
    (?r . "reword")
    (?s . "squash")
    (?t . "reset")
    (?x . "exec"))
  "Alist mapping single key of an action to the full name.")

(defclass git-rebase-action ()
  (;; action-type: commit, exec, bare, label, merge
   (action-type    :initarg :action-type    :initform nil)
   ;; Examples for each action type:
   ;; | action | action options | target  | trailer |
   ;; |--------+----------------+---------+---------|
   ;; | pick   |                | hash    | subject |
   ;; | exec   |                | command |         |
   ;; | noop   |                |         |         |
   ;; | reset  |                | name    | subject |
   ;; | merge  | -C hash        | name    | subject |
   (action         :initarg :action         :initform nil)
   (action-options :initarg :action-options :initform nil)
   (target         :initarg :target         :initform nil)
   (trailer        :initarg :trailer        :initform nil)
   (comment-p      :initarg :comment-p      :initform nil)))

(defvar git-rebase-line-regexps
  `((commit . ,(concat
                (regexp-opt '("e" "edit"
                              "f" "fixup"
                              "p" "pick"
                              "r" "reword"
                              "s" "squash")
                            "\\(?1:")
                " \\(?3:[^ \n]+\\) \\(?4:.*\\)"))
    (exec . "\\(?1:x\\|exec\\) \\(?3:.*\\)")
    (bare . ,(concat (regexp-opt '("b" "break" "noop") "\\(?1:")
                     " *$"))
    (label . ,(concat (regexp-opt '("l" "label"
                                    "t" "reset")
                                  "\\(?1:")
                      " \\(?3:[^ \n]+\\) ?\\(?4:.*\\)"))
    (merge . ,(concat "\\(?1:m\\|merge\\) "
                      "\\(?:\\(?2:-[cC] [^ \n]+\\) \\)?"
                      "\\(?3:[^ \n]+\\)"
                      " ?\\(?4:.*\\)"))))

;;;###autoload
(defun git-rebase-current-line ()
  "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned."
  (save-excursion
    (goto-char (line-beginning-position))
    (if-let ((re-start (concat "^\\(?5:" (regexp-quote comment-start)
                               "\\)? *"))
             (type (-some (lambda (arg)
                            (let ((case-fold-search nil))
                              (and (looking-at (concat re-start (cdr arg)))
                                   (car arg))))
                          git-rebase-line-regexps)))
        (git-rebase-action
         :action-type    type
         :action         (when-let ((action (match-string-no-properties 1)))
                           (or (cdr (assoc action git-rebase-short-options))
                               action))
         :action-options (match-string-no-properties 2)
         :target         (match-string-no-properties 3)
         :trailer        (match-string-no-properties 4)
         :comment-p      (and (match-string 5) t))
      ;; Use default empty class rather than nil to ease handling.
      (git-rebase-action))))

(defun git-rebase-set-action (action)
  (goto-char (line-beginning-position))
  (with-slots (action-type target trailer)
      (git-rebase-current-line)
    (if (eq action-type 'commit)
        (let ((inhibit-read-only t))
          (git-delete-line)
          (insert (concat action " " target " " trailer "\n"))
          (unless git-rebase-auto-advance
            (forward-line -1)))
      (ding))))

(defun git-rebase-line-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (and (oref (git-rebase-current-line) action-type)
         t)))

(defun git-rebase-region-bounds ()
  (when (use-region-p)
    (let ((beg (save-excursion (goto-char (region-beginning))
                               (line-beginning-position)))
          (end (save-excursion (goto-char (region-end))
                               (line-end-position))))
      (when (and (git-rebase-line-p beg)
                 (git-rebase-line-p end))
        (list beg (1+ end))))))

(defun git-rebase-set-noncommit-action (action value-fn arg)
  (goto-char (line-beginning-position))
  (pcase-let* ((inhibit-read-only t)
               (`(,initial ,trailer ,comment-p)
                (and (not arg)
                     (with-slots ((ln-action action)
                                  target trailer comment-p)
                         (git-rebase-current-line)
                       (and (equal ln-action action)
                            (list target trailer comment-p)))))
               (value (funcall value-fn initial)))
    (pcase (list value initial comment-p)
      (`("" nil ,_)
       (ding))
      (`(""  ,_ ,_)
       (git-delete-line))
      (_
       (if initial
           (git-delete-line)
         (forward-line))
       (insert (concat action " " value
                       (and (equal value initial)
                            trailer
                            (concat " " trailer))
                       "\n"))
       (unless git-rebase-auto-advance
         (forward-line -1))))))

(defun git-rebase-exec (arg)
  "Insert a shell command to be run after the current commit.

If there already is such a command on the current line, then edit
that instead.  With a prefix argument insert a new command even
when there already is one on the current line.  With empty input
remove the command on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "exec"
   (lambda (initial) (read-shell-command "Execute: " initial))
   arg))

(defun git-rebase-label (arg)
  "Add a label after the current commit.
If there already is a label on the current line, then edit that
instead.  With a prefix argument, insert a new label even when
there is already a label on the current line.  With empty input,
remove the label on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "label"
   (lambda (initial)
     (read-from-minibuffer
      "Label: " initial))
   arg))

(defun git-rebase-buffer-labels ()
  (let (labels)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(?:l\\|label\\) \\([^ \n]+\\)" nil t)
        (push (match-string-no-properties 1) labels)))
    (nreverse labels)))

(defun git-rebase-reset (arg)
  "Reset the current HEAD to a label.
If there already is a reset command on the current line, then
edit that instead.  With a prefix argument, insert a new reset
line even when point is already on a reset line.  With empty
input, remove the reset command on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "reset"
   (lambda (initial)
     (or (git-completing-read "Label" (git-rebase-buffer-labels)
                              nil t initial)
         ""))
   arg))

(defun git-rebase-merge (arg)
  "Add a merge command after the current commit.
If there is already a merge command on the current line, then
replace that command instead.  With a prefix argument, insert a
new merge command even when there is already one on the current
line.  With empty input, remove the merge command on the current
line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "merge"
   (lambda (_)
     (or (git-completing-read "Merge" (git-rebase-buffer-labels))
         ""))
   arg))

(defun git-rebase-merge-toggle-editmsg ()
  "Toggle whether an editor is invoked when performing the merge at point.
When a merge command uses a lower-case -c, the message for the
specified commit will be opened in an editor before creating the
commit.  For an upper-case -C, the message will be used as is."
  (interactive)
  (with-slots (action-type target action-options trailer)
      (git-rebase-current-line)
    (if (eq action-type 'merge)
        (let ((inhibit-read-only t))
          (git-delete-line)
          (insert
           (format "merge %s %s %s\n"
                   (replace-regexp-in-string
                    "-[cC]" (lambda (c)
                              (if (equal c "-c") "-C" "-c"))
                    action-options t t)
                   target
                   trailer)))
      (ding))))

(defun git-rebase-set-bare-action (action arg)
  (goto-char (line-beginning-position))
  (with-slots ((ln-action action) comment-p)
      (git-rebase-current-line)
    (let ((same-action-p (equal action ln-action))
          (inhibit-read-only t))
      (when (or arg
                (not ln-action)
                (not same-action-p)
                (and same-action-p comment-p))
        (unless (or arg (not same-action-p))
          (git-delete-line))
        (insert action ?\n)
        (unless git-rebase-auto-advance
          (forward-line -1))))))

(defun git-rebase-noop (&optional arg)
  "Add noop action at point.

If the current line already contains a noop action, leave it
unchanged.  If there is a commented noop action present, remove
the comment.  Otherwise add a new noop action.  With a prefix
argument insert a new noop action regardless of what is already
present on the current line.

A noop action can be used to make git perform a rebase even if
no commits are selected.  Without the noop action present, git
would see an empty file and therefore do nothing."
  (interactive "P")
  (git-rebase-set-bare-action "noop" arg))

(defun git-rebase-break (&optional arg)
  "Add break action at point.

If there is a commented break action present, remove the comment.
If the current line already contains a break action, add another
break action only if a prefix argument is given.

A break action can be used to interrupt the rebase at the
specified point.  It is particularly useful for pausing before
the first commit in the sequence.  For other cases, the
equivalent behavior can be achieved with `git-rebase-edit'."
  (interactive "P")
  (git-rebase-set-bare-action "break" arg))

(defun git-rebase-undo (&optional arg)
  "Undo some previous changes.
Like `undo' but works in read-only buffers."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

;;; Mode

;;;###autoload
(define-derived-mode git-rebase-mode special-mode "Git Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run 'git rebase -i' or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running 'man git-rebase' at the command line) for details."
  :group 'git-rebase
  (setq comment-start
        (or (ignore-errors
              (car (process-lines "git" "config" "core.commentchar")))
            "#"))
  (setq git-rebase-comment-re (concat "^" (regexp-quote comment-start)))
  (setq font-lock-defaults (list (git-rebase-mode-font-lock-keywords) t t))
  (unless git-rebase-show-instructions
    (let ((inhibit-read-only t))
      (flush-lines git-rebase-comment-re)))
  (when (boundp 'save-place)
    (setq save-place nil)))

(defun git-rebase-match-comment-line (limit)
  (re-search-forward (concat git-rebase-comment-re ".*") limit t))

(defun git-rebase-mode-font-lock-keywords ()
  "Font lock keywords for Git-Rebase mode."
  `((,(concat "^" (cdr (assq 'commit git-rebase-line-regexps)))
     (1 'font-lock-keyword-face)
     (3 'git-rebase-hash)
     (4 'git-rebase-description))
    (,(concat "^" (cdr (assq 'exec git-rebase-line-regexps)))
     (1 'font-lock-keyword-face)
     (3 'git-rebase-description))
    (,(concat "^" (cdr (assq 'bare git-rebase-line-regexps)))
     (1 'font-lock-keyword-face))
    (,(concat "^" (cdr (assq 'label git-rebase-line-regexps)))
     (1 'font-lock-keyword-face)
     (3 'git-rebase-label)
     (4 'font-lock-comment-face))
    ("^\\(m\\(?:erge\\)?\\) -[Cc] \\([^ \n]+\\) \\([^ \n]+\\)\\( #.*\\)?"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-hash)
     (3 'git-rebase-label)
     (4 'font-lock-comment-face))
    ("^\\(m\\(?:erge\\)?\\) \\([^ \n]+\\)"
     (1 'font-lock-keyword-face)
     (2 'git-rebase-label))
    (,(concat git-rebase-comment-re " *"
              (cdr (assq 'commit git-rebase-line-regexps)))
     0 'git-rebase-killed-action t)
    (git-rebase-match-comment-line 0 'font-lock-comment-face)
    ("\\[[^[]*\\]"
     0 'git-rebase-keyword t)
    (,(format "^%s Rebase \\([^ ]*\\) onto \\([^ ]*\\)" comment-start)
     (1 'git-rebase-comment-hash t)
     (2 'git-rebase-comment-hash t))
    (,(format "^%s \\(Commands:\\)" comment-start)
     (1 'git-rebase-comment-heading t))
    (,(format "^%s Branch \\(.*\\)" comment-start)
     (1 'git-rebase-label t))))

(defun git-rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (and git-rebase-show-instructions
                 (re-search-forward
                  (concat git-rebase-comment-re "\\s-+p, pick")
                  nil t))
        (goto-char (line-beginning-position))
        (pcase-dolist (`(,cmd . ,desc) git-rebase-command-descriptions)
          (insert (format "%s %-8s %s\n"
                          comment-start
                          (substitute-command-keys (format "\\[%s]" cmd))
                          desc)))
        (while (re-search-forward (concat git-rebase-comment-re
                                          "\\(  ?\\)\\([^\n,],\\) "
                                          "\\([^\n ]+\\) ")
                                  nil t)
          (let ((cmd (intern (concat "git-rebase-" (match-string 3)))))
            (if (not (fboundp cmd))
                (delete-region (line-beginning-position) (1+ (line-end-position)))
              (replace-match " " t t nil 1)
              (replace-match
               (format "%-8s"
                       (mapconcat #'key-description
                                  (--remove (eq (elt it 0) 'menu-bar)
                                            (reverse (where-is-internal
                                                      cmd git-rebase-mode-map)))
                                  ", "))
               t t nil 2))))))))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-show-keybindings t)

(defun git-rebase-mode-disable-before-save-hook ()
  (set (make-local-variable 'before-save-hook) nil))

(add-hook 'git-rebase-mode-hook 'git-rebase-mode-disable-before-save-hook)

;;;###autoload
(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")
;;;###autoload
(add-to-list 'auto-mode-alist
             (cons git-rebase-filename-regexp 'git-rebase-mode))

(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude git-rebase-filename-regexp))

;;; _
(provide 'git-rebase-mode)
;;; git-rebase-mode.el ends here
