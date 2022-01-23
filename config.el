;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 14 :weight 'regular))

(defmacro add-hooks (&rest hooks)
  "Register a number of hooks at once, with the hook names auto-quoted.
Provide program bodies rather than functions for the hook values."
  `(progn . ,(mapcar (lambda (hook)
                       `(add-hook ',(car hook)
                                  (lambda () (progn . ,(cdr hook)))))
                     hooks)))

(defmacro add-text-modes (&rest hooks)
  "Register all of the given hooks as having the expected behaviour of a
text-mode."
  `(add-hooks . ,(mapcar (lambda (hook)
                           `(,hook (setq sentence-end-double-space t)
                                   (auto-fill-mode)))
                         hooks)))

(defmacro hook-setq (&rest binds)
  "Register a function for a hook that sets the given variables."
  `(add-hooks . ,(mapcar (lambda (bind)
                           `(,(car bind) (setq-default . ,(cdr bind))))
                         binds)))


(defun applescript (lines)
  "Executes a list of lists of primitive objects as AppleScript.
Returns the exit code."
  (when-let ((osascript (executable-find "osascript")))
    (with-temp-buffer
      (let ((buffer (current-buffer)))
        (dolist (line lines)
          (when line
            (prin1 (car line) buffer)
            (dolist (object (cdr line))
              (insert " ")
              (prin1 object buffer)))
          (insert "\n")))
      (call-process-region (point-min) (point-max) osascript))))

(defun dark-modep ()
  "Determine if macOS is currently in dark mode."
  (zerop (applescript '((tell application "System Events")
                        (tell appearance preferences)
                        (if get dark mode is false then)
                        (error number 1)
                        (end if)
                        (end tell)
                        (end tell)))))

(defun match-macos-theme ()
  "Update the theme to light or dark depending on current the macOS theme."
  (interactive)
  (let ((target-theme (if (dark-modep) 'doom-one 'doom-one-light)))
    (if (not (equal doom-theme target-theme))
        (load-theme target-theme))))

(defun magit-fetch-into-local (remote branch args)
  "Fetch a remote branch into the matching local branch."
  (interactive
   (list (magit-read-remote-or-url "Fetch from remote or url" "origin")
         (magit-read-branch "Fetch into local branch" "develop")
         (magit-fetch-arguments)))
  (magit-git-fetch remote (cons (concat branch ":" branch) args)))

(run-at-time 0 300 'match-macos-theme)

(after! magit
  (transient-append-suffix 'magit-fetch "r" '("i" "into local" magit-fetch-into-local))
  (transient-append-suffix 'magit-push "-n" '("-s" "Skip Gitlab CI" "--push-option=ci.skip")))

(add-hook 'typescript-mode-hook 'deno-fmt-mode)
