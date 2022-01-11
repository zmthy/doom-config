;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Timothy Jones"
      user-mail-address "tim@zmthy.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "InconsolataLGC Nerd Font" :size 11 :weight 'semi-light))
;; (setq doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Do not prompt to quit emacs.
(setq confirm-kill-emacs nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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
  (setq doom-theme (if (dark-modep) 'doom-one 'doom-one-light))
  (doom/reload-theme))

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
