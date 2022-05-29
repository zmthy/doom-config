;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 14 :weight 'regular))

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

(defun doom/ediff-init-and-example ()
  "Open an ediff session to compare init.example.el and init.el."
  (interactive)
  (ediff-files (concat doom-private-dir "init.el")
               (concat doom-emacs-dir "init.example.el")))

(define-key! help-map
  "di" #'doom/ediff-init-and-example)

(run-at-time 0 300 'match-macos-theme)

(after! magit
  (transient-append-suffix 'magit-fetch "r" '("i" "into local" magit-fetch-into-local))
  (transient-append-suffix 'magit-push "-n" '("-s" "Skip Gitlab CI" "--push-option=ci.skip")))

(dolist (hook '(LaTeX-mode-hook
                TeX-mode-hook
                markdown-mode-hook
                org-mode-hook
                text-mode-hook))
  (add-hook hook #'auto-fill-mode))

(setq-hook! 'python-mode-hook
  fill-column 79)

(setq-hook! 'typescript-mode-hook
  fill-column 120)

(add-hook 'js-mode-hook #'prettier-mode)
(add-hook 'js-jsx-mode-hook #'prettier-mode)
(add-hook 'typescript-mode-hook #'prettier-mode)
(add-hook 'typescript-tsx-mode-hook #'prettier-mode)

(add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv)
(add-hook 'window-configuration-change-hook #'auto-virtualenv-set-virtualenv)

;; Add C-d key binding to vterm
(map! :after vterm
      :map vterm-mode-map
      :ni "C-c C-d" #'vterm-send-C-d)

(defun python-coverage-report ()
  "Provide a coverage report after running pytest with the cov plugin."
  (interactive)
  (projectile-run-async-shell-command-in-root "coverage report -m"))

(map! :after python
      :localleader
      :map python-mode-map
      (:prefix ("c" . "code")
       :desc "Toggle coverage overlay" "c" #'python-coverage-overlay-mode
       :desc "Coverage report" "r" #'python-coverage-report))

(defun brew-services-restart-nginx ()
  "Restart nginx."
  (interactive)
  (shell-command "brew services restart nginx")
  (princ 'done))

(map! :leader
      (:prefix ("!" . "execute")
       :desc "Restart nginx" "n" #'brew-services-restart-nginx))

(defun pre-commit-run-all-files ()
  "Run pre-commit on every file in the current project."
  (interactive)
  (projectile-run-async-shell-command-in-root "pre-commit run --all-files"))

(map! :leader
      (:prefix "g"
       :desc "Run pre-commit hook" "h" #'pre-commit-run-all-files))

;; Add leader-mapped commands for flycheck (over the existing error list map)
(after! flycheck
  (map! :leader
        (:prefix-map ("c" . "code")
         "x" flycheck-command-map))
  (setq-default flycheck-python-mypy-config (cons ".mypy.ini" flycheck-python-mypy-config)))

(setq-hook! 'python-mode-hook
  flycheck-checker 'python-pylint)

(after! python
  (add-hook 'before-save-hook 'py-isort-before-save))

;; Set up Python debugger
(after! dap-mode
  (setq dap-python-debugger 'debugpy))
