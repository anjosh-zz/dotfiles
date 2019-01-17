; list the packages you want
(setq package-list '(evil
                     projectile
                     helm
                     helm-projectile
                     helm-ag
                     org
                     magit
                     evil-org
                     evil-surround
                     evil-escape
                     js2-mode
                     tern
                     company
                     company-tern
                     neotree
                     groovy-mode
                     git-link
                     flycheck
                     ox-pandoc
                     solarized-theme
                     zenburn-theme
                     go-mode
                     vue-mode
                     elpy
                     rjsx-mode
                     ))

; list the repositories containing them
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(or (file-exists-p package-user-dir) (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq evil-want-C-u-scroll t)
(evil-mode t)

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(setq helm-display-header-line nil) ;; t by default

(helm-autoresize-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(global-linum-mode t)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(desktop-save-mode 1)

(require 'org)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'org-mode-hook (lambda () (linum-mode 0))) ; linum mode was slowing down large org files
(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))

(setq org-agenda-files
      (append
       (file-expand-wildcards "~/*.org")
       (file-expand-wildcards "~/Dropbox/GTD/*.org")))

(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/GTD/gtd.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("w" "Work" entry (file+headline "~/todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")))

(setq org-refile-targets
      '((nil :maxlevel . 1)
        (org-agenda-files :maxlevel . 1)
        (nil :regexp . "Analytics")
        (nil :regexp . "New Tooling")
        (nil :regexp . "relocation")
        (nil :regexp . "Council")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-tern))

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

; function to manually kill tern process
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(eval-after-load 'evil-maps
  '(define-key evil-normal-state-map (kbd "M-.") nil))

(define-key evil-insert-state-map "\M-p" 'yank)

(global-set-key [f8] 'neotree-toggle)

(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(require 'evil-surround)
(global-evil-surround-mode 1)


(eval-after-load "git-link"
  '(progn
    (add-to-list 'git-link-remote-alist
      '("github.ibm.com" git-link-github))
    (add-to-list 'git-link-commit-remote-alist
      '("github.ibm.com" git-link-commit-github))))

(setq git-link-default-remote "upstream")
(setq git-link-default-branch "develop")

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override t)

(require 'evil-escape)
(evil-escape-mode)

(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g4\\'" . antlr-mode) auto-mode-alist))
(add-hook 'speedbar-load-hook  ; would be too late in antlr-mode.el
          (lambda () (speedbar-add-supported-extension ".g4")))

(setq javascript-indent-level 2) ; javascript-mode
(setq js-indent-level 2) ; js-mode
(setq js2-basic-offset 2) ; js2-mode, in latest js2-mode, it's alias of js-indent-level

(setq evil-shift-width 2)

(elpy-enable) ; python editing

(load-theme 'zenburn t)

(add-to-list 'exec-path "/usr/local/bin/")
(add-hook 'with-editor-mode-hook 'evil-insert-state)

(defun buffer-too-big-p ()
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))
(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))

; fix tern not able to find node
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
