; list the packages you want
(setq package-list '(evil
                     projectile
                     helm
                     org
                     evil-leader
                     evil-org
                     js2-mode
                     tern
                     tern-auto-complete
                     neotree
                     evil-surround
                     evil-escape
                     groovy-mode
                     git-link
                     flycheck
                     ox-pandoc
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

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

(load-theme 'zenburn t)
(global-linum-mode t)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)

(evil-jumper-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(desktop-save-mode 1)

(require 'org)
(require 'evil-org)
(setq org-log-done t)
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

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(antlr-tool-command
   "java -jar ~/Downloads/query-parser-with-dependencies.jar -p -q=")
 '(evil-shift-width 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(org-agenda-files (quote ("~/todo.org" "~/Dropbox/GTD/gtd.org"))))

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

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

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

(require 'evil-escape)
(evil-escape-mode)

(autoload 'antlr-mode "antlr-mode" nil t)
(setq auto-mode-alist (cons '("\\.g4\\'" . antlr-mode) auto-mode-alist))
(add-hook 'speedbar-load-hook  ; would be too late in antlr-mode.el
          (lambda () (speedbar-add-supported-extension ".g4")))
