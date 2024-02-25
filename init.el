(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(global-hl-line-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-resh-contents)
  (package-install 'use-package))

(setq backup-directory-alist `(("." . "~/.saves")))

(display-time-mode t)

(display-battery-mode 1)

(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'tab-width 4)
(customize-set-variable 'tab-stop-list '(4 8 12))

(set-frame-font "InconsolataGo Nerd Font 14" nil t)

;; customize path for hasekll
(setenv "PATH" (concat "/home/user/.ghcup/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/home/user/.ghcup/bin")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOOM Theme
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t)
  (setq neo-global--window nil)
  (setq hl-line-sticky-flag nil)
  (setq neo-vc-integration nil)
  (setq neotree-dir-button-keymap nil)
  (setq neotree-file-button-keymap nil)
  (setq neo-path--file-short-name nil)
  (setq neo-vc-for-node nil)
  (setq neo-buffer--insert-fold-symbol nil)
  (setq neo-buffer--node-list-set nil)
  (setq neo-buffer--newline-and-begin nil)
  (setq neo-global--select-window nil)
  (setq neo-buffer--insert-file-entry nil)
  (setq neo-buffer--insert-dir-entry nil)
  (setq neo-buffer--insert-root-entry nil))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL CODING
;;;;;;;;;;;;;;;;;;;;;;;;;

;; completion
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.5)
  :config
  (global-company-mode 1))

;; project management
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

;; flycheck
(use-package flycheck
  :ensure t)

;; which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package helm
  :ensure t
  :config
  (setq
   helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
   helm-quick-update t ; do not display invisible candidates
   helm-idle-delay 0.1 ; be idle for this many seconds, before updating in delayed sources.
   helm-input-idle-delay 0.01 ; be idle for this many seconds, before updating candidate buffer
   helm-split-window-default-side 'other ;; open helm buffer in another window
   helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window
   helm-candidate-number-limit 200 ; limit the number of displayed canidates
   helm-move-to-line-cycle-in-source nil ; move to end or beginning of source when reaching top or bottom of source.
   ;; helm-command
   helm-M-x-requires-pattern 0     ; show all candidates when set to 0
   )
  (bind-keys ("M-x" . helm-M-x)
             ("M-y" . helm-show-kill-ring)
             ("C-x b" . helm-mini))
  (bind-keys :map helm-map
             ("C-o" . nil)
             ("TAB" . helm-execute-persistent-action)
             ("C-i" . helm-execute-persistent-action)
             ("C-z" . helm-select-action)
             ("C-h" . delete-backward-char)))

(use-package helm-files
  :bind ("C-x C-f" . helm-find-files)
  :config
  (setq
   helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.
   helm-boring-file-regexp-list
   '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$") ; do not show these files in helm buffer
   helm-ff-file-name-history-use-recentf t
   ;; helm-buffers   
   helm-buffers-fuzzy-matching t          ; fuzzy matching buffer names when non--nil
                                          ; useful in helm-mini that lists buffers
   ;; ido
   ido-use-virtual-buffers t     ; Needed in helm-buffers-list
   )
  (setq helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                            '(picture-mode artist-mode)))
  (bind-keys :map helm-find-files-map
             ("C-h" . delete-backward-char)
             ("C-i" . helm-execute-persistent-action)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :commands lsp-after-initialize-hook
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (setq lsp-clients-clangd-args '("--background-index" "--clang-tidy"))

  ;; performance tweaks
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  
  :hook
  (lsp-ui-mode . lsp-mode)
  (clojure-mode . lsp-mode)
  (csharp-mode . lsp-mode)
  (c++-mode . lsp-mode)
  (c++-mode . (lambda () (local-set-key (kbd "C-M-i") #'company-complete)))
  (scala-mode . lsp-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package lsp-treemacs
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; dap mode
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-select-executable ()
  "Prompt the user to select an executable file."
  (read-file-name "Select executable: " (lsp-workspace-root)))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-lldb)
  (require 'dap-cpptools)
  ;; Add more dap-mode related configurations here
  (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode"))
  (dap-register-debug-template "LLDB::Run"
                               (list :type "lldb-vscode"
                                     :request "launch"
                                     :name "CMake::LLDB Run"
                                     :executable (lambda () (read-file-name "Select executable: " (lsp-workspace-root)))
                                     :args ""
                                     :cwd (lsp-workspace-root)
                                     :environment-variables '()
                                     :lldb-vscode-executable "/usr/bin/lldb-vscode"))
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; GLSL
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package glsl-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-java
  :ensure t
  :config
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true"
            )

        lsp-java-java-path "/usr/lib/jvm/java-17-openjdk-amd64/bin/java"

        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil

        ;; Don't format my source code (I use Maven for enforcing my
        ;; coding style)
        lsp-java-format-enabled nil)
  (add-hook 'java-mode-hook 'lsp))

(defun my-java-mode-hook ()
  (auto-fill-mode)
  (flycheck-mode)
  (git-gutter+-mode)
  (subword-mode)
  (yas-minor-mode)
  (when window-system
    (set-fringe-style '(8 . 0)))

  ;; Fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (if (assoc 'inexpr-class c-offsets-alist)
      (c-set-offset 'inexpr-class 0))

  ;; Indent arguments on the next line as indented body.
  (c-set-offset 'arglist-intro '++))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cider
  :ensure t)
  
(use-package clojure-mode
  :ensure t)

(use-package paredit
  :ensure t
  :hook (clojure-mode . paredit-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; C# csharp
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csharp-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; F# fsharp
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fsharp-mode
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  ;; :ensure org-plus-contrib
  :config
  (setq org-hide-emphasis-markers t)
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :ensure t)
(org-roam-db-autosync-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dap-lldb dap-ui company-lsp org-roam glsl-mode lsp-metals lsp-haskell haskell-mode smartparens magit fsharp-mode fsharp csharp-mode rustic lsp-ui helm which-key paredit org-bullets org-plus-contrib doom-modeline doom-themes all-the-icons use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bbc2cf" :family "Sans Serif")))))
