;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'epa-file)
(epa-file-enable)
(load "~/.doom.d/parameters.el.gpg")

(setq user-full-name user-full-name-param
      user-mail-address user-mail-address-param)

(setq doom-theme 'doom-nord)

(setq doom-font (font-spec :family "Fira Code" :style "Retina" :size 14 :height 1.0)
      doom-big-font (font-spec :family "Fira Code" :style "Retina" :size 28 :height 1.0))

;;(setq doom-font (font-spec :family "Source Code Pro" :style "Regular" :size 14))
;;      doom-variable-pitch-font (font-spec :family "ETBembo" :style "RomanLF" :size 18 :height 1.0))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type  t)

(setq fancy-splash-image (concat doom-private-dir "splash/I-am-doom-small.png"))

(require 'beacon)
(beacon-mode 1)
(setq beacon-blink-when-point-moves-horizontally 1)
(setq beacon-blink-when-point-moves-vertically 1)
(setq beacon-dont-blink-commands nil)

(evil-snipe-mode +1)
(evil-snipe-override-mode +1)

(setq evil-snipe-repeat-scope 'buffer)

(evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
(evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S)

(map! :leader
        :desc "Avy goto" "SPC" #'avy-goto-char-2)

(setq avy-all-windows 'all-frames)

(require 'multiple-cursors)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(setq treemacs-is-never-other-window nil)

(defun application-activate (application-name)

(interactive)

  (let ((script (format "tell application \"%s\" \n activate \n end tell" application-name)))
  (start-process "application-activate" nil "osascript" "-e" script)))

(defun firefox-activate ()
(interactive)
(application-activate "Firefox"))

(global-set-key (kbd "M-s-2") 'firefox-activate)

(defun pycharm-activate ()
(interactive)
(application-activate "PyCharm"))

(global-set-key (kbd "M-s-3") 'pycharm-activate)

(defun slack-activate ()
(interactive)
(application-activate "Slack"))

(global-set-key (kbd "M-s-4") 'slack-activate)

(defun calendar-activate ()
(interactive)
(application-activate "Calendar"))

(global-set-key (kbd "M-s-5") 'calendar-activate)

(defun spotify-activate ()
(interactive)
(application-activate "Spotify"))

(global-set-key (kbd "M-s-6") 'spotify-activate)

(use-package multi-vterm)

(setq auto-save-default t
      make-backup-files t)

;;(add-to-list 'auto-mode-alist '("\\.md\\'" . fundamental-mode))

(require 'company-lsp)
(push 'company-lsp company-backends)

(setq org-directory org-directory-param)
(setq org-support-shift-select t)
(setq org-startup-folded 'fold)

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-ellipsis " ▼")

(use-package org-roam
  :after org
  :init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  (org-roam-directory (file-truename org-roam-directory-param))
  :config
  (org-roam-setup)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
           (:map org-mode-map
            (("C-c n i" . org-roam-node-insert)
             ("C-c n o" . org-id-get-create)
             ("C-c n t" . org-roam-tag-add)
             ("C-c n a" . org-roam-alias-add)
             ("C-c n l" . org-roam-buffer-toggle)))))

(map! :leader
       (:prefix ("r" . "org-roam")
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Get random node" "r" #' org-roam-node-random))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(map! :leader
        :desc "Org Present" "<up>" #'org-present)

(map! :leader
        :desc "Org Present" "<down>" #'org-present-quit)

(map! :leader
        :desc "Org Present Next" "<right>" #'org-present-next)

(map! :leader
        :desc "Org Present Prev" "<left>" #'org-present-prev)

;; Centering Org Documents
;; Configure fill width, used in conjuntion with writeroom-mode
(setq visual-fill-column-width 75
      visual-fill-column-center-text t)

;;Org Present

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(defun my/org-present-start ()

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")

  ;; Display inline images automatically
  (org-display-inline-images)

  ;; Center the presentation, wrap lines, and hide modelines
  (writeroom-mode 1)

  ;; Hide line numbers
  (global-display-line-numbers-mode 0)
)

(defun my/org-present-end ()

  ;; Clear the header line string so that it isn't displayed
  (setq header-line-format nil)

  ;; Stop displaying inline images
  (org-remove-inline-images)

  ;; Stop centering the document and wrapping lines; and show modelines
  (writeroom-mode 0)

  ;; Return line numbers
  (global-display-line-numbers-mode 1)
)

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)
(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(setq projectile-project-search-path projectile-project-search-path-param)

(setq magit-refresh-status-buffer nil)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" pyenv-directory-param))

(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;; CONDA
;;(require 'conda)
;;(setq conda-env-home-directory "$CASKROOM-PATH-PARAM/miniconda/base/condabin/conda")
;;(custom-set-variables
;; '(conda-anaconda-home "$CASKROOM-PATH-PARAM/miniconda/base/"))
;;(conda-env-initialize-interactive-shells)
;;(conda-env-initialize-eshell)

(use-package lsp-java
:ensure t
:config (add-hook 'java-mode-hook 'lsp))

(setenv "JAVA_HOME" java-home-param)
(setq lsp-java-java-path lsp-java-java-path-param)

;; Enable scala-mode and sbt-mode
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

;; Add company-lsp backend for metals
(use-package company-lsp)

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable lsp-sourcekit-executable-param))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(require 'ejc-sql)
(setq nrepl-sync-request-timeout nil)

(ejc-create-connection
   ejc-connection-name-param
   :dependencies ejc-dependencies-param
   :classpath ejc-classpath-param
   :connection-uri ejc-connection-uri-param
   )

(setq ejc-result-table-impl 'orgtbl-mode)

(add-hook 'ejc-sql-connected-hook
          (lambda ()
            (ejc-set-fetch-size 50)
            (ejc-set-max-rows 50)
            (ejc-set-show-too-many-rows-message t)
            (ejc-set-column-width-limit 1000)
            (ejc-set-use-unicode t)))

(require 'pocket-reader)
