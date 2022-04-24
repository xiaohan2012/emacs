(require 'package)


;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; install 'use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package monokai-theme
  :ensure t
  )

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package rainbow-delimiters
   :ensure t
   :init
   (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
   (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode))

; or (rainbow-delimiters-mode 1) for global mode

(use-package rainbow-mode
:ensure t
:init (rainbow-mode 1))

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-package dashboard
:ensure t
:config
(dashboard-setup-startup-hook)
(setq dashboard-items '((projects . 10)
			(recents . 5)))
(setq dashboard-banner-logo-title "Hello Han."))

(use-package company
:ensure t
:init
(add-hook 'after-init-hook 'global-company-mode)) ;; global mode, do we need it

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(setq org-src-window-setup 'current-window)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; (define-key org-mode-map (kbd "C-c s") 'org-insert-structure-template)

(defun org-hide-sublevels ()
  (interactive)
  (hide-sublevels 1))

(global-set-key (kbd "C-c h s") 'org-hide-sublevels)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name))))
    (when filename
      (with-temp-buffer
	(insert filename)
	(clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun onboarding-org-visit ()
"visit ~/docs/notes/onboarding.org"
(interactive)
(find-file "~/docs/notes/onboarding.org"))
(global-set-key (kbd "C-c o o") 'onboarding-org-visit)

(defun dmrs-org-visit ()
"visit ~/docs/notes/dmrs.org"
(interactive)
(find-file "~/docs/notes/dmrs.org"))
(global-set-key (kbd "C-c o d") 'dmrs-org-visit)

(use-package switch-window

  :ensure t
  :init
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
  (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
  (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
  (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

  (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
  (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
  (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

  (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
  (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

  (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer))

  (setq switch-window-threshold 2)
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-shortcut-style 'qwerty)

(defun split-window-and-follow-vertically ()
(interactive)
(split-window-below)
(balance-windows)
(other-window 1))
(global-set-key (kbd "C-x 2") 'split-window-and-follow-vertically)

(defun split-window-and-follow-horizontally ()
(interactive)
(split-window-right)
(balance-windows)
(other-window 1))
(global-set-key (kbd "C-x 3") 'split-window-and-follow-horizontally)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-expert t)

(defun kill-and-close-this-buffer ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(global-set-key (kbd "C-c k") 'kill-and-close-this-buffer)

(defun kill-all-buffers ()
(interactive)
(mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-k") 'kill-all-buffers)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

(defun projectile-project-type-to-python-pip ()
  (interactive)
  (setq projectile-project-type 'python-pip)
  )
(global-set-key (kbd "C-c t p p") 'projectile-project-type-to-python-pip)

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching nil
	ido-create-new-buffer 'always
	ido-everywhere t)
  (add-to-list 'ido-ignore-files "\.bak")
  (add-to-list 'ido-ignore-files "\.log")
  (add-to-list 'ido-ignore-files ".venv")
  (add-to-list 'ido-ignore-files "__pycache__")
  (add-to-list 'ido-ignore-files "\.pytest_cache")
  (add-to-list 'ido-ignore-files "\.pkl")
; data files
  (add-to-list 'ido-ignore-files "\.hdf5")
; latex-related
  (add-to-list 'ido-ignore-files "\.nav")
  (add-to-list 'ido-ignore-files "\.out")
  (add-to-list 'ido-ignore-files "\.pdf")
  (add-to-list 'ido-ignore-files "\.snm")
  (add-to-list 'ido-ignore-files "\.synctex.gz")    
  (ido-mode 1)
  )

(use-package ido-vertical-mode
  :ensure t
  :requires ido
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  )

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(use-package avy
:ensure t
:bind
("M-s" . avy-goto-char))

;; (use-package sublimity
	;;   :ensure t
	;;   :config
	;;   (sublimity-mode 1))

	;; (use-package sublimity-scroll
	;;   :ensure t
	;;   :config
	;;   (sublimity-mode 1))
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (sublimity-mode 1)
      ;; (require 'sublimity-scroll)

(defun activate-virtualenv ()
  "pyenv-activate the current directory + '.venv'
    in the future, the name of virtualenv should be specified as input
    "
  (interactive)
  (message "activating virtualenv")
  (pyvenv-activate
   (expand-file-name
    ".venv" default-directory))
  (setq elpy-rpc-virtualenv-path 'current)  ; set path to Python interpreter correctly
  )

(global-set-key (kbd "C-c a v") 'activate-virtualenv)

(use-package flycheck
  :ensure t)

(use-package ein
  :ensure t
  )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-test-runner 'elpy-test-pytest-runner ; use pytest
	elpy-rpc-backend "jedi"
	;; elpy-rpc-project-specific 't
	elpy-modules (delq 'elpy-module-flymake elpy-modules)
	)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

(use-package cython-mode
  :ensure t)

(use-package tex
      :defer t
      :ensure auctex
      :config
      (setq TeX-auto-save t)
      (setq TeX-save-query nil)
      )

; for MacOS: environment variable fix 
    (setenv "PATH" 
	    (concat
	      "/usr/local/bin/" ":" "/Library/TeX/texbin/" ":"
	      (getenv "PATH")))

; forward/reverse search between PDF and Latex source
(defun my/latex-buffer-setup ()
  (TeX-source-correlate-mode)
  (TeX-PDF-mode))

(add-hook 'LaTeX-mode-hook 'my/latex-buffer-setup)
(setq TeX-source-correlate-method 'synctex
      TeX-view-program-list   ;; Use Skim, it's awesome
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
      TeX-view-program-selection '((output-pdf "Skim"))
      TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      ;; TeX-master 'dwim
      )

(setq-default TeX-master "main") ; all master files called "main".

(when (and (eq system-type 'gnu/linux)
	   (file-exists-p "/home/xiaoh1/code/matlab-emacs-src"))
  (add-to-list 'load-path "/home/xiaoh1/code/matlab-emacs-src")
  (load-library "matlab-load"))

(use-package yaml-mode
:ensure t
:config
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/elpa/yasnippet-snippets-20220221.1234/snippets"
	  ))
  ;; "~/.emacs.d/elpa/elpy-20220220.2059/"  ; might need to change
  ;; "~/.emacs.d/elpa/yasnippet-snippets-20220221.1234/snippets"  ; might need to change  
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
:ensure t
)

(defun config-visit ()
"visit ~/.emacs.d/config.org"
(interactive)
(find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
(global-set-key (kbd "C-c t m") 'ansi-term)  ; why does not work? which key is super key?

(defun zshrc-visit ()
  "visit ~/.zshrc"
  (interactive)
  (find-file "~/.zshrc"))
(global-set-key (kbd "C-c z") 'zshrc-visit)

(line-number-mode 1)
(column-number-mode 1)

(global-set-key (kbd "M-o")  'mode-line-other-buffer)
(tool-bar-mode -1)
(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1))


(setq split-width-threshold 1 )   ; horizontal split window

(defalias 'yes-or-no-p 'y-or-n-p)



(setq scroll-conservatively 100)


(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore) ; no warning sound

(when window-system
  (global-hl-line-mode t)
  (global-prettify-symbols-mode t)
  )

(setq make-backup-files nil)
(setq auto-save-default nil)


(show-paren-mode 1)

(global-set-key (kbd "C-c c l") 'avy-copy-line)  ; copy a line
(global-set-key (kbd "C-c d l") 'avy-kill-whole-line)  ; kill&save a line
(global-set-key (kbd "C-c c r") 'avy-copy-region)  ; copy a region
(global-set-key (kbd "C-c d r") 'avy-kill-region)  ; kill&save a region

(global-subword-mode 1)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    ;; (?\' . ?\')  ; 
			    (?\" . ?\")
			    (?\` . ?\`)
			    (?\$ . ?\$)
))
(electric-pair-mode t)

(defun my-kill-whole-word ()
(interactive)
(backward-word)
(kill-word 1)
)

(global-set-key (kbd "C-c w w") 'my-kill-whole-word)

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol))))
  )
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(use-package hungry-delete
:ensure t
:config (global-hungry-delete-mode))

(use-package spaceline
:ensure t
:config
(require 'spaceline-config)
(setq powerline-default-separator (quote arrow))
(spaceline-spacemacs-theme))

(use-package diminish
:ensure t
:init 
(diminish 'hungry-delete-mode)
(diminish 'which-key-mode)
(diminish 'rainbow-mode)
(diminish 'beacon-mode)
(diminish 'subword-mode)
)

(use-package dmenu
    :ensure t
    :bind
    ("C-c d m" . 'dmenu))

(use-package symon
  :ensure t
  :bind
  ("C-c s" . symon-mode))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package magit
  :ensure t)
