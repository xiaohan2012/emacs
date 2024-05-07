(setenv "PATH" (concat ":/usr/local/bin/" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin/")

(setenv "PATH" (concat ":/usr/bin/" (getenv "PATH")))
(add-to-list 'exec-path "/usr/bin/")

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns x))
	  (exec-path-from-shell-initialize))
  )

(use-package compat
  :ensure t)

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

(use-package quelpa
  :ensure t)

(use-package monokai-theme
  :ensure t
  )

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(setq-default cursor-type 'bar)
(set-cursor-color "#fdda9a")

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

(set-face-attribute 'default nil :height 150)

(set-face-attribute 'region nil :background "#666")

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
  (diminish 'smartparens-mode)
  (diminish 'lsp-lens-mode)
  (diminish 'auto-revert-mode)
  :hook
  (lsp-mode . (lambda ()
		(diminish 'lsp-lens-mode)
		(diminish 'lsp-mode "LSP")
		(diminish 'projectile-mode "projt")
		(diminish 'smartparens-mode)
		(diminish 'auto-revert-mode)
		))
  )

(use-package dashboard
:ensure t
:config
(dashboard-setup-startup-hook)
(setq dashboard-items '((projects . 10)
			(recents . 5)))
(setq dashboard-banner-logo-title "Hello Han."))

(use-package company
:ensure t
:hook
(LaTeX-mode . company-mode)
(emacs-lisp-mode . company-mode)
:bind
(:map company-active-map ("<tab>" . company-complete-selection))

)

;; (add-hook 'LaTeX-mode-hook 'my/latex-buffer-setup)

;; (use-package corfu
;;   :ensure t
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :init
;;   ;; (global-corfu-mode) ;; disable it for nowy
;;   )

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   (setq completion-cycle-threshold 3)

;;   ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
;;   ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable indentation+completion using the TAB key.
;;   ;; `completion-at-point' is often bound to M-TAB.
;;   (setq tab-always-indent 'complete))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
	 :map minibuffer-local-map
	 ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package flyspell
  :config
  ; enable flycheck for certain modes
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  ; disable flycheck for certain mode
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(setq org-src-window-setup 'current-window)

(setq org-table-default-size "2x3")

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; (define-key org-mode-map (kbd "C-c s") 'org-insert-structure-template)

(defun org-hide-sublevels ()
  (interactive)
  (hide-sublevels 1))

(global-set-key (kbd "C-c h s") 'org-hide-sublevels)


;; hide lists by default
(setq org-cycle-include-plain-lists 'integrate)

;; hide all levels for default
(setq org-startup-folded t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

;; Making emacs find latex (so that C-c C-x C-l works on orgmode)
;; On MacOS
(setenv "PATH" (concat ":/Library/TeX/texbin/" (getenv "PATH")))
(add-to-list 'exec-path "/Library/TeX/texbin/")

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(add-to-list 'org-latex-packages-alist '("" "han-macros" t))  ;; use t not nil
;; (print org-latex-packages-alist)

(defun my/remove-latex-image-dir ()
  (interactive)
  (let ((dirname (concat
		  (file-name-directory (buffer-file-name))
		  "ltximg"
		  )))
    (if (file-directory-p dirname)
	(progn
	  (delete-directory dirname t)
	  (message (format "%s deleted" dirname))
	  )
      (message (format "%s does not exist" dirname))
      )
    )
  )

(setq org-image-actual-width nil)
(pixel-scroll-mode t) ;; enable pixel scroll mode for better image viewing

(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/org/journal/"
	org-journal-date-format "%A, %d %B %Y"
	org-journal-time-format "日记"))

(use-package org-download
  :ensure t
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  ;; (org-image-actual-width 500)
  ;; (org-download-screenshot-method "/usr/local/bin/pngpaste %s")
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")    
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(use-package valign
  :ensure t
  :after org
  ;; :config   (add-hook 'org-mode-hook #'valign-mode)
  )

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

;; does not work
(use-package org
  :ensure t
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "DOING" "DONE")))
  (setq org-todo-keyword-faces
	'(("TODO" . "red") ("DOING" . "cyan") ("DONE" . "green")))
  )

;; (use-package oc-bibtex
;;   :ensure t)

(save-place-mode 1)

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

(use-package dired-subtree
  :ensure t)

(use-package dired-filter
  :ensure t)

(setq dired-omit-files
      (concat dired-omit-files "\\|^\\.ipynb_checkpoints$\\|^\\.pytest_cache$\\|^\\.venv$\\|^\\.git$\\|^\\_\\_pycache\\_\\_$"))

(use-package neotree
  :ensure t
  :init
  (setq neo-window-width 30)
  )

(defun my-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
	 ($inputStr
	  (if (use-region-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (let ($p0 $p1 $p2
		      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
		      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
	      (setq $p0 (point))
	      (skip-chars-backward $pathStops)
	      (setq $p1 (point))
	      (goto-char $p0)
	      (skip-chars-forward $pathStops)
	      (setq $p2 (point))
	      (goto-char $p0)
	      (buffer-substring-no-properties $p1 $p2))))
	 ($path
	  (replace-regexp-in-string
	   "^file:///" "/"
	   (replace-regexp-in-string
	    ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
	(if (fboundp 'xahsite-url-to-filepath)
	    (let (($x (xahsite-url-to-filepath $path)))
	      (if (string-match "^http" $x )
		  (browse-url $x)
		(find-file $x)))
	  (progn (browse-url $path)))
      (progn ; not starting “http://”
	(if (string-match "#" $path )
	    (let (
		  ( $fpath (substring $path 0 (match-beginning 0)))
		  ( $fractPart (substring $path (1+ (match-beginning 0)))))
	      (if (file-exists-p $fpath)
		  (progn
		    (find-file $fpath)
		    (goto-char (point-min))
		    (search-forward $fractPart ))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		  (find-file $fpath))))
	  (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
	      (let (
		    ($fpath (match-string 1 $path))
		    ($line-num (string-to-number (match-string 2 $path))))
		(if (file-exists-p $fpath)
		    (progn
		      (find-file $fpath)
		      (goto-char (point-min))
		      (forward-line (1- $line-num)))
		  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		    (find-file $fpath))))
	    (if (file-exists-p $path)
		(progn ; open f.ts instead of f.js
		  (let (($ext (file-name-extension $path))
			($fnamecore (file-name-sans-extension $path)))
		    (if (and (string-equal $ext "js")
			     (file-exists-p (concat $fnamecore ".ts")))
			(find-file (concat $fnamecore ".ts"))
		      (find-file $path))))
	      (if (file-exists-p (concat $path ".el"))
		  (find-file (concat $path ".el"))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
		  (find-file $path ))))))))))

(global-set-key (kbd "C-c o f") 'my-open-file-at-cursor)

(use-package bookmark-view
  :ensure t
  )

;; (use-package switch-window

;;   :ensure t
;;   :init
;;   (global-set-key (kbd "C-x o") 'switch-window)
;;   (global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
;;   (global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
;;   (global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
;;   (global-set-key (kbd "C-x 0") 'switch-window-then-delete)

;;   (global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
;;   (global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
;;   (global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
;;   (global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

;;   (global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
;;   (global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

;;   (global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer))

;;   (setq switch-window-threshold 2)
;;   (setq switch-window-input-style 'minibuffer)
;;   (setq switch-window-shortcut-style 'qwerty)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-M-o") 'ace-window)
  ;; update aw-ignored-buffers ignore certain buffers
  )

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

;; (global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x b") 'consult-buffer)

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

(use-package avy
:ensure t
:bind
("M-s" . avy-goto-char-timer))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-M-j" . 'mc/mark-all-dwim)
  ("C-M-l" . 'mc/edit-lines)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C->" . 'mc/mark-next-like-this)
  ;; ("C-M->" . 'mc/skip-to-next-like-this)
  ;; ("C-M-<" . 'mc/skip-to-previous-like-this)
  )

(use-package goto-chg
  :ensure t
  :bind
  ("C-c C-g" . 'goto-last-change))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((python-mode . (lambda ()
                    (lsp-mode 1)
                    (flycheck-mode nil)  ;; does not turn off
                    ))
   (lsp-mode . lsp-enable-which-key-integration)
   )
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-flake8-enabled nil ;; use flake8
        lsp-pyls-plugins-flake8-ignore  nil
        lsp-diagnostics-provider nil  ;; do not use any provider, but flake8
        ) 
  ;; how to use the plugins?
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  )



;; clangd is used by default
(when (executable-find "clangd")
  (add-hook 'c++-mode-hook #'lsp))


;; do not use it, it is slow
;; (use-package lsp-ui
;;   :ensure t
;;   ;; :commands lsp-ui-mode
;;   )


;; (use-package ccls
;;   :ensure t
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;; 	 (lambda () (require 'ccls) (lsp))))

(use-package lsp-treemacs  
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(defun my/goto-treemacs ()
  "goto treemacs window, create one if it is not there"
  (window-list)
  ;; (print (get-buffer-window-list "Treemacs"))
  )

;; (my/goto-treemacs)

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
  :ensure t
  :hook
  (lsp-mode . (lambda () (flycheck-mode nil))) ;; turn it off in lsp-mode
  )

(use-package ein
  :ensure t
  :custom
  ;; use below to show inline images
  ;; reference: https://github.com/peterewills/emacs-ipython-notebook
  (ein:output-area-inlined-images t) ;; not necessary in older versions
  (ein:slice-image t)
  (pixel-scroll-mode t) ;; enable pixel scroll mode for better image viewing

  :bind
  ("C-c C-k C-c" . 'ein:worksheet-kill-cell)
  )

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq elpy-test-runner 'elpy-test-pytest-runner ; use pytest
	elpy-rpc-backend "jedi"
	;; elpy-rpc-project-specific 't
	;; elpy-modules (delq 'elpy-module-flymake elpy-modules)
	)
  ;; (add-hook 'elpy-mode-hook 'flycheck-mode)
  :bind
  ;; remap the keys for some navigation functions
  ("C-s-n" . 'elpy-nav-forward-block)
  ("C-s-p" . 'elpy-nav-backward-block)
  ("C-s-f" . 'elpy-nav-forward-indent)
  ("C-s-b" . 'elpy-nav-backward-indent)
  ("C-s-<left>" . 'elpy-nav-indent-shift-left)
  ("C-s-<right>" . 'elpy-nav-indent-shift-right)
  ("C-c b" . elpy-black-fix-code)
  )

(use-package cython-mode
  :ensure t)

(setq-default indent-tabs-mode nil)  ; indentation

(defun my/turn-on-python-profiling ()
  (interactive)
  (replace-string "# @profile" "@profile")
  )


(defun my/turn-off-python-profiling ()
  (interactive)
  (replace-string "@profile" "# @profile")
  )

(use-package typescript-mode
  :ensure t
  )
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; if you use typescript-mode
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)  % use prettier instead
         )
  :init (setq typescript-indent-level
              (or (plist-get (tide-tsfmt-options) ':indentSize) 2 ))
  )



;; ;; if you use treesitter based typescript-ts-mode (emacs 29+)
;; (use-package tide
;;   :ensure t
;;   :after (company flycheck)
;;   :hook ((typescript-ts-mode . tide-setup)
;;          (tsx-ts-mode . tide-setup)
;;          (typescript-ts-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

(use-package prettier-js
  :ensure t
  )

(add-hook 'typescript-mode-hook 'prettier-js-mode)

(with-eval-after-load 'treemacs
  (defun treemacs-ignore-c++-object-files (file _)
    (s-suffix? ".o" file))
  (push #'treemacs-ignore-c++-object-files treemacs-ignored-file-predicates))

(add-hook
   'c++-mode-hook
    (lambda ()
      (local-set-key (kbd "C-c C-c") #'compile)))
;; (define-key c++-mode-map (kbd "C-c C-c") 'compile)

(defun my/treemacs-back-and-forth ()
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
      (aw-flip-window)
    (treemacs-select-window)))

(global-set-key (kbd "C-x m") 'my/treemacs-back-and-forth)

(when (and (eq system-type 'gnu/linux)
	   (file-exists-p "/home/xiaoh1/code/matlab-emacs-src"))
  (add-to-list 'load-path "/home/xiaoh1/code/matlab-emacs-src")
  (load-library "matlab-load"))

(use-package sqlformat
  :ensure t
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

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

(use-package origami
  :ensure t
  :init (global-origami-mode))

  ; (global-set-key (kbd "C-c c t") 'origami-toggle-node)
  (global-set-key (kbd "C-c o t") 'origami-toggle-node)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets"
	  "~/.emacs.d/elpa/yasnippet-snippets-20230220.1659/snippets/"
	  "~/.emacs.d/elpa/yasnippet-snippets-20230227.1504/snippets"
	  ))
  ;; "~/.emacs.d/elpa/elpy-20220220.2059/"  ; might need to change
  ;; "~/.emacs.d/elpa/yasnippet-snippets-20220221.1234/snippets"  ; might need to change
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
:ensure t
)

(defun my/yas-add-acronyms (mode-sym acronyms)
  "add 'acronyms' to yasnippets for a given mode, e.g., 'org-mode
acronyms is a list of (list acronym full-name)
"
  (dolist (acr acronyms)
    (yas--define mode-sym (car acr) (car (cdr acr)))
    )
  )

(add-hook 'org-mode-hook #'(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

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

;; (setq-default TeX-master "main_kais") ; all master files called "main".
;; (setq-default TeX-master "reviews_cover_letter.tex")
;; (setq-default TeX-master "sn-article") ; all master files called "sn-article".
;; (setq-default TeX-master "cover") ; all master files called "cover".
(setq-default TeX-master "algs") ; all master files called "main".
;; (setq-default TeX-master "cv") ; all master files called "main".
(setq-default TeX-engine 'default)

;; (add-hook latex-mode-hook
;; 	  (lambda()
;; 	    (local-unset-key (kbd "C-M-a"))))

(add-hook 'LaTex-mode-hook
	  (lambda()
	    (define-key LaTex-mode-map (kbd "C-M-a") nil)))

(defun wrap-by-href ()
  "wrap a text by by \\href"
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert (concat "\\href{}{")))
  (save-excursion
    (goto-char (region-end))
    (insert "}"))
  (goto-char (+ (region-beginning) 6)) ; go to the first {} to insert the link
  )


;; how to define the key only for latex mode?
;; tried to the following
;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda () (local-set-key (kbd "C-c h r") #'wrap-by-href)))
;; (eval-after-load 'latex
;;   '(define-key LaTeX-mode-map [(kbd "C-c h r")] 'wrap-by-href))
(global-set-key (kbd "C-c h r") 'wrap-by-href)

(use-package citar
  :ensure t
  :after oc
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)

  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)


  ;; :general
  ;; (:keymaps 'org-mode-map
  ;; 	    :prefix "C-c b"
  ;; 	    "b" '(citar-insert-citation :wk "Insert citation")
  ;; 	    "r" '(citar-insert-reference :wk "Insert reference")
  ;; 	    "o" '(citar-open-notes :wk "Open note"))
  )

(defun my/open-defines-tex ()
  "open defines.tex under current directory"
  (interactive)
  (find-file "./defines.tex"))
(global-set-key (kbd "C-c o d") 'my/open-defines-tex)

(defun forward-jump-into-next-brace ()
  (interactive)
  (search-forward "{")
  )
(defun backward-jump-into-next-brace ()
  (interactive)
  (search-backward "{")
  )

(defun forward-jump-into-next-pair ()
  "however, for $, we may jump into the closing pair, how to fix it?"
  (interactive)
  (search-forward-regexp "[{\$(]")
  )
(defun backward-jump-into-next-pair ()
  (interactive)
  (search-backward-regexp "[{\$(]")
  )


;; (global-set-key (kbd "C-<tab>") #'forward-jump-into-next-brace)
;; (global-set-key (kbd "C-S-<tab>") #'backward-jump-into-next-brace)
(global-set-key (kbd "C-<tab>") #'forward-jump-into-next-pair)
(global-set-key (kbd "C-S-<tab>") #'backward-jump-into-next-pair)
;; (add-hook 'LaTex-mode-hook (lambda () (
;; 				       (local-set-key (kbd "C-<tab>") #'forward-jump-into-next-brace)
;; 				  )))

(use-package yaml-mode
:ensure t
:config
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 '(markdown-command "/usr/local/bin/pandoc"))

(use-package dockerfile-mode
  :ensure t)

;; (use-package multi-term
;;   :ensure t
;;   :config (setq multi-term-program "/bin/zsh")
;;   :bind ("C-c m t" . 'multi-term)
;;   )



(global-set-key (kbd "C-c s h")  'shell)

(defvar my-term-shell "/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; (quelpa '(popon
;; 	  :fetcher git
;; 	  :url "https://codeberg.org/akib/emacs-popon.git"))



;; (quelpa '(corfu-terminal
;; 	  :fetcher git
;; 	  :url "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package ssh
  :ensure t)

(defun config-visit ()
"visit ~/.emacs.d/config.org"
(interactive)
(find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c o e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
  (org-babel-load-file (expand-file-name "~/.emacs.d/unity.org"))
  )
;; (global-set-key (kbd "C-c r") 'config-reload)

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

;; (use-package cl-lib
;;   :ensure t)

(require 'cl-lib)

(defvar punctuation-marks '(","
			    "."
			    "'"
			    "&"
			    "\"")
  "List of Punctuation Marks that you want to count.")

(defun count-raw-word-list (raw-word-list)
  (cl-loop with result = nil
	   for elt in raw-word-list
	   do (cl-incf (cdr (or (assoc elt result)
				(car (push (cons elt 0) result)))))
	   finally return (sort result
				(lambda (a b) (string< (car a) (car b))))))

(defun word-stats ()
  (interactive)
  (let* ((words (split-string
		 (downcase (buffer-string))
		 (format "[ %s\f\t\n\r\v]+"
			 (mapconcat #'identity punctuation-marks ""))
		 t))
	 (punctuation-marks (cl-remove-if-not
			     (lambda (elt) (member elt punctuation-marks))
			     (split-string (buffer-string) "" t )))
	 (raw-word-list (append punctuation-marks words))
	 (word-list (count-raw-word-list raw-word-list)))
    (with-current-buffer (get-buffer-create "*word-statistics*")
      (erase-buffer)
      (insert "| word | occurences |
	       |-----------+------------|\n")

      (dolist (elt word-list)
	(insert (format "| '%s' | %d |\n" (car elt) (cdr elt))))

      (org-mode)
      (indent-region (point-min) (point-max))
      (goto-char 100)
      (org-cycle)
      (goto-char 79)
      (org-table-sort-lines nil ?N)))
  (pop-to-buffer "*word-statistics*"))

(use-package dmenu
    :ensure t
    :bind
    ("C-c d m" . 'dmenu))

(use-package edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
	      (edit-server-start)
	    (add-hook 'after-init-hook
		      #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
		'((name . "Edit with Emacs FRAME")
		  (top . 200)
		  (left . 200)
		  (width . 80)
		  (height . 25)
		  (minibuffer . t)
		  (menu-bar-lines . t)
		  ;; comment out the following due to "No applicable method: frame-creation-function"
		  ;; reference: https://emacs-china.org/t/edit-server-no-applicable-method-frame-creation-function/17562
		  ;; (window-system . x)
		  )))

(use-package good-scroll
  :ensure t
  :config (good-scroll-mode)
  )

(defun refrained-backward-word ()
  "similar to backward-word but does not move to the previous word if the cursor is at the begining of the word"
  (unless (member  ;; check if the previous point is left paren or space, or newline
	   (char-to-string (char-after (1- (point))))
	   '("(" " " "\n" "-"))
    (backward-word))
  )

(defun refrained-backward-sexp ()
  "similar to backward-sexp but does not move to the previous sexp if the cursor is at the begining of the sexp"
  (unless (member  ;; check if the previous point is left paren or space, or newline
	   (char-to-string (char-after (1- (point))))
	   '("(" " " "\n"))
    (backward-sexp))
  )

(defun search-backward-no-move (str)
  "search backward for a string without moving the cursor, return the position of the first occurrence"
  (save-excursion (search-backward str))
  )

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; (global-set-key (kbd "C-c w l") 'avy-copy-line)  ; copy a line
(global-set-key (kbd "C-c w r") 'avy-copy-region)  ; copy a region
;; (global-set-key (kbd "C-c d l") 'avy-kill-whole-line)  ; kill&save a line
(global-set-key (kbd "C-c d r") 'avy-kill-region)  ; kill&save a region

(global-subword-mode 1)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    ;; (?\' . ?\')  ;
			    (?\" . ?\")
			    ;; (?\` . ?\`)
			    ;; (?\$ . ?\$)
))
(electric-pair-mode nil)

(defvar latex-electric-pairs '(;; (?= . ?=)
			       (?$ . ?$)) "Electric pairs for latex.")

(defun add-latex-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs latex-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'add-latex-electric-pairs)

(add-hook 'latex-mode-hook 'add-latex-electric-pairs) ; does not work, very weird

;; (use-package company
;;   :hook
;;   (LaTex-mode . add-latex-electric-pairs)
;;   )

(defvar my/path-delimiters "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\" "characters that delimit a path")

(defun kill-word-at-point ()
  "kill the current word"
  (interactive)
  (refrained-backward-word)
  (kill-word 1)
  )

(defun kill-sexp-at-point ()
  "kill the current sexp"
  (interactive)
  (refrained-backward-sexp)
  (kill-sexp 1)
  )


(defun kill-line-at-point ()
  "kill the current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-whole-line)  ;; kill-line does not kill the \n
  (previous-line)
  )

(defun kill-path-at-point ()
  "kill path at point"
  (interactive)
  (let (beg end)        
    (save-excursion
      (skip-chars-backward my/path-delimiters)
      (setq beg (point))
      (skip-chars-forward my/path-delimiters)
      (setq end (point))
      (kill-region beg end))
    )
  )

;; to override major-mode keybindings (e.g., C-c C-k in org-mode is used)
(bind-keys*
 ("C-c d w" . kill-word-at-point)
 ("C-c d l" . kill-line-at-point)
 ("C-c d s" . kill-sexp-at-point)
 ("C-c d p" . kill-path-at-point)
 )

(defun copy-word (&optional arg)
    "copy a word at point into kill-ring"
    (interactive "p")
    (save-excursion
      ;; to the begining of the sexp if needed
      (refrained-backward-word)
      (mark-word)  ;; mark the sexp
      (kill-ring-save (region-beginning) (region-end))
      (message (format "copied %s"(car kill-ring)))
      )
    )
(global-set-key (kbd "C-c w w") 'copy-word)

;; (defun get-point (symbol &optional arg)
;;   "get the point"
;;   (funcall symbol arg)
;;   (point))

;; (defun copy-thing (begin-of-thing end-of-thing &optional arg)
;;   "Copy thing between beg & end into kill ring."
;;   (save-excursion
;;     (let ((beg (get-point begin-of-thing 1))
;; 	  (end (get-point end-of-thing arg)))
;;       (copy-region-as-kill beg end))))

;; (defun my-copy-word (&optional arg)
;;   "Copy words at point into kill-ring"
;;   (interactive "P")
;;   (copy-thing 'backward-word 'forward-word arg)
;;   (message (format "copied %s"(car kill-ring)))
;;   )

;; (global-set-key (kbd "C-c w w") 'my-copy-word)

(defun copy-sexp (&optional arg)
  "copy an sexp at point into kill-ring"
  (interactive "p")
  (save-excursion
    ;; to the begining of the sexp if needed
    (refrained-backward-sexp)
    (mark-sexp)  ;; mark the sexp
    (kill-ring-save (region-beginning) (region-end))
    (message (format "copied %s"(car kill-ring)))
    )
  )
(global-set-key (kbd "C-c w s") 'copy-sexp)

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol))))
  (message "a line is copied")
  )
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(use-package hungry-delete
:ensure t
:config (global-hungry-delete-mode)
:bind
("C-c h d f" . hungry-delete-forward)
("C-c h d b" . hungry-delete-backward))

;; (global-set-key (kbd "C-c d p") 'delete-pair)

(defun path-at-point ()
  "return a path at point."
  (let (beg end)
    (save-excursion
      (skip-chars-backward my/path-delimiters)
      (setq beg (point))
      (skip-chars-forward my/path-delimiters)
      (setq end (point))
      (buffer-substring-no-properties beg end)
      ))
  )

(defun copy-path-at-point ()
  "copy a path at point."
  (interactive)
  (let ((str (path-at-point)))
    (with-temp-buffer
      (insert str)
      (clipboard-kill-region (point-min) (point-max)))
    (message (format "copied '%s'" str))
    )
  )

(global-set-key (kbd "C-c w p") 'copy-path-at-point)

(defun close-string (open-str)
  "given an open string (, return the close string, such as )"
  (cond
   ((string= open-str "(") ")")
   ((string= open-str "[") "]")
   ((string= open-str "<") ">")
   ((string= open-str "{") "}")
   (t open-str)
   )
  )

;; Instead of using region-beginning and region-end, a command designed to operate on a region should normally use interactive with the ‘r’ specification to find the beginning and end of the region. 
(defun my/surround-region (start end open-str)
  (save-excursion
    (goto-char end)
    (insert (close-string open-str))
    (goto-char start)
    (insert open-str)
    )
  )


(defun my/surround-sexp (open-str)
  "surround a sexp by str"
  (save-excursion
    (refrained-backward-sexp)
    (insert open-str)
    (forward-sexp)
    (insert (close-string open-str))
    )
  )

(defun my/surround-by-single-quote (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "'")
    (my/surround-sexp "'"))
  )

(defun my/surround-by-double-quote (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "\"")
    (my/surround-sexp "\""))
  )

(defun my/surround-by-back-tick (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "`")
    (my/surround-sexp "`"))
  )
(defun my/surround-by-dollar (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "$")
    (my/surround-sexp "$"))    
  )

(defun my/surround-by-parenthesis (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "(")
    (my/surround-sexp "("))
  )


(defun my/surround-by-brace (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "{")
    (my/surround-sexp "{"))
  )

(defun my/surround-by-bracket (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "[")
    (my/surround-sexp "["))
  )

(defun my/surround-by-asterisk (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "*")
    (my/surround-sexp "*"))
  )    

(defun my/surround-by-plus (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "+")
    (my/surround-sexp "+"))
  )  

(defun my/surround-by-slash (beg end)
  (interactive "r")
  (if (use-region-p)
      (my/surround-region beg end "/")
    (my/surround-sexp "/"))
  )

(global-set-key (kbd "C-c s '") 'my/surround-by-single-quote)
(global-set-key (kbd "C-c s \"") 'my/surround-by-double-quote)
(global-set-key (kbd "C-c s $") 'my/surround-by-dollar)
(global-set-key (kbd "C-c s `") 'my/surround-by-back-tick)
(global-set-key (kbd "C-c s (") 'my/surround-by-parenthesis)
(global-set-key (kbd "C-c s [") 'my/surround-by-bracket)
(global-set-key (kbd "C-c s {") 'my/surround-by-brace)
(global-set-key (kbd "C-c s *") 'my/surround-by-asterisk)
(global-set-key (kbd "C-c s +") 'my/surround-by-plus)
(global-set-key (kbd "C-c s /") 'my/surround-by-slash)

(defun my/surround-path-by-string (str)
  "surround a path-like string by another string"
  (let*  ((open-str str)
	  (close-str (close-string open-str))
	  )
    (save-excursion
      (skip-chars-backward my/path-delimiters)
      (insert open-str)
      (skip-chars-forward my/path-delimiters)
      (insert close-str)
      )
    )
  )

(defun my/py-insert-callable (beg end)
  "prepends a Python callable (e.g., function or method) to a string (e.g., representing an argument, e.g., `args' -> `func(args)'"
  (interactive "r")
  (let ((py-callable (read-string "Which callable:")))
    (save-excursion
      (my/surround-by-parenthesis beg end)
      (unless (string= (char-to-string (char-after)) "(") ; if we are not at the begining of the the chunk
	(search-backward "(")); search backward to the point to insert the prefix
      (insert py-callable)
      )
    )
  )

;; enable the following keybinding only in Python
(use-package elpy
  :bind ("C-c s f" . 'my/py-insert-callable))

(defun delete-in-between (open)
  "delete the text between a pair of symbols (e.g., `(' and `)'), \
     the first element of the pair is speicifed by `open', \
     while the second is inferred automatically using `close-str'"
  (let ((close (close-string open)))
    (save-excursion
      (delete-region
       (+ (search-backward-no-move open) (length open)) ; leave the open and close string there
       (- (search-forward close) (length close))
       )
      )
    )
  )


(defun my/delete-between-single-quote  ()
  (interactive)
  (delete-in-between "'")
  )
(defun my/delete-between-double-quote  ()
  (interactive)
  (delete-in-between "\"")
  )
(defun my/delete-between-parenthesis  ()
  (interactive)
  (delete-in-between "(")
  )
(defun my/delete-between-bracket  ()
  (interactive)
  (delete-in-between "[")
  )
(defun my/delete-between-brace  ()
  (interactive)
  (delete-in-between "{")
  )
(defun my/delete-between-dollar  ()
  (interactive)
  (delete-in-between "$")
  )    

(defun my/delete-between-equal  ()
  (interactive)
  (delete-in-between "=")
  )

(global-set-key (kbd "C-c d '") 'my/delete-between-single-quote)
(global-set-key (kbd "C-c d \"") 'my/delete-between-double-quote)
(global-set-key (kbd "C-c d (") 'my/delete-between-parenthesis)
(global-set-key (kbd "C-c d [") 'my/delete-between-bracket)
(global-set-key (kbd "C-c d {") 'my/delete-between-brace)
(global-set-key (kbd "C-c d $") 'my/delete-between-dollar)
(global-set-key (kbd "C-c d =") 'my/delete-between-equal)

(use-package smartparens-config
  :ensure smartparens
  :config
  (progn (show-smartparens-global-mode t))
  :hook
  (LaTeX-mode . turn-on-smartparens-mode)
  )

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
;; (add-hook 'LaTex-mode-hook 'turn-on-smartparens-mode)
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; (global-set-key (kbd "C-M-a") 'sp-beginning-of-sexp)
;; (global-set-key (kbd "C-M-e") 'sp-end-of-sexp)

;; (global-set-key (kbd "C-down") 'sp-down-sexp)

(bind-keys
 :map smartparens-mode-map
 ("M-a" . sp-beginning-of-sexp)
 ("M-e" . sp-end-of-sexp)

 ;; hierarchy-level movement
 ("C-<down>" . sp-down-sexp)
					; it might be clearer if the name is sp-forward-down-sexp, which forward + down on paren
					; and the cursor stops at the begining of the target sexp

 ("C-<up>" . sp-up-sexp)
					; = forward and up, which jumps out of the current paren

 ("M-<down>" . sp-backward-down-sexp)
					; = backward and "jump in" on paren the reverse of sp-down-sexp
					; the cursor stops at the beginning of the target
 ("M-<up>" . sp-backward-up-sexp)
					; = backward and "jump out"

 ;; balanced expression-level movement, like normal "C-M-f" and "C-M-b"
 ("C-M-f" . sp-forward-sexp)
					; forward one sexp (including comments) to the end of the target
 ("C-M-b" . sp-backward-sexp)
					; backward one sexp to the begining of the target

 ;; sexp-level movement
 ;; keybinding is set to mimic org-mode's C-c C-[n|p]
 ("C-M-n" . sp-next-sexp)
					; move to the begining of the next sexp, which is equals to sp-down-sexp + sp-up-sexp + sp-down-sexp
 ("C-M-p" . sp-previous-sexp)           ;

 ;;free-form movement, ignoring the hierarchy
 ("C-S-f" .  sp-forward-symbol)

 ("C-S-b" .  sp-backward-symbol))

(let ((x "asdf asdf asdfads ")))

  (defun format-date (format)
    "Insert date with FORMAT specification using a specific locale."
    (let (
	  (system-time-locale "en_US.UTF-8"))
      (insert (format-time-string format))))

  "asf"

  (defun blah ()
    "Returns blah of foo."
    (asf)
    (asdf))
  (defun blah () (let ((x 0)) (+ x 1)))


  (defun asd ()
    (insert (a very long and long and long and long list
) (asdf asdf ads )))

  "asdfasdfads sadf asdf asdf ads"

(defun wrap-with-parens (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "(")
  )	    

(defun wrap-with-brackets (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "[")
  )	    

(defun wrap-with-braces (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "{")
  )	    

(defun wrap-with-single-quotes (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "'")
  )

(defun wrap-with-double-quotes (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "\"")
  )

(defun wrap-with-back-quotes (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "`")
  )

(defun wrap-with-underscores (&optional arg)
  (interactive "p")
  (sp-wrap-with-pair "_")
  )

(bind-keys
 :map smartparens-mode-map
 ("C-c s ("  . wrap-with-parens)
 ("C-c s ["  . wrap-with-brackets)
 ("C-c s {"  . wrap-with-braces)
 ;; ("C-c s '"  . wrap-with-single-quotes) ; do not collide with C-' in yasnippet-mode
 ("C-c s \"" . wrap-with-double-quotes)
 ("C-c s _"  . wrap-with-underscores)
 ;; ("C-c s `"  . wrap-with-back-quotes) ; do not collide with C-` in latex-mode
 )

(bind-keys
 :map smartparens-mode-map
 ("M-[" . sp-backward-unwrap-sexp)
					; unwrap the previous expression
 ("M-]" . sp-unwrap-sexp))
					; unwrap the following expression


;; (foo (bar x y z))

(bind-keys
   :map smartparens-mode-map
   ("C-<right>" . sp-forward-slurp-sexp)
					  ; takes in the next sexp
   ("C-<left>" . sp-backward-slurp-sexp)
					; takes in the previous sexp

   ("M-<right>" . sp-forward-barf-sexp)
					  ; takes out the next sexp
   ("M-<left>" . sp-backward-barf-sexp)
					  ; takes out the previous sexp   
   )

;; [foo bar "baz"]
;; [foo bar] baz

(bind-keys
   :map smartparens-mode-map
   ("C-M-t" . sp-transpose-sexp)
   )
;; (a) (b (c))

(bind-keys
   :map smartparens-mode-map
   ("C-M-k" . sp-kill-sexp)
   )

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
	 (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(global-set-key (kbd "C-c w f") 'copy-current-line-position-to-clipboard)

(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l))
)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(use-package avy
  :ensure t
  :config
  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
	(alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  )

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
	(bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(use-package avy
  :config
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
	(alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
	(alist-get ?w avy-dispatch-alist) 'avy-action-copy
	(alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
	)
  )

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(use-package avy
  :config
  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
	(alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
  )

(use-package avy
  :config
  (setf (alist-get ?z avy-dispatch-alist) 'avy-action-zap-to-char)
  )

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(use-package avy
  :config
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
  )

(use-package helpful
  :ensure t
  :bind
  ("C-c h f" . helpful-at-point)
  )

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(use-package avy
  :config
  (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

  )

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  )

;; use citar-embard to enable using citation-key as target
(use-package citar-embark
  :ensure t
  :after citar embark
  ;; :config (citar-embark-mode)
  )

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
	(goto-char pt)
	(embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

;; (use-package swiper
;;   :ensure t
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-r" . swiper-backward)
;;   )

(use-package consult
  :ensure t
  :bind
  ("C-c f" . 'consult-find)  ;; find file
  ("C-c i" . 'consult-imenu) ;;  find functions, classes, etc in Python script, or headings in org
					; consult-imenu-multi for multiple buffers
  ("C-s" . 'consult-line)  ;; line search in current buffer
  ("C-c s g" . 'consult-git-grep) ;; search in git-tracked files
  ("C-c y" . 'consult-yank-from-kill-ring)
  ("C-c r s" . 'consult-register-store)
  ("C-c r l" . 'consult-register)
  ("C-c r l" . 'consult-register)
  ("C-c m" . 'consult-mark)
  ("M-g M-g" . 'consult-goto-line)
  ("C-c c e" . 'consult-compile-error)
  )

(defun consult-line-other-window (prefix)
    "Function to consult-line in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
	(let ((next (if prefix -1 1)))
	  (other-window next)
	  (consult-line)
	  (other-window (- next))))))

;; (defun consult-backward-other-window (prefix)
;;   "Function to consult-backward in other-window."
;;   (interactive "P")
;;   (unless (one-window-p)
;;     (save-excursion
;;       (let ((next (if prefix 1 -1)))
;; 	(other-window next)
;; 	(consult-line)
;; 	(other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'consult-line-other-window)
;; (define-key global-map (kbd "C-M-r") 'consult-backward-other-window)

(use-package magit
  :ensure t)

(use-package simple-mpc
  :ensure t)

;; (use-package ido
;;   :ensure t
;;   :config
;;   (setq ido-enable-flex-matching nil
;; 	ido-create-new-buffer 'always
;; 	ido-everywhere t)
;;   (add-to-list 'ido-ignore-files "\.bak")
;;   (add-to-list 'ido-ignore-files "\.log")
;;   (add-to-list 'ido-ignore-files ".venv")
;;   (add-to-list 'ido-ignore-files "__pycache__")
;;   (add-to-list 'ido-ignore-files "\.pytest_cache")
;;   (add-to-list 'ido-ignore-files "\.pkl")
;;   ;; data files
;;   (add-to-list 'ido-ignore-files "\.hdf5")
;;   ;; latex-related
;;   (add-to-list 'ido-ignore-files "\.nav")
;;   (add-to-list 'ido-ignore-files "\.out")
;;   (add-to-list 'ido-ignore-files "\.pdf")
;;   (add-to-list 'ido-ignore-files "\.snm")
;;   (add-to-list 'ido-ignore-files "\.synctex.gz")
;;   ;; org
;;   (add-to-list 'ido-ignore-files "\.org_archive")
;;   (ido-mode 1)
;;   )

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :requires ido
;;   :config
;;   (ido-vertical-mode 1)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;   )

;; (use-package smex
;;   :ensure t
;;   :init (smex-initialize)
;;   :bind
;;   ("M-x" . smex))
