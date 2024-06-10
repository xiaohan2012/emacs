(require 'package)


;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; install 'use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;; (load "~/.emacs.d/config.el")

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; (when (file-exists-p "~/.emacs.d/unity.org")
;;   (org-babel-load-file (expand-file-name "~/.emacs.d/unity.org")))

(when (file-exists-p "~/.emacs.d/local.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/local.org")))

(when (file-exists-p "~/.emacs.d/upright.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/upright.org")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "88deeaaab5a631834e6f83e8359b571cfcbf5a18b04990288c8569cc16ee0798" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(hl-sexp-background-color "#121212")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2aa198")
     ("PROG" . "#268bd2")
     ("OKAY" . "#268bd2")
     ("DONT" . "#d70000")
     ("FAIL" . "#d70000")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#875f00")
     ("KLUDGE" . "#875f00")
     ("HACK" . "#875f00")
     ("TEMP" . "#875f00")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(markdown-command "/usr/local/bin/pandoc")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-agenda-files '("~/.emacs.d/config.org"))
 '(org-export-backends '(ascii beamer html latex md))
 '(org-fontify-done-headline nil)
 '(org-fontify-todo-headline nil)
 '(package-selected-packages
   '(org-colored-text minimap pyimpsort pyim pyimport good-scroll goto-chg c++-mode lsp-treemacs ccls lsp-ui lsp-mode oc-bibtex multiple-cursors consult marginalia orderless vertico citar-embark citar embark helpful tab-jump-out exec-path-from-shell valign multi-term org-download simple-mpc yasnippet-classic-snippets org-journal corfu-terminal popon quelpa corfu bookmark-view dockerfile-mode smartparens neotree dired-filter dired-subtree dired-hacks emojify magit markdown-mode afternoon-theme moe-theme zenburn-theme monokai-theme swiper sublimity-scroll sublimity symon dmenu diminish spaceline dashboard hungry-delete rainbow avy switch-window rainbow-delimiters org-bullets beacon spacemacs-theme which-key material-theme projectile ido-vertical-mode smex yasnippet-snippets yassnippet matlab-mode rainbow-mode auctex ein cython-mode yaml-mode flycheck-pycheckers jedi-direx jedi elpy use-package flycheck))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#262626"))
 '(projectile-globally-ignored-file-suffixes '("pkl" "zip"))
 '(projectile-globally-ignored-files nil)
 '(projectile-indexing-method 'hybrid)
 '(sql-connection-alist
   '(("upright"
      (sql-product 'postgres)
      (sql-user "hanxiao")
      (sql-database "localuprightdb")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column-indicator ((t (:foreground "#CCCCCC" :weight normal))))
 '(multi-magit-repo-heading ((t (:inherit magit-section-heading :box nil))))
 '(speedbar-selected-face ((t (:foreground "#119911" :underline t)))))

