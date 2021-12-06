(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;:ensure t for all use-packages

(add-to-list 'custom-theme-load-path "/home/andy/.emacs.d/elpa")
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

(scroll-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'maximized)

(use-package abyss-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t)

(if (display-graphic-p)
  (load-theme 'abyss)
(load-theme 'spacemacs-dark))

;; Ask user a "y or n" question.
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package elpy
  :ensure t
  :init
  (setq py-python-command "python3")
  (setq elpy-test-runner 'elpy-test-pytest-runner)
	;;elpy-test-pytest-runner-command '("python" "-m" "pytest"))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
;  (setq elpy-shell-starting-directory "current-directory")
  (with-eval-after-load 'python
    (elpy-enable)))

(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))

(setq sql-indent-offset 2)
;;Load database connection settings
(eval-after-load "sql"
  '(load-if-exists "~/.sql.el"))

(use-package web-mode
    :ensure t
    :config
    (setq tab-width 2)
    (setq web-mode-enable-current-column-highlight 1)
    (setq web-mode-enable-current-element-highlight 1)
    :mode ("\\.html?\\'"
	   "\\.css\\'"
	   "\\.phtml\\'"
	   "\\.php\\'"
	   "\\.inc\\'"
	   "\\.tpl\\'"
	   "\\.jsp\\'"
	   "\\.as[cp]x\\'"
	   "\\.erb\\'"
	   "\\.mustache\\'"
	   "\\.djhtml\\'"
	   "\\.jsx\\'"
	   "\\.tsx\\'"))

  (defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  )

(add-hook 'web-mode-hook 'my-web-mode-hook)

(use-package emmet-mode
  :ensure t
  :hook
  (web-mode)
  :init
  (add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
	    (web-mode-language-at-pos)))
	       (if (string= web-mode-cur-language "php")
	   (yas-activate-extra-mode 'php-mode)
	 (yas-deactivate-extra-mode 'php-mode))
	       (if (string= web-mode-cur-language "css")
	   (setq emmet-use-css-transform t)
	 (setq emmet-use-css-transform nil)))))
  )

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :config
  (setq js2-indent-level 2
	js2-strict-missing-semi-warning nil)
  :custom
  (js2r-prefer-let-over-var t)
  (js-indent-align-list-continuation t))

(use-package company-web
  :ensure t
  :defer t)

(use-package company
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-minimum-prefix-length 2))

(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (web-mode . yas-minor-mode))

(use-package hideshow
  :ensure t
  :hook
  (python-mode . hs-minor-mode)
  :config
  (setq hs-isearch-open 'code)
  )

(use-package magit
  :ensure t
  :bind (("C-x C-g s" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package org
    :ensure t
    :config
    (setq org-hide-emphasis-markers t)
    (setq org-ellipsis " ▾")
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-image-actual-width 700)
    (setq org-refile-targets
	  '(("archive.org" :maxlevel . 1)))
    ;; Save org buffers after refiling
    (advice-add 'org-refile :after 'org-save-all-org-buffers)
    (setq org-archive-location "%s_archive::")
    :bind
    ("C-c a" . 'org-agenda)
    ;;:hook
    ;;(org-mode 'turn-on-auto-fill)
    )
;;control size of headings
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.15)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))
  (add-hook 'org-mode-hook 'auto-fill-mode)

(require 'org-tempo)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun a/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
		      (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'a/org-babel-tangle-config)))

(defun org-focus-private() "Set focus on private things."
       (interactive)
       (setq org-agenda-files '("~/Orgy/me.org")))
(defun org-focus-work() "Set focus on work things."
       (interactive)
       (setq org-agenda-files '("~/Orgy/limo.org")))
(defun org-focus-all() "Set focus on all things."
       (interactive)
       (setq org-agenda-files '("~/Orgy/limo.org"
				"~/Orgy/me.org")))

(org-focus-all)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-directory "~/Orgy/Notes")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "programmer" plain
      "\n* Personal data\n- Name: ${title}\n- Email: %^{Email}\n- Work: %^{Work}\n- Country: %^{Country}\n- Topics of interest\n> %?\n* Social\n\n* Publications\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n g" . org-roam-graph)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (dolist (dir '(
		 "[/\\\\]\\venv$"
		 "[/\\\\]\\.git$"
		 "[/\\\\]__pycache__$"
		 ))
    (push dir lsp-file-watch-ignored))
  (lsp-enable-which-key-integration t))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  ;;:diminish
  :init (ivy-mode)
  :bind
  ("C-s" . swiper)
  :config
  (setq ivy-wrap t)
  (setq ivy-use-virtual-buffers t)
  (setf (alist-get 'swiper ivy-height-alist) 10)
  (setf (alist-get 'ivy-switch-buffer ivy-height-alist) 7))

(use-package ivy-rich
  :init (ivy-rich-mode))

(use-package counsel
  :config (counsel-mode)
  :bind
  (:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :ensure t
  :init
  (which-key-setup-side-window-right-bottom)
  :config
  (which-key-mode))

(use-package auto-dim-other-buffers
  :ensure t
  :init
  (auto-dim-other-buffers-mode))

(use-package multi-term
  :ensure t
  :bind (("C-x M" . multi-term)
	 ("C-x m" . switch-to-term-mode-buffer))
  :config
  (add-hook 'term-mode-hook
	    (lambda ()
	      (dolist
		  (bind '(("<S-down>" . multi-term)
			  ("<S-left>" . multi-term-prev)
			  ("<S-right>" . multi-term-next)
			  ("C-<backspace>" . term-send-backward-kill-word)
			  ("C-<delete>" . term-send-forward-kill-word)
			  ("C-<left>" . term-send-backward-word)
			  ("C-<right>" . term-send-forward-word)
			  ("C-c C-j" . term-line-mode)
			  ("C-c C-k" . term-char-mode)
			  ("C-v" . scroll-up)
			  ("C-y" . term-paste)
			  ("C-z" . term-stop-subjob)
			  ("M-DEL" . term-send-backward-kill-word)
			  ("M-d" . term-send-forward-kill-word)))
		(add-to-list 'term-bind-key-alist bind)))))

(setq erc-server "irc.libera.chat"
      erc-nick "andy---"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

(fset 'insert-py-docstring
   (kmacro-lambda-form [?\' ?\' ?\' return return tab ?A ?r ?g ?u ?m ?e ?n ?t ?s ?: return tab ?\C-u ?1 ?3 ?- return return tab ?R ?e ?t ?u ?r ?n ?s ?: return tab ?\C-u ?1 ?3 ?- return return tab ?\' ?\' ?\' return ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p tab] 0 "%d"))
(global-set-key (kbd "C-c (") 'insert-py-docstring)

(append (list '("\\.c$" . c-mode)
	      '("\\.tex$" . latex-mode)
	      '("\\.S$" . S-mode)
	      '("\\.s$" . S-mode)
	      '("\\.R$" . R-mode)
	      '("\\.r$" . R-mode)
	      '("\\.html$" . html-mode)
	      '("\\.emacs" . emacs-lisp-mode)
	      )
	auto-mode-alist)

(use-package crux
  :ensure t
  :bind
  (("C-c x o" . crux-open-with)
   ("C-c x k" . crux-smart-kill-line)
   ("C-c x n" . crux-cleanup-buffer-or-region)
   ("C-x x t" . crux-transpose-windows)
   ("C-c x D" . crux-delete-file-and-buffer)
   ("C-c x r" . crux-rename-file-and-buffer)
   ("C-c x d" . crux-duplicate-and-comment-current-line-or-region)
   ("C-c x TAB" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c x I" . cux-indent-defun)
   ("C-c x i" . crux-find-user-init-file)
   ("C-c x j" . crux-top-join-line)))

(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
     (setq comint-scroll-to-bottom-on-output 'others)
     (setq comint-scroll-show-maximum-output t)
     ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above promp
     (setq comint-scroll-to-bottom-on-input 'this)
     ))

(defun a/swap-windows ()
  "Swap your windows."
  (interactive)
  (cond ((not (> (count-windows)1))
	 (message "You can't rotate a single window!"))
	(t
	 (setq i 1)
	 (setq numWindows (count-windows))
	 (while (< i numWindows)
	   (let* ((w1 (elt (window-list) i))
		  (w2 (elt (window-list) (+ (% i numWindows) 1)))
		  (b1 (window-buffer w1))
		  (b2 (window-buffer w2))
		  (s1 (window-start w1))
		  (s2 (window-start w2)))
	     (set-window-buffer w1 b2)
	     (set-window-buffer w2 b1)
	     (set-window-start w1 s2)
	     (set-window-start w2 s1)
	     (setq i (1+ i)))))))

(defun a/rotate-windows ()
  "Rotate your windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun last-term-mode-buffer (list-of-buffers)
  "Returns the most recently used term-mode buffer."
  (when list-of-buffers
    (if (eq 'term-mode (with-current-buffer (car list-of-buffers) major-mode))
	(car list-of-buffers) (last-term-mode-buffer (cdr list-of-buffers)))))

;;Switch to the most recently used term-mode buffer, or create a new one.
(defun switch-to-term-mode-buffer ()
  "Switch to the most recently used term-mode buffer, or create a
new one."
  (interactive)
  (let ((buffer (last-term-mode-buffer (buffer-list))))
    (if (not buffer)
	(multi-term)
      (switch-to-buffer buffer))))

(defun load-if-exists (file)
  "Load `file` if it exists."
  (when (file-exists-p file)
    (load file)))

(global-set-key (kbd "C-c r") 'a/rotate-windows)
(global-set-key (kbd "C-c s") 'a/swap-windows)
