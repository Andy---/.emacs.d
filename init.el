(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa")
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

(defun load-if-exists (file)
  "Load `file` if it exists."
  (when (file-exists-p file)
    (load file)))

(scroll-bar-mode -1)

;; Ask user a "y or n" question.
(defalias 'yes-or-no-p 'y-or-n-p)

;;rainbow-delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;org-mode
(use-package org
  :ensure t
  :config
  (setq org-hide-emphasis-markers t)
  (setq	org-ellipsis " ▾")
  (setq org-log-done 'time)
  (setq org-image-actual-width 700)
  (setq org-refile-targets
	'(("archive.org" :maxlevel . 1)))
  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (setq org-archive-location "%s_archive::")
  :bind
  ("C-c a" . 'org-agenda))

;;org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;control size of headings
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.15)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.0)))
  (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

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

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;;Language Server
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
    (push dir lsp-file-watch-ignored)))
;;  (lsp-enable-which-key-integration t))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;;SQL
(setq sql-indent-offset 2)
;;Load database connection settings
(eval-after-load "sql"
    '(load-if-exists "~/.sql.el"))

;;octave mode
;;(autoload 'run-octave "octave-inf" nil t)
;;(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (turn-on-font-lock)
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input)))
;;(setq inferior-octave-prompt ">> ")

;;python
;;(require 'ein)
(use-package ein
  :ensure t
  :commands (ein:notebooklist-open))

;;Elpy
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
    ;;(delete 'elpy-module-highlight-indentation elpy-modules)
    (elpy-enable)))
;;(elpy-use-ipython)))

;;Auctex
;;(use-package auctex
;;  :ensure t)

;;Keybindings
(global-set-key (kbd "C-x f") 'find-file-at-point)
(add-hook 'LaTeX-mode-hook #'(lambda ()
			       (local-set-key "\C-cb" 'ebib-insert-bibtex-key)))

;;For Auctex < 11.82 exchange ";;" in the following 2 lines
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)
;(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t) ;; .pdf statt .dvi per default:
;;Zeilenumbruch
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;Syntax Higlight
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)
;; Mathe Modus
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; Reftex einflechten und laden
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Satzende ". " statt ". ". " f ̈r M-k: l ̈schen bis Satzende usw.
(setq sentence-end "[.?!][]\"’)}]*\\($\\| \\| \\)[
;;]*") ;; Da ist ein "Newline in der Zeile!"
(setq sentence-end-double-space nil)
;;direkte Rechtschreib Korrektur:
;;(add-hook ’LaTeX-mode-hook ’flyspell-mode)
;; Nur benutzen falls Auctex > 11.81 mit preview-latex:
;; aspell ist besser als ispell.
;; Zeile kommentieren, falls nicht installiert:
(setq-default ispell-program-name "aspell")
;; Deutsche Rechtschreibung falls \usepackage{ngerman}
;; oder german benutzt wird
(add-hook 'TeX-language-de-hook
(function (lambda () (ispell-change-dictionary "german8"))))

;;; Anpassungen für RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(add-hook 'latex-mode-hook 'turn-on-reftex) 
(setq reftex-plug-into-AUCTeX t)
(setq reftex-external-file-finders
      '(("tex" . "kpsewhich -format=.tex %f")
        ("bib" . "kpsewhich -format=.bib %f")))

;;ebib für BibTex
(use-package ebib
  :ensure t)

(autoload 'ebib "ebib" "Ebib, ab BibTex database manager." t)

;;keyboardmacros für Auctex
;;mathInline
(fset 'mathInline
   "\\(\\)\C-b\C-b")
(global-set-key (kbd "C-c m") 'mathInline)
;; mathDisplayed
(fset 'mathDisplayed
   "\\[\\]\C-b\C-b")
(global-set-key (kbd "C-c n") 'mathDisplayed)
;;mathrm
(fset 'mathrm
   "\\mathrm{}\C-b")
(global-set-key (kbd "C-c j") 'mathrm)

;;ivy
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

;;ivy-rich
(use-package ivy-rich
  :init (ivy-rich-mode))

;;counsel
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

;; ;; IDO-MODE
;; (ido-mode t)
;; (setq ido-auto-merge-work-directories-length nil
;;       ido-create-new-buffer 'always
;;       ido-enable-flex-matching t
;;       ido-enable-prefix nil
;;       ido-handle-duplicate-virtual-buffers 2
;;       ido-max-prospects 10
;;       ido-use-filename-at-point 'guess
;;       ido-use-virtual-buffers t)

;; ;;smex
;; (use-package smex
;;   :ensure t)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :init
;;   (ido-vertical-mode)
;;   (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;;multi-term
(use-package multi-term
  :ensure t
  :bind (("C-x M" . multi-term)
         ("C-x m" . switch-to-term-mode-buffer))
  :config
  ;; (setq multi-term-dedicated-select-after-open-p t
  ;;       multi-term-dedicated-window-height 25
  ;;       multi-term-program "/bin/bash")

  ;; ;; Enable compilation-shell-minor-mode in multi term.
  ;; ;; http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/

  ;; ;; TODO: WTF? Turns off colors in terminal.
  ;; ;; (add-hook 'term-mode-hook 'compilation-shell-minor-mode)
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

;;auto-dim-other-buffers
(use-package auto-dim-other-buffers
  :ensure t
  :init
  (auto-dim-other-buffers-mode))

;;Returns the most recently used term-mode buffer.
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

(defun swap-windows ()
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

(defun rotate-windows ()
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

(global-set-key (kbd "C-c r") 'rotate-windows)
(global-set-key (kbd "C-c s") 'swap-windows)

;;(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
;;(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

;;öffnet Emacs im Fullscreen
(set-frame-parameter nil 'fullscreen 'maximized)


;; ESS
(use-package ess
  :ensure t)

;;web-development
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

;;javascript
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

;;emmet
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

;;yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (web-mode . yas-minor-mode))

;;company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;company-web
(use-package company-web
  :ensure t
  :defer t)

;;hideshow
(use-package hideshow
  :ensure t
  :hook
  (python-mode . hs-minor-mode)
  :config
  (setq hs-isearch-open 'code)
  )


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
; automatically get the correct mode 

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
(setq ess-history-file nil)
(setq inferior-R-program-name "/usr/bin/R")
; comment out the following if you are not using R/S-Plus on ACPUB
; add a ";" in front of each line 
;(load "/usr/pkg/ess/lisp/ess-site")
;(setq-default inferior-S+6-program-name "Splus")
;(setq-default inferior-R-program-name "R")
 
;;Magit
(use-package magit
  :ensure t
  :bind (("C-x C-g s" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;(use-package spacemacs-theme
;;  :ensure t)
  
(use-package abyss-theme
  :ensure t)

;;detecting if emacs is in terminal- or gui-mode and selecting color-theme accordingly (s.http://emacs.stackexchange.com/questions/7151/is-there-a-way-to-detect-that-emacs-is-running-in-a-terminal)
(if (display-graphic-p)
    (load-theme 'abyss)
  (load-theme 'spacemacs-dark))

;;Soundklaus
(use-package soundklaus
  :ensure t
  :commands
  (soundklaus-activities
   soundklaus-connect
   soundklaus-my-favorites
   soundklaus-my-playlists
   soundklaus-my-tracks
   soundklaus-playlists
   soundklaus-tracks))

(fset 'insert-py-docstring
   (kmacro-lambda-form [?\' ?\' ?\' return return tab ?A ?r ?g ?u ?m ?e ?n ?t ?s ?: return tab ?\C-u ?1 ?3 ?- return return tab ?R ?e ?t ?u ?r ?n ?s ?: return tab ?\C-u ?1 ?3 ?- return return tab ?\' ?\' ?\' return ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p ?\C-p tab] 0 "%d"))
(global-set-key (kbd "C-c (") 'insert-py-docstring)
