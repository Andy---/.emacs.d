(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'custom-theme-load-path "/home/andy/.emacs.d/elpa")
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

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
(use-package python-mode
  :ensure t)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq py-python-command "python3")
;;(setq py-shell-name "/usr/bin/python2")
;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
;;(setq py-python-command-args
;;  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

;;(require 'ipython)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--simple-prompt"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;(require 'ein)

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

;; Ask user a "y or n" question.
(defalias 'yes-or-no-p 'y-or-n-p)

;; IDO-MODE
(ido-mode t)
(setq ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

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
 
 ;;smex
(use-package smex
  :ensure t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;Magit
(use-package magit
  :ensure t
  :bind (("C-x C-g s" . magit-status))
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-stage-all-confirm nil)
  (setq magit-unstage-all-confirm nil)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;;Soundklaus
;;(require 'emms-setup)
;;(emms-standard)
;;(emms-default-players)
;;(require 'emms-player-vlc)
;;(define-emms-simple-player vlc '(file url)
;;  (concat "\\`\\(https?\\|mms\\)://\\|"
;;      (emms-player-simple-regexp
;;       "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
;;       "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
;;       "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape")
;;      "vlc" "--intf=rc"))

;;(ido-vertical-mode)

(scroll-bar-mode -1)

(use-package abyss-theme
  :ensure t
  :init
  (load-theme #'abyss t))


	  
