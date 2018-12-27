;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defun setToTextProg (myMode)
  (dolist (hook
           '(prog-mode-hook text-mode-hook ))
    (add-hook hook myMode)))


(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Operator Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Mod" :size 19))

(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(def-package! drag-stuff
  :config
  (setq drag-stuff-mode t))

(def-package! vmd-mode)

(def-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setToTextProg #'highlight-indent-guides-mode))

(map!
 (:after evil
   :n "M-h" #'next-buffer
   :n "M-l" #'previous-buffer
   :n "M-j" #'drag-stuff-down
   :n "M-k" #'drag-stuff-up

   (:leader
     (:desc "Dumb jump" :prefix "d"
       :desc "Jump to symbol"    :nv "j" #'dumb-jump-go
       :desc "Jump back"         :nv "k" #'dumb-jump-back
       :desc "Quick look"        :nv "q" #'dumb-jump-quick-look)))

 (:after ivy
   :n "M-F" #'+ivy/project-search
   :n "M-f" #'swiper	))

(setq c-basic-offset 2)
;; web development
(setq coffee-tab-width 2) ; coffeescript
(setq javascript-indent-level 2) ; javascript-mode
(setq js-indent-level 2) ; js-mode
(setq js2-basic-offset 2) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
(setq web-mode-markup-indent-offset 2) ; web-mode, html tag in html file
(setq web-mode-css-indent-offset 2) ; web-mode, css in html file
(setq web-mode-code-indent-offset 2) ; web-mode, js code in html file
(setq css-indent-offset 2) ; css-mode

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq evil-ex-search-case 'sensitive)

(load-file "~/.doom.d/fira.el")

(set-face-attribute 'font-lock-comment-face nil :inherit 'font-lock-comment-face :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :inherit 'font-lock-function-name-face :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :inherit 'font-lock-variable-name-face :slant 'italic)

(with-eval-after-load "js2-mode"
  (set-face-attribute 'js2-function-param nil :inherit 'font-lock-variable-name-face :slant 'italic))

(with-eval-after-load "rjsx-mode"
  (set-face-attribute 'rjsx-attr nil :inherit 'font-lock-variable-name-face :slant 'normal)
  (set-face-attribute 'rjsx-tag nil :inherit 'font-lock-function-name-face :slant 'italic)
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  )

(with-eval-after-load "whitespace-mode"
  (set-face-attribute 'whitespace-hspace nil :background "maroon2"))

(set-face-attribute 'nobreak-space nil :background "maroon2")
(set-face-attribute 'nobreak-hyphen nil :background "maroon2")

(setToTextProg #'fira-code-mode)

(setq whitespace-line-column 500)
(setToTextProg #'whitespace-mode)

(defvar my-prev-whitespace-mode nil)
(make-variable-buffer-local 'my-prev-whitespace-mode)
(defun pre-popup-draw ()
  "Turn off whitespace mode before showing company complete tooltip"
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1)
        (setq my-prev-whitespace-mode t))))
(defun post-popup-draw ()
  "Restore previous whitespace mode after showing company tooltip"
  (if my-prev-whitespace-mode
      (progn
        (whitespace-mode 1)
        (setq my-prev-whitespace-mode nil))))

(advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
(advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)

(add-hook 'graphql-mode-hook (lambda() (whitespace-mode -1)))
