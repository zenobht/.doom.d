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
   :n "M-f" #'+ivy/project-search
   :n "M-F" #'swiper	))

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

(setToTextProg #'fira-code-mode)
(setq whitespace-line-column 500)
(setToTextProg #'whitespace-mode)
