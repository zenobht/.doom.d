;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(defun setToTextProg (myMode)
  (dolist (hook
           '(prog-mode-hook text-mode-hook ))
    (add-hook hook myMode)))

;; doom theme config
(setq doom-theme 'doom-one)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; doom font config
(setq doom-font (font-spec :family "Operator Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Mod" :size 19))

;; enable drag-stuff-mode
(def-package! drag-stuff
  :config
  (setq drag-stuff-mode t))

(def-package! vmd-mode)

(def-package! kotlin-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode)))

;; setup highlight-indent-guides
(def-package! highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setToTextProg #'highlight-indent-guides-mode))

(map!
 (:after evil
   :g "M-h" #'next-buffer
   :g "M-l" #'previous-buffer
   :g "M-j" #'drag-stuff-down
   :g "M-k" #'drag-stuff-up

   (:leader
     (:desc "Dumb jump" :prefix "d"
       :desc "Jump to symbol"    :nv "j" #'dumb-jump-go
       :desc "Jump back"         :nv "k" #'dumb-jump-back
       :desc "Quick look"        :nv "q" #'dumb-jump-quick-look)))

 (:after ivy
   :n "s-F" #'+ivy/project-search
   :n "s-f" #'swiper	)

 (:after dired
   :map dired-mode-map
   :n "J" #'dired-up-directory))


;; indentation config
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

;; frame config
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; enable case sensitive search in evil
(setq evil-ex-search-case 'sensitive)

;; load custom fira liguatures config
(load-file "~/.doom.d/fira.el")

;; ----------------- override font faces---------------------------------------------
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

(set-face-attribute 'nobreak-space nil :background "maroon2")
(set-face-attribute 'nobreak-hyphen nil :background "maroon2")
;; ----------------- override font faces---------------------------------------------

;; set minor modes to prog and text mode
(setToTextProg #'fira-code-mode)

;; (setq whitespace-line-column 500)
;; (setToTextProg #'whitespace-mode)

;; ;;---------------------------------------handle whitespace mode with company popup
;; (defvar my-prev-whitespace-mode nil)
;; (make-variable-buffer-local 'my-prev-whitespace-mode)

;; (defun pre-popup-draw ()
;;   "Turn off whitespace mode before showing company complete tooltip"
;;   (if whitespace-mode
;;       (progn
;;         (setq my-prev-whitespace-mode t)
;;         (whitespace-mode -1)
;;         (setq my-prev-whitespace-mode t))))

;; (defun post-popup-draw ()
;;   "Restore previous whitespace mode after showing company tooltip"
;;   (if my-prev-whitespace-mode
;;       (progn
;;         (whitespace-mode 1)
;;         (setq my-prev-whitespace-mode nil))))

;; (advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
;; (advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)
;; ;;---------------------------------------handle whitespace mode with company popup

;; ;;---------------------------------------handle whitespace mode in graphql mode
;; (add-hook 'graphql-mode-hook (lambda() (whitespace-mode -1)))
