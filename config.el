;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(setq-default initial-major-mode 'markdown-mode)


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
  (set-face-attribute 'highlight-indent-guides-character-face nil :inherit 'custom-variable-obsolete)
  (setToTextProg #'highlight-indent-guides-mode))

(defun vsplit-and-create-buffer ()
  (interactive)
  (split-window-horizontally)
  (let (($buf (generate-new-buffer "*temp*")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(map!
 (:leader
   (:desc "custom" :prefix "j"
     :desc "Jump to symbol"        :nv "g" #'dumb-jump-go
     :desc "Jump back"             :nv "b" #'dumb-jump-back
     :desc "Quick look"            :nv "q" #'dumb-jump-quick-look
     :desc "Deer"                  :nv "d" #'deer
     :desc "Avy go to line"        :nv "l" #'avy-goto-line
     :desc "Avy word"              :nv "w" #'avy-goto-word-1
     :desc "Select all"            :nv "a" #'evil-multiedit-match-all
     )
   (:prefix "b"
     :desc "Split & Create Buffer" :n "x" #'vsplit-and-create-buffer)
   )


 (:after evil
   :n "H" #'previous-buffer
   :n "L" #'next-buffer
   :nvi "M-j" #'drag-stuff-down
   :nvi "M-k" #'drag-stuff-up
   :n "M-d" #'evil-multiedit-match-and-next
   :g "M-c" #'evil-yank
   :g "M-v" #'yank
   :g "M-a" #'mark-whole-buffer
   )

 (:after ranger-key
   (:map ranger-mode-map
     "H"   nil
     "L"   nil
     )
   )

 (:after ivy
   :n "M-F" #'+ivy/project-search
   :n "M-f" #'swiper
   (:map ivy-minibuffer-map
     "C-j"  nil
     "C-k"  nil
     )
   )

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

(def-package! js2-mode
  :config
  (set-face-attribute 'js2-function-param nil :inherit 'font-lock-variable-name-face :slant 'italic))

(defun my/prettier-setup ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    (if (not (and prettier (file-executable-p prettier)))
        ;; hack to remove formatting for js files if prettier is not installed locally
        (advice-remove #'format-all-buffer :override #'+format/buffer)
      )))

(def-package! rjsx-mode
  :config
  (set-face-attribute 'rjsx-attr nil :inherit 'font-lock-variable-name-face :slant 'normal)
  (set-face-attribute 'rjsx-tag nil :inherit 'font-lock-function-name-face :slant 'italic)
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'my/prettier-setup))

(set-face-attribute 'nobreak-space nil :background "maroon2")
(set-face-attribute 'nobreak-hyphen nil :background "maroon2")
;; ----------------- override font faces---------------------------------------------

;; set minor modes to prog and text mode
(setToTextProg #'fira-code-mode)

(setq whitespace-line-column 500)
(setToTextProg #'whitespace-mode)
(setq whitespace-style '(trailing tabs lines-tail newline newline-mark))
;;---------------------------------------handle whitespace mode with company popup
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
;;---------------------------------------handle whitespace mode with company popup

;;---------------------------------------handle whitespace mode in graphql mode
;; (add-hook 'graphql-mode-hook (lambda() (whitespace-mode -1)))

(global-linum-mode -1)
(setToTextProg #'display-line-numbers-mode)
(setq ranger-deer-show-details t)
(add-hook 'typescript-mode-hook #'my/prettier-setup)

(def-package! web-mode
  :config
  (set-face-attribute 'web-mode-html-tag-face nil :inherit 'web-mode-html-tag-face :slant 'italic))

;; enable emmet for all js based modes
(dolist (hook
         '(rjsx-mode-hook web-mode-hook typescript-mode-hook tide-mode-hook))
  (add-hook hook 'emmet-mode))

(add-hook 'graphql-mode-hook #'my/prettier-setup)

(setq +doom-modeline-buffer-file-name-style 'relative-to-project)

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(global-visual-line-mode +1)
