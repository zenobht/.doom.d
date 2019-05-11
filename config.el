;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-
;; ------------------------------functions----------------------------------

(defun my/setToTextProg (myMode)
  (dolist (hook
           '(prog-mode-hook text-mode-hook ))
    (add-hook hook myMode)))

(defun my/vsplit-and-create-buffer ()
  (interactive)
  (split-window-horizontally)
  (let (($buf (generate-new-buffer "*temp*")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))


(defun my/pre-popup-draw ()
  "Turn off whitespace mode before showing company complete tooltip"
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1)
        (setq my-prev-whitespace-mode t))))

(defun my/post-popup-draw ()
  "Restore previous whitespace mode after showing company tooltip"
  (if my-prev-whitespace-mode
      (progn
        (whitespace-mode 1)
        (setq my-prev-whitespace-mode nil))))

(defun my/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

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

(defun my/escape-and-save ()
  (interactive)
  (evil-force-normal-state)
  (save-buffer)
  )

;; -------------------------config------------------------------------------------
(setq
      ;; key mappings
      mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil

      ;; theme config
      doom-theme 'doom-one
      doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t   ; if nil, italics is universally disabled
      +doom-modeline-buffer-file-name-style 'relative-to-project

      ;; font config
      doom-font (font-spec :family "Operator Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Mod" :size 19)

      doom-modeline-buffer-file-name-style 'file-name
      doom-modeline-icon t
      doom-modeline-major-mode-icon t

      ;; indentation config
      c-basic-offset 2

      ;; web development
      coffee-tab-width 2 ; coffeescript
      javascript-indent-level 2 ; javascript-mode
      js-indent-level 2 ; js-mode
      js2-basic-offset 2 ; js2-mode, in latest js2-mode, it's alias of js-indent-level
      web-mode-markup-indent-offset 2 ; web-mode, html tag in html file
      web-mode-css-indent-offset 2 ; web-mode, css in html file
      web-mode-code-indent-offset 2 ; web-mode, js code in html file
      css-indent-offset 2 ; css-mode

      ;; whitespace config
      whitespace-line-column 500
      whitespace-style '(trailing tabs lines-tail newline newline-mark)

      ;; search config for /
      ;; enable case sensitive search in evil
      evil-ex-search-case 'sensitive
      )

(setq-default initial-major-mode 'markdown-mode)

;; ------------------------package config----------------------------------------------
;; enable drag-stuff-mode
(def-package! drag-stuff
  :defer t
  :config
  (setq drag-stuff-mode t))

(def-package! ranger
  :defer t
  :config
  (setq ranger-deer-show-details t
        ranger-cleanup-on-disable t))

(def-package! vmd-mode
    :defer t)

(def-package! kotlin-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode)))

;; setup highlight-indent-guides
(def-package! highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  (set-face-attribute 'highlight-indent-guides-character-face nil :inherit 'custom-variable-obsolete)
  (my/setToTextProg #'highlight-indent-guides-mode))

(def-package! js2-mode
  :defer t
  :config
  (set-face-attribute 'js2-function-param nil :inherit 'font-lock-variable-name-face :slant 'italic))

(def-package! web-mode
  :defer t
  :config
  (set-face-attribute 'web-mode-html-tag-face nil :inherit 'web-mode-html-tag-face :slant 'italic))

(def-package! rjsx-mode
  :defer t
  :config
  (set-face-attribute 'rjsx-attr nil :inherit 'font-lock-variable-name-face :slant 'normal)
  (set-face-attribute 'rjsx-tag nil :inherit 'font-lock-function-name-face :slant 'italic)
  (add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
  (add-hook 'rjsx-mode-hook #'my/prettier-setup))

(def-package! markdown-mode
  :defer t
  :config
  ;; disable cmd+` in markdown mode as it blocks switching frames'
  (map! :map markdown-mode-map
        "M-`"  #'other-frame
        (:leader (:prefix "m" :n "v" #'vmd-mode))
        )
  (setq markdown-command "/usr/local/bin/pandoc")
  )

;;------------------------------------key bindings-----------------------------------
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
     :desc "Split & Create Buffer" :n "x" #'my/vsplit-and-create-buffer
     :desc "Switch buffer"         :n "b" #'projectile-switch-to-buffer
     )
   (:prefix "w"
     :desc "Kill buffer and split" :n "C" #'kill-buffer-and-window
     :desc "Delete window"         :n "c" #'delete-window
     )
   )

 (:after evil
   :n "M-[" #'dumb-jump-back
   :n "M-]" #'dumb-jump-go
   :n "M-{" #'previous-buffer
   :n "M-}" #'next-buffer
   :nvi "M-j" #'drag-stuff-down
   :nvi "M-k" #'drag-stuff-up
   :n "M-d" #'evil-multiedit-match-and-next
   ;;-------------------------------------------------------------------------------
   ;; keybindings that mimics cmd functions
   :g "M-h" #'ns-do-hide-emacs
   :g "M-H" #'ns-do-hide-others
   :g "M-`" #'+workspace/cycle ;; as only one frame is open always, switch between workspaces
   :g "M-c" #'evil-yank ;; copy
   :g "M-v" #'yank  ;; paste
   :g "M-a" #'mark-whole-buffer ;; select all
   :g "M-q" #'evil-quit  ;; quit
   ;; :g "M-`" #'other-frame
   :g "M-s" #'my/escape-and-save
   )

 (:after ranger-key
   (:map ranger-mode-map
     "H"   nil
     "L"   nil
     )
   )
 ;; (:after helm
 ;;   :n "M-F" #'helm-projectile-rg
 ;;   :n "M-f" #'swiper-helm
 ;;   )

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

;; frame config
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; load custom fira liguatures config
(load-file "~/.doom.d/fira.el")

;; ----------------- override font faces---------------------------------------------
(set-face-attribute 'font-lock-comment-face nil :inherit 'font-lock-comment-face :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :inherit 'font-lock-function-name-face :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :inherit 'font-lock-variable-name-face :slant 'italic)

(set-face-attribute 'nobreak-space nil :background "maroon2")
(set-face-attribute 'nobreak-hyphen nil :background "maroon2")
;; ----------------- override font faces---------------------------------------------

;; set minor modes to prog and text mode
(my/setToTextProg #'fira-code-mode)

(my/setToTextProg #'whitespace-mode)
;;---------------------------------------handle whitespace mode with company popup
(defvar my-prev-whitespace-mode nil)
(make-variable-buffer-local 'my-prev-whitespace-mode)

(advice-add 'company-pseudo-tooltip-unhide :before #'my/pre-popup-draw)
(advice-add 'company-pseudo-tooltip-hide :after #'my/post-popup-draw)
;;---------------------------------------handle whitespace mode with company popup

;;---------------------------------------handle whitespace mode in graphql mode
;; (add-hook 'graphql-mode-hook (lambda() (whitespace-mode -1)))

(global-visual-line-mode +1)
(global-linum-mode -1)
(my/setToTextProg #'display-line-numbers-mode)

(add-hook 'typescript-mode-hook #'my/prettier-setup)

;; enable emmet for all js based modes
(dolist (hook
         '(rjsx-mode-hook web-mode-hook typescript-mode-hook tide-mode-hook))
  (add-hook hook 'emmet-mode))

(add-hook 'graphql-mode-hook #'my/prettier-setup)

(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))
