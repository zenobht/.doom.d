;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fira Code Mod" :size 13)
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Fira Code Mod" :size 19))

(def-package! ns-auto-titlebar
  :config
  (setq ns-auto-hide-menu-bar nil
        ns-auto-titlebar-mode t))

(map!
 (:after evil
 :n "M-h" #'next-buffer
 :n "M-l" #'previous-buffer))
