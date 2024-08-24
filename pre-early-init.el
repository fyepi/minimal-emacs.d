;;; pre-early-init.el --- pre early init -*- no-byte-compile: t; lexical-binding: t; -*-

;; Enable both file dialogs and dialog boxes
(setq minimal-emacs-disable-dialogs nil)

;; Enable context menu (right-click menu)
(setq minimal-emacs-disable-context-menu nil)

;; Enable the tool bar at the top
(setq minimal-emacs-disable-tool-bar nil)

;; Enable the menu bar at the top
(setq minimal-emacs-disable-menu-bar nil)

;; Enable tooltips (hover text)
(setq minimal-emacs-disable-tooltips nil)

(setenv "LSP_USE_PLISTS" "true")
