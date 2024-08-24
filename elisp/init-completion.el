;;; init-completion.el --- post init -*- no-byte-compile: t; lexical-binding: t; -*-

;; (use-package corfu
;;   :ensure t
;;   :defer t
;;   :commands (corfu-mode global-corfu-mode)
;; 
;;   :hook ((prog-mode . corfu-mode)
;;          (shell-mode . corfu-mode)
;;          (eshell-mode . corfu-mode))
;; 
;;   :custom
;;   ;; Hide commands in M-x which do not apply to the current mode.
;;   (read-extended-command-predicate #'command-completion-default-include-p)
;;   ;; Disable Ispell completion function. As an alternative try `cape-dict'.
;;   (text-mode-ispell-word-completion nil)
;;   (tab-always-indent 'complete)
;; 
;;   ;; Enable Corfu
;;   :config
;;   (global-corfu-mode))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(provide 'init-completion)
