;;; init-lsp.el --- init lsp -*- no-byte-compile: t; lexical-binding: t; -*-

;; (use-package eglot
;;   :ensure nil
;;   :defer t
;;   :commands (eglot
;;              eglot-rename
;;              eglot-ensure
;;              eglot-rename
;;              eglot-format-buffer)
;; 
;;   :custom
;;   (eglot-report-progress nil)  ; Prevent minibuffer spam
;; 
;;   :config
;;   ;; Optimizations
;;   (fset #'jsonrpc--log-event #'ignore)
;;   (setq jsonrpc-event-hook nil))

;; LSPPac
;; (use-package lsp-mode
;;   :preface
;;   (defun lsp-booster--advice-json-parse (old-fn &rest args)
;;     "Try to parse bytecode instead of json."
;;     (or
;;      (when (equal (following-char) ?#)
;; 
;;        (let ((bytecode (read (current-buffer))))
;;          (when (byte-code-function-p bytecode)
;;            (funcall bytecode))))
;;      (apply old-fn args)))
;;   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
;;     "Prepend emacs-lsp-booster command to lsp CMD."
;;     (let ((orig-result (funcall old-fn cmd test?)))
;;       (if (and (not test?)                             ;; for check lsp-server-present?
;;                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
;;                lsp-use-plists
;;                (not (functionp 'json-rpc-connection))  ;; native json-rpc
;;                (executable-find "emacs-lsp-booster"))
;;           (progn
;;             (message "Using emacs-lsp-booster for %s!" orig-result)
;;             (cons "emacs-lsp-booster" orig-result))
;;         orig-result)))
;; 
;;   :defer t
;;   :commands lsp
;;   :custom
;;   (lsp-keymap-prefix "C-x l")
;;   (lsp-auto-guess-root nil)
;;   (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
;;   (lsp-enable-file-watchers nil)
;;   (lsp-enable-folding nil)
;;   (lsp-ui-doc-enable nil)
;;   (read-process-output-max (* 1024 1024))
;;   (lsp-keep-workspace-alive nil)
;;   (lsp-eldoc-hook nil)
;;   :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
;; 
;;   :hook ((java-mode python-mode go-mode rust-mode fsharp-mode
;;                     js-mode js2-mode web-mode typescript-ts-mode
;;                     c-mode c++-mode objc-mode) . lsp-deferred)
;;   :config
;;   (defun lsp-update-server ()
;;     "Update LSP server."
;;     (interactive)
;;     ;; Equals to `C-u M-x lsp-install-server'
;;     (lsp-install-server t))
;;   :init
;;   ;; (setq lsp-use-plists t)
;;   (advice-add (if (progn (require 'json)
;;                          (fboundp 'json-parse-buffer))
;;                   'json-parse-buffer
;;                 'json-read)
;;               :around
;;               #'lsp-booster--advice-json-parse)
;;   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))
;; ;; -LSPPac
;; 
;; ;; LSPUI
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;   (lsp-ui-doc-background ((t (:background nil))))
;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind
;;   (:map lsp-ui-mode-map
;;         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;         ([remap xref-find-references] . lsp-ui-peek-find-references)
;;         ("C-c u" . lsp-ui-imenu)
;;         ("M-i" . lsp-ui-doc-focus-frame))
;;   (:map lsp-mode-map
;;         ("M-n" . forward-paragraph)
;;         ("M-p" . backward-paragraph))
;;   :custom
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-border (face-foreground 'default))
;;   (lsp-ui-sideline-enable t)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-code-actions t)
;;   (lsp-ui-sideline-show-hover nil)
;;   :config
;;   ;; Use lsp-ui-doc-webkit only in GUI
;;   (when (display-graphic-p)
;;     (setq lsp-ui-doc-use-webkit t))
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil))
;;   ;; `C-g'to close doc
;;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))
;; ;; -LSPUI
;; 
;; ;; DAPPac
;; (use-package dap-mode
;;   :diminish
;;   :bind
;;   (:map dap-mode-map
;;         (("<f12>" . dap-debug)
;;          ("<f8>" . dap-continue)
;;          ("<f9>" . dap-next)
;;          ("<M-f11>" . dap-step-in)
;;          ("C-M-<f11>" . dap-step-out)
;;          ("<f7>" . dap-breakpoint-toggle))))
;; ;; -DAPPac



(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  :init
  (setq lsp-use-plists t))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))


(provide 'init-lsp)
