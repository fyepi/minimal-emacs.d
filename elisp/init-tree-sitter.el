;;; init-tree-sitter.el --- init tree sitter -*- no-byte-compile: t; lexical-binding: t; -*-


(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq my-js-tsauto-config
      (make-treesit-auto-recipe
       :lang 'javascript
       :ts-mode 'js-ts-mode
       :remap '(js2-mode js-mode javascript-mode)
       :url "https://github.com/tree-sitter/tree-sitter-javascript"
       :revision "master"
       :source-dir "src"
       :ext "\\.js\\'"))

(add-to-list 'treesit-auto-recipe-list my-js-tsauto-config)

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))


(provide 'init-tree-sitter)
