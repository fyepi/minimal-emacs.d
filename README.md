# Minimal ~/.emacs.d

This repository hosts a minimal Emacs configuration with `early-init.el` and `init.el` files. It is designed to serve as a base for your vanilla Emacs configuration, offering a robust foundation for a better vanilla Emacs experience.

## Installation

Execute the following command to clone this repository into `~/.emacs.d`:
```
git clone https://github.com/jamescherti/minimal-emacs.d ~/.emacs.d
```

## Features

1. **Performance Improvements:**
   - Increases the amount read from processes in a single chunk.
   - Customizes `file-name-handler-alist` for improved startup time and package load time.
   - Reduces rendering workload by not rendering cursors or regions in non-focused windows.
   - Disables warnings from the legacy advice API and suppresses warnings about aliased variables.
   - Avoids unnecessary excessive UI updates.
   - Disables font compacting to avoid high memory usage.
   - Prefers loading newer compiled files.
   - Reduces startup screen and message noise, including removing the "For information about GNU Emacs..." message.
   - Configures Emacs to start with a scratch buffer in `fundamental-mode` to shave seconds off startup time.
   - Delays garbage collection during startup to improve performance and resets it to a more reasonable value once Emacs has started.

2. **Native Compilation and Byte Compilation:**
   - Configures native compilation and byte compilation settings
   - Suppresses compiler warnings and errors during native compilation.

4. **UI Element Management:**
   - Disables the startup screen and messages, including menu bar, tool bar, and scroll bars.
   - Configures Emacs to avoid resizing frames and minimizes unnecessary UI updates.

5. **Package Management:**
   - Configures package archives and priorities for MELPA, ELPA, and other repositories.

6. **Customizable Initialization Files:**
   - Supports additional configuration files (`pre-init.el`, `post-init.el`, `pre-early-init.el`, and `post-early-init.el`) to allow further customization at different stages of the startup process.

7. **File Management:**
   - Manages auto-save and backup files, including backup options and version control settings.

8. **User Experience Enhancements:**
   - Configures user interface settings such as cursor behavior, scrolling, and response to prompts.
   - Disables beeping and blinking to avoid distractions.

9. **Buffer and Window Configuration:**
   - Sets default fringe widths and buffer boundaries.
   - Configures smooth scrolling and cursor behavior for a more seamless editing experience.

## Update

To keep your Emacs configuration up to date, you can pull the latest changes from the repository. Run the following command in your terminal:

```
git -C ~/.emacs.d pull
```

## Customizations

The `init.el` and `early-init.el` files should never be modified directly because they are intended to be managed by Git during an update.

The minimal-emacs.d init files support additional customization files that are loaded at different stages of the Emacs startup process. These files allow you to further customize the initialization sequence:

- `~/.emacs.d/pre-init.el`: This file is loaded before `init.el`. Use it to set up variables or configurations that need to be available early in the initialization process but after `early-init.el`.

- `~/.emacs.d/post-init.el`: This file is loaded after `init.el`. It is useful for additional configurations or package setups that depend on the configurations in `init.el`.

- `~/.emacs.d/pre-early-init.el`: This file is loaded before `early-init.el`. Use it for configurations that need to be set even earlier in the startup sequence, typically affecting the initial setup of the Emacs environment.

- `~/.emacs.d/post-early-init.el`: This file is loaded after `early-init.el` but before `init.el`. It is useful for setting up configurations that depend on the early initialization but need to be set before the main initialization begins.

## Frequently asked questions

### How to configure Vim keybindings using Evil?

Configuring Vim keybindings in Emacs can greatly enhance your editing efficiency if you are accustomed to Vim's modal editing style. Add the following to `~/.emacs.d/post-init.el` set up Evil along with some additional packages for undo functionality:

``` emacs-lisp
(use-package undo-fu
  :ensure t)

(use-package undo-fu-session
  :ensure t)

(use-package evil
  :ensure t
  :after undo-fu
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-want-integration t)
  :config
  (evil-collection-init))
```

## License

Copyright (C) 2024 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [minimal-emacs.d @GitHub](https://github.com/jamescherti/minimal-emacs.d)
