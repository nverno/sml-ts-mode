# SML major mode using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This mode provides the following features for Standard ML (SML) buffers:

- Indentation
- Font-locking
- Imenu
- Structural navigation

![example](doc/sml-ts-mode.png)

## Installing

Emacs 29.1 or above with tree-sitter support is required. 

This package requires the SML tree-sitter grammar from
https://github.com/MatthewFluet/tree-sitter-sml. _It won't work with other
parsers!_

### Installing the SML parser

Add the source to `treesit-language-source-alist`. 

```elisp
(add-to-list
 'treesit-language-source-alist
 '(sml "https://github.com/MatthewFluet/tree-sitter-sml"))
```

Then run `M-x treesit-install-language-grammar` and select `sml` to install.
