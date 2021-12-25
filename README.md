[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/isearch-project-badge.svg)](https://melpa.org/#/isearch-project)
[![MELPA Stable](https://stable.melpa.org/packages/isearch-project-badge.svg)](https://stable.melpa.org/#/isearch-project)

# isearch-project
> Incremental search through the whole project.

[![CI](https://github.com/jcs-elpa/isearch-project/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/isearch-project/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/isearch-project-demo.gif" width="450" height="513"/>
</p>

This is an alternative package replacing `ag` searcher. The  logic behinds this 
package is similar to  [Visual Studio](https://visualstudio.microsoft.com/)
`find and replace` functionalities, but using `isearch`.  It simply searches 
`regexp` in each file accordingly to the project file tree.

`isearch` gives a better visuality than `ag` does. `ag` is great but is verbose 
for me to use because it gives you a whole list that matches even the matching line 
are in the exact same line.

*P.S. Inspired by [Visual Studio](https://visualstudio.microsoft.com/) `find and replace` preset behavior.*

## Customization

Set the paths you usually want to ignore to search through.

```el
(setq isearch-project-ignore-paths '(".vs/"
                                     ".vscode/"
                                     "node_modules/"))
```

## Usage

Call it from `minibuffer` directly, 

```
M-x isearch-project-forward
```

Or you can bind it globally to any key you want.

```el
(global-set-key (kbd "any-key") #'isearch-project-forward)
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
