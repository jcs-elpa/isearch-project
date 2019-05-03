[![Build Status](https://travis-ci.com/jcs090218/isearch-project.svg?branch=master)](https://travis-ci.com/jcs090218/isearch-project)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


# isearch-project
> Incremental search through the whole project.

<p align="center">
  <img src="./screenshot/isearch-project-demo.gif" width="450" height="513"/>
</p>

This is an alternative package replacing `ag` searcher. The 
logic behinds this package is similar to 
[Visual Studio](https://visualstudio.microsoft.com/)
`find and replace` functionalities, but using `isearch`. 
It simply searches `regexp` in each file accordingly to the 
project file tree.

`isearch` gives a better visuality than `ag` does. `ag` is 
great but is verbose for me to use because it gives you a 
whole list that matches even the matching line are in the 
exact same line.

*P.S. Inspired by [Visual Studio](https://visualstudio.microsoft.com/) `find and replace` preset behavior.*


## Configuration
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


## Contribution
If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
