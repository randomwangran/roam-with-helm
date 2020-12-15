<img src="pic/demo-show-selection.gif" align="center">


# Installation

Clone or download this repository (path of the folder is the <roam-with-helm> used below).

In your ~/.emacs, add the following two lines:

    (add-to-list 'load-path "<roam-with-helm>") 
    (require 'roam-with-helm)


# Usage

Use `org-roam-find-file` as usual.


## Jump to the content

After input the title, press `RET` to visit the file


## Preview the content

In helm, press `C-c C-f`. When navigation, you can preview the
content.


## Insert the contents as a transclusion link.

This requires user to mark the candidates by pressing `SPC`. After
the selection, press `RET` to insert the candidates as
transclusions.
