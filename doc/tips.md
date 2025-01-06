# Emacs Feature Tour

## Basics

* <kbd>C-g</kbd> as many times as needed, to get out of a prompt
* [How to open and save files using Emacs](http://mally.stanford.edu/~sr/computing/emacs.html) - remember that arrow keys and <kbd>Page Up</kbd>/<kbd>Page Down</kbd> work fine as well.

## Browsing Projects

* <kbd>C-c p a a</kbd> to locate projects under the given directory
* <kbd>C-c p a s</kbd> to switch Emacs to a local asdf-managed version of node.js or other development program
* <kbd>C-c p c</kbd> to start a compilation at the top of the current project
* <kbd>C-c p f</kbd> to find a file within the current project
* <kbd>C-c p g g</kbd> to start a Claude AI interaction buffer
* <kbd>C-c p g p</kbd> to bring up a gptel menu for taking different AI actions
* <kbd>C-c p g q</kbd> to bring up a Claude AI query about a selected region
* <kbd>C-c p k</kbd> to close all files in the current project
* <kbd>C-c p p</kbd> to visit a known project in Magit, which is a git status browser and git shell
* <kbd>C-c p t</kbd> to switch to a different open buffer within the project
* <kbd>C-c p s r</kbd> to search content within a project non-interactively
* <kbd>C-c p s s</kbd> to search content within a project interactively
* <kbd>C-c p w p</kbd> to copy the part of the file path relative to the project root
* <kbd>C-c p w w</kbd> to copy the file path of the current buffer
* <kbd>C-c p !</kbd> to run a shell command at the top of the current project

## Using Magit

* <kbd>C-c V s</kbd> to visit Magit Status Mode from any file within a git project
* <kbd>C-c V l</kbd> to visit Magit Log mode and see a list of git revisions made to the project, which can be opened
* <kbd>C-c V b</kbd> to visit Magit Branch mode and see a list of git branches to checkout
* <kbd>C-c V a</kbd> to open `git blame` on the current file - use <kbd>q</kbd> to exit when done

## Search

* <kbd>C-c p s s</kbd> to search an entire project for a phrase as you type, using `ripgrep`
  * <kbd>RET</kbd> to visit a result, or
  * <kbd>C-c C-o</kbd> to move the results into a separate buffer. <kbd>r</kbd> within that buffer will allow you to start making changes to any text in that buffer, and <kbd>C-c C-c</kbd> applies the changes.
* <kbd>C-c p s r</kbd> to search an entire project and put the results in a buffer with links that can be visited
  * <kbd>g</kbd> to refresh the buffer, which is useful for refactoring, if you've changed any files
* <kbd>C-c p r</kbd> to search and replace a phrase throughout an entire project, asking you what to do with each match

## Finding Definitions

When you're visiting a source code file, you can use these commands to navigate to the definition of the function that
the cursor is at.

* <kbd>C-c . .</kbd> to visit the definition
* <kbd>C-c . ,</kbd> to return to the original point
* <kbd>C-c . /</kbd> to get a quick view of the definition

## Editing Directories

* <kbd>C-c p D</kbd> to open the root of the project in Dired Mode, which looks like `ls` output but you can manipulate it
* <kbd>C-c p d</kbd> to open a different project directory in Dired
* <kbd>RET</kbd> when in Dired, will visit the file at point
* <kbd>r</kbd> when in Dired, will allow you to make changes to the names of any files
  * <kbd>C-c C-c</kbd> applies the changes
  * <kbd>C-c ESC</kbd> removes the changes and takes you back to Dired
* <kbd>C</kbd> when in Dired, will copy the file
* <kbd>R</kbd> when in Dired, will rename the file or move it to a different directory

## Binaries

The following shortcuts are added to `~/emacs-shared/bin` which can be added to your `$PATH`:

* `en`: Open the provided file in the existing Emacs window, and return right away
* `et`: Open the provided file in terminal Emacs, and wait until done editing
* `ew`: Open the provided file in a new Emacs window, and wait until done editing

---

[Back to README.md](../README.md#documentation)
