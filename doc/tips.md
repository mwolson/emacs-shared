Emacs Feature Tour
==================

Basics
------

* <kbd>C-g</kbd> as many times as needed, to get out of a prompt
* [How to open and save files using Emacs](http://mally.stanford.edu/~sr/computing/emacs.html) - remember that arrow keys and <kbd>Page Up</kbd>/<kbd>Page Down</kbd> work fine as well.

Browsing Projects
-----------------

* <kbd>M-x projectile-discover-projects-in-directory RET</kbd> to tell Emacs about all projects within a directory tree, like `~/devel`. Whenever Emacs finds a `.git` directory, it will add that directory as a project.
* <kbd>C-c p p</kbd> to visit a known project in Magit, which is a git status browser and git shell
* <kbd>C-c p f</kbd> to find a file within the current project

Using Magit
-----------

* <kbd>C-c V s</kbd> to visit Magit Status Mode from any file within a git project
* <kbd>C-c V l</kbd> to visit Magit Log mode and see a list of git revisions made to the project, which can be opened
* <kbd>C-c V b</kbd> to visit Magit Branch mode and see a list of git branches to checkout
* <kbd>C-c V a</kbd> to open `git blame` on the current file - use <kbd>q</kbd> to exit when done

Search
------

* <kbd>C-c p s s</kbd> to search an entire project for a phrase as you type, using `ripgrep`
  * <kbd>RET</kbd> to visit a result, or
  * <kbd>C-c C-o</kbd> to move the results into a separate buffer. <kbd>r</kbd> within that buffer will allow you to start making changes to any text in that buffer, and <kbd>C-c C-c</kbd> applies the changes.
* <kbd>C-c p s r</kbd> to search an entire project and put the results in a buffer with links that can be visited
  * <kbd>g</kbd> to refresh the buffer, which is useful for refactoring, if you've changed any files
* <kbd>C-c p r</kbd> to search and replace a phrase throughout an entire project, asking you what to do with each match

Finding Definitions
-------------------

When you're visiting a source code file, you can use these commands to navigate to the definition of the function that
the cursor is at.

* <kbd>C-c . .</kbd> to visit the definition
* <kbd>C-c . ,</kbd> to return to the original point
* <kbd>C-c . /</kbd> to get a quick view of the definition

Editing Directories
-------------------

* <kbd>C-c p D</kbd> to open the root of the project in Dired Mode, which looks like `ls` output but you can manipulate it
* <kbd>C-c p d</kbd> to open a different project directory in Dired
* <kbd>RET</kbd> when in Dired, will visit the file at point
* <kbd>r</kbd> when in Dired, will allow you to make changes to the names of any files
  * <kbd>C-c C-c</kbd> applies the changes
  * <kbd>C-c ESC</kbd> removes the changes and takes you back to Dired
* <kbd>C</kbd> when in Dired, will copy the file
* <kbd>R</kbd> when in Dired, will rename the file or move it to a different directory

---

[Back to README.md](https://github.com/mwolson/emacs-shared#documentation)
