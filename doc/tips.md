Emacs Feature Tour
==================

Browsing Projects
-----------------

* <kbd>M-x projectile-discover-projects-in-directory RET</kbd> to tell Emacs about all projects within a directory tree, like `~/devel` - whenever Emacs finds a `.git` directory, it will add that directory as a project
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

* <kbd>C-c p s s</kbd> to search an entire project for a phrase as you type, using `ripgrep` - use <kbd>C-c C-o</kbd> to move the results into a separate buffer. <kbd>r</kbd> within that buffer will allow you to start making changes to any text in that buffer, and <kbd>C-c C-c</kbd> applies the changes.
* <kbd>C-c p s r</kbd> to search an entire project and put the results in a buffer with links that can be visited - use <kbd>g</kbd> to refresh the buffer if you've changed it, which is useful for refactoring

Finding definitions
-------------------

When you're visiting a source code file, you can use these commands to navigate to the definition of the function that
the cursor is at.

* <kbd>C-c . .</kbd> to visit the definition
* <kbd>C-c . ,</kbd> to return to the original point
* <kbd>C-c . /</kbd> to get a quick view of the definition

---

[Back to README.md](https://github.com/mwolson/emacs-shared#documentation)
