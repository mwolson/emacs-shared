Shared Emacs settings
=====================

This is a collection of Emacs Lisp add-ons and an Emacs init file with the following features.

- Uses the solarized-dark theme by default, with custom code to support using both emacsclient and GUI frame
- Cross-platform support: decent level of feature parity between Linux, Windows, and Mac OS X; this includes a custom
  patch to improve Windows support for magit
- Emacs is fullscreen by default on Windows and OS X, with code to account for dock size of 64 pixels on OS X (options:
  `my-frame-pad-width`, `my-frame-pad-height`, and `my-frame-maximize-p`); frame width and height is customizable if
  you don't like fullscreen
- Can be personalized with a single `setq` statement; there are many 
- Supports list of features to enable
- Principle of least surprise is a design goal
- Same great-looking Inconsolata font on all platforms
- Splash screen appears while starting a non-daemon GUI, for appearance of speed :)
- Extensively-documented setup instructions
- Manuals are pre-built and show up when you do `C-h i`
- Supplements your existing `~/.emacs.d/init.el` file instead of replacing it

Installing
----------

### Install git
  On Windows, the best way to do this is with [Git Extensions](https://code.google.com/p/gitextensions/).  When
  prompted about SSH during installation, choosing PuTTY is recommended.

  On Mac OS X, the best way to do this is to install Xcode from App Store, and then install the Command Line Tools for
  Xcode using the Downloads preferences pane within Xcode 4.6.1 and later.

### (Mac OS X only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier installation of other useful tools, though it's not a strict requirement.

### (Windows only) Install PuTTY

This is useful for doing git development, since Pageant can hold onto your git keys and auto-load them when Windows starts.

### (Optional) Install Aspell

Install Aspell and an Aspell dictionary for your language for spell-checking.  This is more likely to be useful on Windows, which does not come with an ispell variant.

### Install Emacs

The recommended version is Emacs 24.3.  The recommended installers for each OS are:

*Windows*

- Download `emacs-24.3-bin-i386.zip` (or the latest version of the -bin package) from [ftp.gnu.org](http://ftp.gnu.org/gnu/emacs/windows/).
- Unzip to `C:/Program Files (x86)/Emacs`.  When done, you should see a file named `C:/Program Files (x86)/Emacs/bin/runemacs.exe`.
- If you change the location, you may want to update the `my-system-paths` option later.

*Mac OS X*

Use the unofficial [Emacs for OS X](http://emacsformacosx.com/) installer.

You'll want to make sure that your path has the version of Emacs you installed in it, before the ancient version that comes with OS X.  To do this, edit `~/.profile` and add:

```sh
export PATH=/Applications/Emacs.app/Contents/Resources/bin:"$PATH"
```

Restart your Terminal app to make the change take effect.  Sourcing the file is not enough, since OS X seems to perform indexing of location on program name.

*Ubuntu*

Install the `emacs24` package.

### Inconsolata Font

Install the Inconsolata font (this can be configured with the `my-default-font` setting).

*Windows and Mac OS X*

Download the [OpenType file](http://www.levien.com/type/myfonts/Inconsolata.otf) from the [Inconsolata website](http://www.levien.com/type/myfonts/inconsolata.html) and double-click on it.

*Ubuntu*

```sh
apt-get install fonts-inconsolata
```

### Download the `emacs-shared` code

```sh
cd ~/
git clone git://github.com/mwolson/emacs-shared.git
```

### Run boostrap.sh

On Windows, you'll want to open Git Bash and run the command from there.

```sh
cd ~/emacs-shared
./bootstrap.sh
```

### Create a ~/.emacs.d/init.el file

```sh
mkdir -p ~/.emacs.d
cd ~/.emacs.d
emacs -q -nw init.el  # or other editing command
```

`init.el` contents, change these as appropriate:

```lisp
;; Configure emacs-shared
(setq my-changelog-address "you@example.com"
      my-email-address     "you@example.com"
      my-full-name         "Your Name"
      my-irc-handle        "your_irc_handle"
      my-emacs-path        "~/emacs-shared/")

;; Load shared init file
(load-file (concat my-emacs-path "init/shared-init.el"))
```

If you are new to Emacs and very proficient with `vi`, then add this to the end of `init.el` to install VIPER *(VI Plan for Emacs Rescue)*.  This will use vi-like keys.  If you're not a vi guru, then it's probably best to not do this.

```lisp
;; Enable Vi emulation
(require 'viper)
```

### Start Emacs

Updating
--------

To keep up-to-date on the latest shared-emacs changes, do:

```sh
cd ~/emacs-shared
git pull
./bootstrap.sh
```

To Do
-----

- Document each of the individual shared-emacs options in code and possibly on a wiki page
- List each Emacs add-on we use and/or bundle
- Finish uploading my scripts to manage Emacs as a daemon
- Include various Emacs registry hacks for Windows (like Edit with Emacs)
- Decide whether to document the `ALTERNATE_EDITOR` environment variable on Windows; may be useful for those who want
  to make view source or double-click on various file types open up in an existing Emacs frame
- Document the hack to make the Quick Launch icon work as expected on Windows
- Document how to remap Caps Lock to Control (recommended as it can be more ergonomic)
- Decide whether setting up MSYS on Windows gives us anything useful that Git does not include already

License
-------

Unless stated otherwise, the files contained in this repo may be used, distributed, and modified without restriction.

