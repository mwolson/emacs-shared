Shared Emacs settings
=====================

This is a collection of Emacs Lisp add-ons and an Emacs init file with the following features.

- **Cross-platform support**: decent level of feature parity between Linux, Windows, and Mac OS X
- **Fullscreen by default** on Windows and OS X, with code to account for dock size of 64 pixels on OS X (options:
  my-frame-pad-width, my-frame-pad-height, and my-frame-maximize-p); frame width and height is customizable if
  you don't like fullscreen
- **"afternoon" dark color theme**
- Can be personalized with a single `setq` statement; there are many options
- Supports list of features to enable
- Principle of least surprise is a design goal
- Same great-looking **Inconsolata font** on all platforms
- **Splash screen** appears while starting a non-daemon GUI, for appearance of speed :)
- Extensively-documented **setup instructions**
- **Manuals** are pre-built and show up when you do `C-h i`
- Supplements your existing `~/.emacs.d/init.el` file instead of replacing it

Installing
----------

### Install git

On Windows, the best way to do this is with [Git Extensions](https://code.google.com/p/gitextensions/).  When prompted about SSH during installation, choosing PuTTY is recommended.

On Mac OS X, the best way to do this is to install Xcode from App Store, and then install the Command Line Tools for Xcode using the Downloads preferences pane within Xcode 4.6.1 and later.

### (Windows only) Install Git manpages

The Windows installer doesn't include manpages.  If you want them (and they're readable with this Emacs configuration by doing `M-x man`) then follow these steps:

- Find and download the manpages for your version of git from [the Git download list](https://code.google.com/p/git-core/downloads/list)
- Extract them to "C:\Program Files (x86)\Git\share\man", which will probably be a new folder.  That folder should contain directories like "man7" after extraction if you did it right.

### (Mac OS X only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier installation of other useful tools, though it's not a strict requirement.

### (Optional, Mac OS X only) Install docker manpages

This isn't really related to Emacs, but if you're using Docker on OS X, it might not install the manpages. Here's how to do that:

``` sh
git clone https://github.com/docker/docker.git
cd docker
make manpages
cd man
cp -R man* /usr/local/share/man/
```

### (Mac OS X only) Update man database

After installing new packages, the `M-x man` command might not list the new manpages for those packages, because the `whatis` DB used by `man` gets updated weekly via a cron job. Further, any manpages for programs that are part of the XCode Commandline Tools will never get installed because the cron job inexplicably excludes them. To make force them to be generated, run the following:

``` sh
sudo /etc/periodic/weekly/320.whatis
sudo /usr/libexec/makewhatis /Applications/Xcode.app/Contents/Developer/usr/share/man
```

### (Windows only) Install PuTTY

This is useful for doing git development, since Pageant can hold onto your git keys and auto-load them when Windows starts.  If you've already installed PuTTY in the past, make sure that you have have at least version 0.62 installed, since earlier versions might fail in ways that are difficult to diagnose.

### (Windows only) Install MinGW

If you want to be able to byte-compile Emacs Lisp libraries, you'll need a working version of "make.exe".  Here's the recommended steps for that:
- Download the [MinGW installer](http://www.mingw.org/)
- Run the installer
- Run the mingw32-get.exe file (it should be in `C:\MinGW\bin`, and also the Start Menu)
- Click on "All Packages" on the left
- Find mingw32-make, the "bin" version, right-click on it, "Mark for Installation"
- On the Installation menu, click on Apply Changes
- When this is done, bring up a Git Bash prompt and run:
- `cd /c/MinGW/bin`
- `ln -s /c/MinGW/bin/mingw32-make.exe make.exe`
- When you change PATH in the later instructions, add this to the end of PATH as well: `C:\MinGW\bin`

### (Optional) Install Aspell

Install Aspell and an Aspell dictionary for your language for spell-checking.  This is more likely to be useful on Windows, which does not come with an ispell variant.

### (Optional) Install ripgrep

If you want to quickly search for patterns within multiple files, install [ripgrep](https://github.com/BurntSushi/ripgrep).

### Install Emacs

The recommended version is Emacs 25.1.  The recommended installers for each OS are:

*Windows*

- Download `emacs-25.1-x86_64-w64-mingw32.zip` from [ftp.gnu.org](http://ftp.gnu.org/gnu/emacs/windows/).
- Unzip to `C:\Program Files (x86)` and then rename `emacs-25.1` to `Emacs`.  When done, you should verify that a file named `C:\Program Files (x86)\Emacs\bin\runemacs.exe` exists.
- If you change the location, you may want to update the `my-system-paths` option later.

*Mac OS X*

 - Download the "official-icon" build from [the Mituhara Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases/tag/emacs-25.1-mac-6.1).
 - Open the zip file and drag the Emacs icon over to the Applications folder.

*Ubuntu*

Install the `emacs25` package if it's available. If it's not available, [build it manually](http://ubuntuhandbook.org/index.php/2016/09/install-gnu-emacs-25-1-in-ubuntu-16-04/).

### Inconsolata Font

Install the Inconsolata font (this can be configured with the `my-default-font` setting).

*Windows and Mac OS X*

Download the [OpenType file](http://www.levien.com/type/myfonts/Inconsolata.otf) from the [Inconsolata website](http://www.levien.com/type/myfonts/inconsolata.html) and double-click on it.

*Ubuntu*

```sh
apt-get install fonts-inconsolata
```

### Download the emacs-shared code

```sh
cd ~/
git clone https://github.com/mwolson/emacs-shared.git
```

### Run boostrap.sh

On Windows, you'll want to open Git Bash and run the command from there.

(Note: On Windows, you'll need to do the "Set up PATH" step first, otherwise it will fail)

```sh
cd ~/emacs-shared
./bootstrap.sh
```

### Set up PATH

*Mac*

You'll want to make sure that your path includes the correct version of Emacs and some helper scripts, ahead of the ancient version that comes with OS X.  To do this, edit `~/.profile` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal app to make the change take effect.  Sourcing the file is not enough, since OS X seems to perform indexing of location on program name.

To verify your work, run `emacs --version` and make sure it shows the version number you'd expect.

*Linux*

You'll want to make sure that your path includes the correct version of Emacs and some helper scripts.  To do this, edit `~/.bashrc` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal program to make the change take effect, or source the file.

To verify your work, run `emacs --version` and make sure it shows the version number you'd expect.

*Windows*

(Note: for now you'll need to do this ahead of the bootstrap.sh step, otherwise it will fail)

- Adjust some environment variables so that Emacs can be started successfully
  - Open `Control Panel -> System -> Advanced System Settings (on left) -> Environment Variables`.
  - Note: If any of the below variables aren't present, click on "Add" to add them
  - In User Variables, Inspect `HOME` and make sure it points to something like `C:\Users\You`.
  - In one of User Variables or System Variables (depending on whether you want the change to apply to all users or just you), edit `PATH` and make sure `C:\Program Files (x86)\Emacs\bin` is there, with a semicolon separating it from the other entries.
  - Don't forget to add `C:\MinGW\bin` as well, if you're using MinGW
- If you have chosen to use PuTTY, then make sure that the `GIT_SSH` variable is set to `C:\Program Files (x86)\PuTTY\plink.exe` (or wherever PuTTY is installed).
  - Click OK
  - Relaunch any open Git Bash windows

### Read up a bit on Emacs usage

At this point, it's recommended to [familiarize yourself with how to open and save files using Emacs](http://mally.stanford.edu/~sr/computing/emacs.html).

### Create a ~/.emacs.d/init.el file

```sh
mkdir -p ~/.emacs.d
cd ~/.emacs.d
emacs -q init.el  # or other editing command
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

### Start Emacs

*Windows*

Open `Start Menu -> Gnu Emacs -> Emacs`.  This should point to the file `C:\Program Files (x86)\Emacs\bin\runemacs.exe`.

*Windows Taskbar*

To pin Emacs to the Taskbar / Quick Launch bar and have it behave correctly:
- First pin it
- Then right-click its Taskbar button, right-click "Emacs", click "Properties"
- Change "C:\Program Files (x86)\Emacs\bin\emacs.exe" to "C:\Program Files (x86)\Emacs\bin\runemacs.exe"

*Mac OS X*

Open `Applications -> Emacs`.

*Linux*

GUI frame:

```sh
emacs -n -c
```

Console frame:

```sh
emacs -nw
```

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
- Document how to remap Caps Lock to Control (recommended as it can be more ergonomic)

License
-------

Unless stated otherwise, the files contained in this repo may be used, distributed, and modified without restriction.

This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
