Installing Emacs
================

## Install git

*Windows*

On Windows, the best way to do this is with [Git Extensions](https://gitextensions.github.io/).  When prompted about SSH during installation, choosing PuTTY is recommended.

*macOS*

On macOS, the best way to do this is to install Xcode from App Store, and then install the Command Line Tools for Xcode using the Downloads preferences pane within Xcode 4.6.1 and later.

## (macOS only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier installation of other useful tools, though it's not a strict requirement.

## (Windows only) Install PuTTY

This is useful for doing git development, since Pageant can hold onto your git keys and auto-load them when Windows starts.  If you've already installed PuTTY in the past, make sure that you have have at least version 0.62 installed, since earlier versions might fail in ways that are difficult to diagnose.

## (Windows only) Install MinGW

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

## Install ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest project search command available. If you're using Windows, drop the `rg.exe` executable into `C:\MinGW\bin`.

## Install Emacs

The recommended version is Emacs 25.1.  The recommended installers for each OS are:

*Windows*

- Download `emacs-25.1-x86_64-w64-mingw32.zip` from [ftp.gnu.org](http://ftp.gnu.org/gnu/emacs/windows/).
- Unzip to `C:\Program Files (x86)` and then rename `emacs-25.1` to `Emacs`.  When done, you should verify that a file named `C:\Program Files (x86)\Emacs\bin\runemacs.exe` exists.
- If you change the location, you may want to update the `my-system-paths` option later.

*macOS*

 - Download the "official-icon" build from [the Mitsuharu Yamamoto Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases/tag/emacs-25.1-mac-6.1).
 - Open the zip file and drag the Emacs icon over to the Applications folder.

*Ubuntu*

Install the `emacs25` package if it's available. If it's not available, [build it manually](http://ubuntuhandbook.org/index.php/2016/09/install-gnu-emacs-25-1-in-ubuntu-16-04/).

## Inconsolata Font

Install the Inconsolata font (this can be configured with the `my-default-font` setting).

*Windows and macOS*

Download the [OpenType file](http://www.levien.com/type/myfonts/Inconsolata.otf) from the [Inconsolata website](http://www.levien.com/type/myfonts/inconsolata.html) and double-click on it.

*Ubuntu*

```sh
apt-get install fonts-inconsolata
```

## Download the emacs-shared code

```sh
cd ~/
git clone https://github.com/mwolson/emacs-shared.git
```

## Run boostrap.sh

On Windows, you'll want to open Git Bash and run the command from there.

(Note: On Windows, you'll need to do the "Set up PATH" step first, otherwise it will fail)

```sh
cd ~/emacs-shared
./bootstrap.sh
```

## Set up PATH

*macOS*

You'll want to make sure that your path includes the correct version of Emacs and some helper scripts, ahead of the ancient version that comes with macOS.  To do this, edit `~/.profile` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal app to make the change take effect.  Sourcing the file is not enough, since macOS seems to perform indexing of location on program name.

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

## Create a ~/.emacs.d/init.el file

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

## Start Emacs

*Windows*

Open `Start Menu -> Gnu Emacs -> Emacs`.  This should point to the file `C:\Program Files (x86)\Emacs\bin\runemacs.exe`.

*Windows Taskbar*

To pin Emacs to the Taskbar / Quick Launch bar and have it behave correctly:
- First pin it
- Then right-click its Taskbar button, right-click "Emacs", click "Properties"
- Change "C:\Program Files (x86)\Emacs\bin\emacs.exe" to "C:\Program Files (x86)\Emacs\bin\runemacs.exe"

*macOS*

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

Extras
======

The following are optional steps.

## Install Aspell

Install Aspell and an Aspell dictionary for your language for spell-checking.  This is more likely to be useful on Windows, which does not come with an ispell variant.

## (Windows only) Install Git manpages

The Windows installer doesn't include manpages.  If you want them (and they're readable with this Emacs configuration by doing <kbd>M-x man</kbd>) then follow these steps:

- Find and download the manpages for your version of git from [the Git download list](https://code.google.com/p/git-core/downloads/list)
- Extract them to "C:\Program Files (x86)\Git\share\man", which will probably be a new folder.  That folder should contain directories like "man7" after extraction if you did it right.

## (macOS only) Install docker manpages

This isn't really related to Emacs, but if you're using Docker on macOS, it might not install the manpages. Here's how to do that:

``` sh
git clone https://github.com/docker/docker.git
cd docker
make manpages
cd man
cp -R man* /usr/local/share/man/
```

## (macOS only) Update man database

After installing new packages, the <kbd>M-x man</kbd> command might not list the new manpages for those packages, because the `whatis` DB used by `man` gets updated weekly via a cron job. Further, any manpages for programs that are part of the XCode Commandline Tools will never get installed because the cron job inexplicably excludes them. To make force them to be generated, run the following:

``` sh
sudo /etc/periodic/weekly/320.whatis
sudo /usr/libexec/makewhatis /Applications/Xcode.app/Contents/Developer/usr/share/man
```

Updating
========

To keep up-to-date on the latest `emacs-shared` changes, do:

```sh
cd ~/emacs-shared
git pull
./bootstrap.sh
```

---

[Back to README.md](https://github.com/mwolson/emacs-shared#documentation)
