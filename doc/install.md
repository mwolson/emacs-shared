Installing Emacs
================

## Install git

*Windows*

On Windows, the best way to do this is to install both [Git Extensions](https://gitextensions.github.io/) and [Git for Windows](https://git-scm.com/download/win). When prompted about SSH during installation, choosing PuTTY is recommended.

*macOS*

On macOS, the best way to do this is to install Xcode from App Store.

## (macOS only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier installation of other useful tools, though it's not a strict requirement.

## (Windows only) Install PuTTY

This is useful for doing git development, since Pageant can hold onto your git keys and auto-load them when Windows starts. If you've already installed PuTTY in the past, make sure that you have have at least version 0.62 installed, since earlier versions might fail in ways that are difficult to diagnose.

To load a key automatically, create or edit a shortcut for Pageant and add the path to the key file as a commandline argument. To start Pageant automatically when Windows starts:
* Click on the Windows Search bar, type "run", click on the "Run" program
* Type "shell:startup" and hit Enter
* Copy the Pageant shortcut that we modified earlier into this folder

## (Windows only) Install MSYS2

You'll need a working version of `make.exe` in order to complete the bootstrap script. Here's the recommended steps for that:
- Download the [MSYS2 installer](http://msys2.github.io/), choosing the one for `x86_64`
- Run the installer, choosing the default install location (or if you change it, replacing paths as appropriate below)
- Close the command prompt that came with the installer
- Use the Start Menu to search for and open the task `Edit the System Environment Variables`. It may pop behind any open windows - if so, bring it to the front. Click `Environment Variables`. In the `System variables` section, double-click on `Path`. Add entries for `C:\msys64\usr\bin` and `C:\msys64\mingw64\bin` (in that order) and move them to the very bottom of the list. Click `OK` until all of those windows close.
- Open an "MSYS2 MinGW 64-bit" window and run these commands:
```sh
pacman -Sy pacman
pacman -Syu
pacman -Su
pacman -S --needed mingw64/mingw-w64-x86_64-make mingw64/mingw-w64-x86_64-gnutls mingw64/mingw-w64-x86_64-aspell-en msys/man-db mingw-w64-x86_64-cmake gcc git libssh2-devel mingw-w64-x86_64-ninja mingw-w64-x86_64-ripgrep
```

## Install cmake

You'll need to [install cmake](https://cmake.org/) in order to support fast execution of Magit. On Mac, install the `cmake` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install fd

[fd](https://github.com/sharkdp/fd/releases) is helpful for reducing time for projectile project management in some cases. On Mac, install the `fd` package from homebrew. For Windows, download the `x86_64-msvc` package from the latest release on the GitHub page and copy the binaries to the `c:\msys64\usr\bin` folder.

## Install ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest project search command available. On Mac, install the `ripgrep` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install Emacs

The recommended version is Emacs 27.2. The recommended installers for each OS are:

*Windows*

- [Download and install emacs](https://ftp.gnu.org/gnu/emacs/windows/emacs-27/emacs-27.2-x86_64-installer.exe).
- When done, you should verify that a file named `C:\Program Files\Emacs\x86_64\bin\runemacs.exe` exists.
- If you change the location, you may want to update the `my-system-paths` option later.

*macOS*

 - Download the non-Spacemacs build from [the Mitsuharu Yamamoto Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases/tag/emacs-27.2-mac-8.2).
 - Open the zip file and drag the Emacs icon over to the Applications folder.

*Ubuntu*

Install the `emacs27` package if it's available. If it's not available, [use a PPA](http://ubuntuhandbook.org/index.php/2020/09/install-emacs-27-1-ppa-ubuntu-20-04/).

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

## Run bootstrap.sh

On Windows:
* You will need to do the "Set up PATH" step first, otherwise it will fail.
* You will want to open an "MSYS2 MinGW 64-bit" window (not Git Bash) and run the commands from there.

Commands to run regardless of OS:

```sh
cd ~/emacs-shared
./bootstrap.sh
```

## Set up PATH

*macOS*

You'll want to make sure that your path includes the correct version of Emacs and some helper scripts, ahead of the ancient version that comes with macOS. To do this, edit `~/.profile` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal app to make the change take effect. Sourcing the file is not enough, since macOS seems to perform indexing of location on program name.

To verify your work, run `emacs --version` and make sure it shows the version number you'd expect.

*Linux*

You'll want to make sure that your path includes the correct version of Emacs and some helper scripts. To do this, edit `~/.bashrc` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal program to make the change take effect, or source the file.

To verify your work, run `emacs --version` and make sure it shows the version number you'd expect.

*Windows*

(Note: you'll need to do this ahead of the bootstrap.sh step, otherwise it will fail)

- Adjust some environment variables so that Emacs can be started successfully
  - Use the Start Menu to search for and open the task `Edit the System Environment Variables`. It may pop behind any open windows - if so, bring it to the front. Click `Environment Variables`.
  - Note: If any of the below variables aren't present, click on "Add" to add them
  - In User Variables, Inspect `HOME` and make sure it points to something like `C:\Users\You`.
  - In System Variables, double-click `Path` and make sure an entry for `%USERPROFILE%\emacs-shared\bin` is present at the very top.
  - In System Variables, double-click `Path` and make sure an entry for `C:\Program Files\Emacs\x86_64\bin` is present at the end.
  - If you have chosen to use PuTTY, then make sure that the `GIT_SSH` variable is set to `C:\Program Files\PuTTY\plink.exe` (or wherever PuTTY is installed).
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
(setq my-email-address "you@example.com")
(setq my-full-name     "Your Name")
(setq my-emacs-path    "~/emacs-shared/")

;; Load shared init file
(load-file (concat my-emacs-path "init/shared-init.el"))
```

## Start Emacs

*Windows*

Open `Start Menu -> Emacs`. This should point to the file `C:\Program Files\Emacs\x86_64\bin\runemacs.exe`.

*Windows Taskbar*

To pin Emacs to the Taskbar / Quick Launch bar and have it behave correctly:
- First pin it
- Then right-click its Taskbar button, right-click "Emacs", click "Properties"
- Change "C:\Program Files\Emacs\x86_64\bin\emacs.exe" to "C:\Program Files\Emacs\x86_64\bin\runemacs.exe"

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

Install Aspell and an Aspell dictionary for your language if you want to support spell-checking. We've already done this for Windows in the `Install MSYS2` section.

## (Windows only) Install Git manpages

The Windows installer doesn't include manpages. If you want them (and they're readable with this Emacs configuration by doing <kbd>M-x man</kbd>) then follow these steps:

- Clone the https://github.com/gitster/git-manpages repo somewhere
- (Optional) Run `git log` and find the commit that most closely matches your version of git; then switch to it with `git checkout`
- Move that "git-manpages" folder to "C:\Program Files\Git\", and rename it to "man", so the full path looks like "C:\Program Files\Git\man". That folder should contain directories like "man7" if you did it right.

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

## (macOS only) Enable autorepeat in browsers

Run this in a Terminal window:

```sh
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
```

Then restart any open applications to apply the change to them.

## Install grip or marked for markdown preview

By default [marked](https://marked.js.org/) is used to preview markdown pages.

Another option with slightly better output (though not apprioriate for private data) is [grip](https://github.com/joeyespo/grip). This will send the content to Github's API for rendering. Customize the Emacs `markdown-command` option if you would like to use `grip`.

Updating
========

To keep up-to-date on the latest `emacs-shared` changes, do:

```sh
cd ~/emacs-shared
git pull
./bootstrap.sh
```

---

[Back to README.md](../README.md#documentation)
