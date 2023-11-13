# Installing Emacs

## Install git

*Windows*

On Windows, the best way to do this is to install [Git for Windows](https://git-scm.com/download/win). Enabling cache during installation is recommended for a massive speedup.

*macOS*

On macOS, the best way to do this is to install Xcode from App Store.

## (macOS only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier installation of other useful tools, though it's not a strict requirement.

## (Windows only) Install chocolatey

It's recommended to do an [individual install of Chocolatey](https://chocolatey.org/install#individual) to make it easier to install several other dependencies later on.

## (Windows only) Install MSYS2

You'll need a working version of `make.exe` in order to complete the bootstrap script. Here's the recommended steps for that:
* Download the [MSYS2 installer](https://www.msys2.org/#installation), choosing the one for `x86_64`
* Run the installer, choosing the default install location (or if you change it, replacing paths as appropriate below)
* Close the command prompt that came with the installer
* Use the Start Menu to search for and open the task `Edit the System Environment Variables`. It may pop behind any open windows - if so, bring it to the front. Click `Environment Variables`. In the `System variables` section, double-click on `Path`. Add entries for `C:\msys64\usr\bin` and `C:\msys64\ucrt64\bin` (in that order) and move them to the very bottom of the list. Click `OK` until all of those windows close.
* Open an "MSYS2 MinGW 64-bit" window and run these commands:
```sh
pacman -Sy pacman
pacman -Syu
pacman -Su
pacman -S --needed base-devel libssh2-devel msys/man-db xmlto
pacman -S --needed mingw-w64-ucrt-x86_64-{asciidoc,aspell-en,clang-tools-extra,cmake,fd,gcc,gnutls,make,ninja,ripgrep,rust}
```
* Make sure you do not have git installed through MSYS2 (we want it to come from Git For Windows instead so we get caching) by running this and ignoring any "target not found" errors:
```sh
pacman -R git
```

## (Arch Linux only) Install utilities

```sh
sudo pacman -S aspell-en base-devel cmake fd gnutls libssh2-devel make man-db man-pages ninja openssh ripgrep
```

## Install clangd

You'll need to [install clangd](https://clangd.llvm.org/installation) in order to support language server features for C/C++. On Mac, install the `llvm` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install cmake

You'll need to [install cmake](https://cmake.org/) in order to support fast execution of Magit. On Mac, install the `cmake` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install fd

[fd](https://github.com/sharkdp/fd/releases) is helpful for reducing time for locating files in project directories in some cases. On Mac, install the `fd` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install ninja

You'll need to [install ninja](https://ninja-build.org/) in order to support fast execution of Magit. On Mac, install the `ninja` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install node.js

It's recommended to do a [manual install of fnm](https://github.com/Schniz/fnm#manually), configure your shell to work with it, restart any open terminals, and then install the latest LTS node.js version with:

```sh
fnm install --lts
fnm use <version> # replace <version> with the version number that got installed
```

## Install Omnisharp

*Windows*

Run `choco install omnisharp`

*Mac OS*

Run:

``` sh
brew install omnisharp/omnisharp-roslyn/omnisharp-mono
sudo mkdir -p /usr/local/opt
sudo ln -s /opt/homebrew/opt/omnisharp-mono /usr/local/opt
```

Confirm that you can run `omnisharp --help` and get a help menu. If you get "assembly not found", make sure to create `/usr/local/opt` and set up the symlink as above.

*Arch Linux*

Run `yay install omnisharp-roslyn`

## Install ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest project search command available. On Mac, install the `ripgrep` package from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install rust-analyzer

You'll need to [install rust analyzer](https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary) in order to support language server features for Rust. On Mac, install the `rust` and `rust-analyzer` packages from homebrew. For Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install Emacs

The recommended version is Emacs 29.1. The recommended ways to install Emacs for each OS are as follows.

### Install Emacs on Windows

- [Download and install emacs](https://ftp.gnu.org/gnu/emacs/windows/emacs-29/emacs-29.1_2-installer.exe).
- When done, you should verify that a file named `C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe` exists.
- If you change the location, you may want to update the `my-system-paths` option later.
- Adjust some environment variables so that Emacs can be started successfully
  - Use the Start Menu to search for and open the task `Edit the System Environment Variables`. It may pop behind any open windows - if so, bring it to the front. Click `Environment Variables`.
  - Note: If any of the below variables aren't present, click on "Add" to add them
  - In User Variables, Inspect `HOME` and make sure it points to something like `C:\Users\You`.
  - In System Variables, double-click `Path` and make sure an entry for `%USERPROFILE%\emacs-shared\bin` is present at the very top.
  - In System Variables, double-click `Path` and make sure an entry for `C:\Program Files\Emacs\emacs-29.1\bin` is present at the end. If you see an entry for a different version of Emacs, change it to have this content instead and remove any duplicates of it.
  - Click OK
  - Relaunch any open Git Bash or MSYS2 windows

If upgrading:
- Unpin any pinned Emacs icons
- Change any existing desktop icons to point to `C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe`.

### Install Emacs on macOS

- Download the non-Spacemacs build from [the Mitsuharu Yamamoto Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport/releases/tag/emacs-29.1-mac-10.0).
- Open the zip file and drag the Emacs icon over to the Applications folder.

### Install Emacs on Arch Linux

Install the `emacs` package.

### Install Emacs on Ubuntu

We'll build from scratch, since no PPAs have consistently had latest version releases in a timely way.

* Uninstall any versions of Emacs that were previously installed by `apt` or `dpkg`.
* Edit `/etc/apt/sources.list` and uncomment all `#deb-src` lines that match `deb` lines and run `sudo apt update`.
* Follow these instructions, derived from [this post](https://practical.li/blog/posts/build-emacs-28-on-ubuntu/):

```sh
sudo apt build-dep -y emacs
sudo apt install libjansson4 libjansson-dev gnutls-bin
cd build/emacs
curl -o - https://ftp.gnu.org/gnu/emacs/emacs-29.1.tar.gz | tar -xzf -
cd emacs-29.1
./autogen.sh
./configure
make -j$(proc)
sudo make install
```

## Fonts

Typically we install the Fira Code font. This can be configured with the `my-default-font` setting.

*macOS*

Download [Fira Code](https://github.com/tonsky/FiraCode#download--install), open the zip, open the `ttf` folder, select all fonts, control-click and choose Open to install them.

*Windows*

Download [Fira Code](https://github.com/tonsky/FiraCode#download--install) and follow the instructions.

*Arch Linux*

```sh
pacman -S ttf-fira-code
```

*Ubuntu*

```sh
apt-get install fonts-firacode
```

## Download the emacs-shared code

```sh
cd ~/
git clone https://github.com/mwolson/emacs-shared.git
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

We already took care of this earlier before setting up MSYS2.

## Run bootstrap.sh

On Windows:
* You will need to do the "Set up PATH" step first, otherwise it will fail.
* For Git Bash and MSYS2 support, you may need to add something like this to `.bashrc` and restart any open Terminal windows:
```sh
if uname | grep "MINGW64_NT" > /dev/null 2>&1; then
    # for MSYS2
    export PATH="~/emacs-shared/bin":"/c/Windows/System32/OpenSSH":"/c/Program Files/Emacs/emacs-29.1/bin":/ucrt64/bin:"/c/Program Files/Git/bin":"$PATH"
elif uname | grep "MSYS_NT" > /dev/null 2>&1; then
    # for Git Bash
    export PATH="/c/Windows/System32/OpenSSH":"$PATH"
fi
```
* Check `.bashrc` and update any `PATH` entries which have a different version of Emacs to instead point to `/c/Program Files/Emacs/emacs-29.1/bin` and restart Terminal.
* You will want to open an "MSYS2 MinGW 64-bit" window (not Git Bash) and run the commands from there.

Commands to run regardless of OS:

```sh
cd ~/emacs-shared
./bootstrap.sh
```

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

Open `Start Menu -> Emacs`. This should point to the file `C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe`.

*Windows Taskbar*

To pin Emacs to the Taskbar / Quick Launch bar and have it behave correctly:
- If an older version is already there, unpin Emacs from the launcher menu
- Run Emacs from Start menu or a desktop shortcut
- Pin it
- Then right-click its Taskbar button, right-click "Emacs", click "Properties"
- Change "C:\Program Files\Emacs\emacs-29.1\bin\emacs.exe" to "C:\Program Files\Emacs\emacs-29.1\bin\runemacs.exe"

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

# Extras

The following are optional steps.

## Install Aspell

Install Aspell and an Aspell dictionary for your language if you want to support spell-checking. We've already done this for Windows in the `Install MSYS2` section.

## (macOS only) Install docker manpages

This isn't really related to Emacs, but if you're using Docker on macOS, it might not install the manpages. Here's how to do that:

```sh
git clone https://github.com/docker/docker.git
cd docker
make manpages
cd man
cp -R man* /usr/local/share/man/
```

## (macOS only) Update man database

After installing new packages, the <kbd>M-x man</kbd> command might not list the new manpages for those packages, because the `whatis` DB used by `man` gets updated weekly via a cron job. Further, any manpages for programs that are part of the XCode Commandline Tools will never get installed because the cron job inexplicably excludes them. To make force them to be generated, run the following:

```sh
sudo /etc/periodic/weekly/320.whatis
sudo /usr/libexec/makewhatis /Applications/Xcode.app/Contents/Developer/usr/share/man
```

## (macOS only) Enable keyboard autorepeat

Run this in a Terminal window:

```sh
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
```

Then restart any open applications to apply the change to them.

## Install grip or marked for markdown preview

By default [marked](https://marked.js.org/) is used to preview markdown pages.

Another option with slightly better output (though not apprioriate for private data) is [grip](https://github.com/joeyespo/grip). This will send the content to Github's API for rendering. Customize the Emacs `markdown-command` option if you would like to use `grip`.

# Updating

To keep up-to-date on the latest `emacs-shared` changes, do:

```sh
cd ~/emacs-shared
git pull
./bootstrap.sh
```

# Maintenance

The following are tasks used to maintain the repo, and not required for a typical installation.

## Build Git manpages

The Windows installer for Git doesn't include manpages. Some prebuilt ones will be made available after running `bootstrap.sh`.

If you want them to rebuild them to reflect the latest git-for-windows changes, do this:

```sh
pushd extra/git
git pull
popd

BUILD_GIT_MANPAGES=y ./bootstrap.sh

pushd share/man
git add . ; git add -u .
git commit -m "Build for commit ..."
git push upstream main
popd
```

## Add New Submodules

Do this:

```sh
git submodule add --depth 1 https://github.com/path/to/module elisp/my-new-module
```

Ensure that the `shallow = true` line is present for that submodule in `.gitmodules` and sort the config.

Update `bootstrap.sh` to initialize the submodule and `init/shared-init.el` to add the load path.

## Update Emacs Source Code

Do this:

```sh
pushd extra/emacs
git fetch --depth 1 origin tag emacs-29.1
git checkout emacs-29.1
popd
```

---

[Back to README.md](../README.md#documentation)
