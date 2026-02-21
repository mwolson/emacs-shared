# Installing Emacs

## Install git

_Windows_

On Windows, the best way to do this is to install
[Git for Windows](https://git-scm.com/download/win). Enabling cache during
installation is recommended for a massive speedup.

_macOS_

On macOS, the best way to do this is to install Xcode from App Store.

## (macOS only) Install Homebrew

You may want to pick up [Homebrew](http://mxcl.github.io/homebrew/) for easier
installation of other useful tools, though it's not a strict requirement.

## (Windows only) Install chocolatey

It's recommended to do an
[individual install of Chocolatey](https://chocolatey.org/install#individual) to
make it easier to install several other dependencies later on.

## (Windows only) Install MSYS2

You'll need a working version of `make.exe` in order to complete the bootstrap
script. Here's the recommended steps for that:

- Download the [MSYS2 installer](https://www.msys2.org/#installation), choosing
  the one for `x86_64`
- Run the installer, choosing the default install location (or if you change it,
  replacing paths as appropriate below)
- Close the command prompt that came with the installer
- Use the Start Menu to search for and open the task
  `Edit the System Environment Variables`. It may pop behind any open windows -
  if so, bring it to the front. Click `Environment Variables`. In the
  `System variables` section, double-click on `Path`. Add entries for
  `C:\msys64\usr\bin` and `C:\msys64\ucrt64\bin` (in that order) and move them
  to the very bottom of the list. Click `OK` until all of those windows close.
- Open an "MSYS2 MinGW 64-bit" window and run these commands:

```sh
pacman -Sy pacman
pacman -Syu
pacman -Su
pacman -S --needed base-devel libssh2-devel msys/man-db xmlto
pacman -S --needed mingw-w64-ucrt-x86_64-{asciidoc,aspell-en,clang-tools-extra,cmake,fd,gcc,gnutls,go,libgccjit,make,ninja,ripgrep,rust,ty}
```

- Make sure you do not have git installed through MSYS2 (we want it to come from
  Git For Windows instead so we get caching) by running this and ignoring any
  "target not found" errors:

```sh
pacman -R git
```

## (Windows only) Install Visual Studio Build tools

In an Administrator PowerShell, run:

```sh
winget install --id Microsoft.VisualStudio.2022.BuildTools -e --source winget --override "--add Microsoft.VisualStudio.Component.Windows11SDK.22621 --add Microsoft.VisualStudio.Workload.VCTools --add Microsoft.VisualStudio.Component.VC.Runtimes.x86.x64.Spectre --add Microsoft.VisualStudio.Component.VC.ATL.Spectre --add Microsoft.VisualStudio.Component.VC.ATLMFC.Spectre"
```

Note: This is taken from the [VSCode build instructions](https://github.com/microsoft/vscode/wiki/How-to-Contribute).

## (Arch Linux only) Install utilities

It's assumed that you have installed `paru` already. If you haven't yet, do
this:

```sh
sudo pacman -Sy --needed base-devel
mkdir -p ~/pkgbuilds
cd ~/pkgbuilds
git clone https://aur.archlinux.org/paru-bin.git
cd paru-bin
makepkg -si
```

Minimal install:

```sh
paru -S aspell-en base-devel clang cmake fd gnutls gopls make man-db man-pages \
    mise ninja openssh python ripgrep rust-analyzer ty zls
```

For a full install, in addition to the above also run:

```sh
paru -S clojure clojure-lsp-bin jdtls leiningen omnisharp-roslyn-bin zprint-bin
```

## Install clangd

You'll need to [install clangd](https://clangd.llvm.org/installation) in order
to support language server features for C/C++. On Mac, install the `llvm`
package from homebrew. For Windows, we've already done this for Windows in the
`Install MSYS2` section.

## (Optional) Install clojure, clojure-lsp, cljstyle, and leiningen

You'll need to [install clojure](https://clojure.org/guides/install_clojure),
[install clojure-lsp](https://clojure-lsp.io/installation/),
[install leiningen](https://wiki.leiningen.org/Packaging), and
[install zprint](https://github.com/kkinnear/zprint/blob/main/README.md#get-zprint)
in order to support Clojure. On Mac:

```sh
brew install --cask temurin@21
brew install clojure/tools/clojure leiningen
brew install clojure-lsp/brew/clojure-lsp-native
brew install --cask zprint
# do this to avoid failures when starting CIDER for the first time
mkdir -p ~/.lein
```

Note that on macOS: You'll need to attempt to run
`/opt/homebrew/bin/zprint --version` and then go into System Settings -> Privacy
& Security -> Security -> Allow, run it one more time, and click Run Anyway. You
may also need to restart any open terminals so that the version of zprint in
`/usr/bin` doesn't shadow the one in `/opt/homebrew/bin`.

You'll also want to create a `~/.zprintrc` file with these contents:

```clojure
;; -*- clojure -*-
{:search-config? true, :style [:how-to-ns]}
```

## Install fd

[fd](https://github.com/sharkdp/fd/releases) is helpful for reducing time for
locating files in project directories in some cases. On Mac, install the `fd`
package from homebrew. For Windows, we've already done this for Windows in the
`Install MSYS2` section.

## Install gopls

You'll need to
[install gopls](https://github.com/golang/tools/blob/master/gopls/README.md#installation)
in order to support language server features for C/C++. On Mac, install the `go`
and `gopls` packages from homebrew. For Windows, we've already done this for
Windows in the `Install MSYS2` section.

## Install jdtls (optional)

You'll need to install both a v21 or higher JDK/JRE and a JD-TLS in order to
support language server features for Java. On Mac, install the `jdtls` package
from homebrew. Since this requires a JDK/JRE, and is extremely slow, it can be
skipped if you don't need these features for Java code.

Note that the first time you visit a Java file in a large project, JD-TLS might
take a minute to initialize.

## Install ninja

You'll need to [install ninja](https://ninja-build.org/) in order to support
fast execution of Magit. On Mac, install the `ninja` package from homebrew. For
Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install node.js

It's recommended to [install mise](https://mise.jdx.dev/getting-started.html),
configure your shell to work with it, restart any open terminals, and then
install the latest LTS node.js version and pnpm with:

```sh
mise use -g node@lts pnpm@latest
mise settings add idiomatic_version_file_enable_tools node
```

## Install Omnisharp (optional)

Since this requires installing a .NET SDK, it's optional.

_Windows_

Run `choco install omnisharp`

_Mac OS_

Run:

```sh
brew install omnisharp/omnisharp-roslyn/omnisharp-mono
sudo mkdir -p /usr/local/opt
sudo ln -s /opt/homebrew/opt/omnisharp-mono /usr/local/opt
```

Confirm that you can run `omnisharp --help` and get a help menu. If you get
"assembly not found", make sure to create `/usr/local/opt` and set up the
symlink as above.

_Arch Linux_

Run `paru -S omnisharp-roslyn-bin`

## Install ripgrep

[ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest project search
command available. On Mac, install the `ripgrep` package from homebrew. For
Windows, we've already done this for Windows in the `Install MSYS2` section.

## Install rust-analyzer

You'll need to
[install rust analyzer](https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary)
in order to support language server features for Rust. On Mac, install the
`rust` and `rust-analyzer` packages from homebrew. For Windows, we've already
done this for Windows in the `Install MSYS2` section.

## Install ty

You'll need to
[install ty](https://docs.astral.sh/ty/installation/#installing-with-the-standalone-installer)
in order to support language server features for Python. On Mac, install the
`ty` package from homebrew.

## Install zls

You'll need to install ZLS in order to support language server features for Zig.
On Mac, install the `zls` package from homebrew.

## Install Emacs

The recommended version is Emacs 30.2. The recommended ways to install Emacs for
each OS are as follows.

### Install Emacs on Windows

- [Download and install emacs](https://ftp.gnu.org/gnu/emacs/windows/emacs-30/emacs-30.2-installer.exe).
- When done, you should verify that a file named
  `C:\Program Files\Emacs\emacs-30.2\bin\runemacs.exe` exists.
- If you change the location, you may want to update the `my-system-paths`
  option later.
- Adjust some environment variables so that Emacs can be started successfully
  - Use the Start Menu to search for and open the task
    `Edit the System Environment Variables`. It may pop behind any open
    windows - if so, bring it to the front. Click `Environment Variables`.
  - Note: If any of the below variables aren't present, click on "Add" to add
    them
  - In User Variables, Inspect `HOME` and make sure it points to something like
    `C:\Users\You`.
  - In System Variables, double-click `Path` and make sure an entry for
    `%USERPROFILE%\emacs-shared\bin` is present at the very top.
  - In System Variables, double-click `Path` and make sure an entry for
    `C:\Program Files\Emacs\emacs-30.2\bin` is present at the end. If you see an
    entry for a different version of Emacs, change it to have this content
    instead and remove any duplicates of it.
  - Click OK
  - Relaunch any open Git Bash or MSYS2 windows

If upgrading:

- Unpin any pinned Emacs icons
- Change any existing desktop icons to point to
  `C:\Program Files\Emacs\emacs-30.2\bin\runemacs.exe`.

### Install Emacs on macOS

We'll install the
[emacs-plus cask from Homebrew](https://github.com/d12frosted/homebrew-emacs-plus):

```sh
brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app
```

If you get an error when starting Emacs like
`"dyld[48068]: Library not loaded: /opt/homebrew/opt/tree-sitter/lib/libtree-sitter.0.23.dylib"`,
when you may need to reinstall the XCode Commandline Tools, then check System
Update for any updates to it, and then reboot to make the changes take effect.
After that, try installing `emacs-plus` again per the above instructions.

You'll also probably want to go into System Settings -> Privacy & Security ->
Full Disk Acccess and add Emacs, so that it can open files from any location.

### Install Emacs on Arch Linux

- If you want Emacs to be hyper-fast and stable but require a bit of extra
  setup, install the `emacs-lucid` AUR package.
- If you want Emacs to be reasonably fast, install the `emacs` package.
- If you are on Wayland (the default window system) and want better font hinting
  at the cost of much slower editing, install the `emacs-wayland` package.

_Emacs Lucid Setup_

If you're using `emacs-lucid`, you'll probably want
[Xresource settings](https://www.gnu.org/software/emacs/manual/html_node/emacs/Lucid-Resources.html)
like this for dark background menus on a 4k hi-res display, stored in a file
called `~/.Xresources`:

```sh
Emacs.pane.menubar.buttonForeground: white
Emacs.pane.menubar.cursor: left_ptr
Emacs.pane.menubar.horizontalSpacing: 12
Emacs.pane.menubar.verticalSpacing: 8
```

To apply the changes, run `xrdb -merge ~/.Xresources` and then restart Emacs. On
most window environments, the `~/.Xresources` file should be automatically read
after logging in.

### Install Emacs on Ubuntu

We'll build from scratch, since no PPAs have consistently had latest version
releases in a timely way.

- Uninstall any versions of Emacs that were previously installed by `apt` or
  `dpkg`.
- Edit `/etc/apt/sources.list` and uncomment all `#deb-src` lines that match
  `deb` lines and run `sudo apt update`.
- Follow these instructions, derived from
  [this post](https://practical.li/blog/posts/build-emacs-28-on-ubuntu/):

```sh
sudo apt build-dep -y emacs
sudo apt install libjansson4 libjansson-dev gnutls-bin
cd build/emacs
curl -o - https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.gz | tar -xzf -
cd emacs-30.2
./autogen.sh
./configure
make -j$(proc)
sudo make install
```

## Fonts

Typically we install the Fira Code font. This can be configured with the
`my-default-font` setting.

_macOS_

Download [Fira Code](https://github.com/tonsky/FiraCode#download--install), open
the zip, open the `ttf` folder, select all fonts, control-click and choose Open
to install them.

_Windows_

Download [Fira Code](https://github.com/tonsky/FiraCode#download--install) and
follow the instructions.

_Arch Linux_

```sh
paru -S ttf-fira-code
```

_Ubuntu_

```sh
apt-get install fonts-firacode
```

## Download the emacs-shared code

```sh
cd ~/
git clone https://github.com/mwolson/emacs-shared.git
```

## Set up PATH

_macOS_

You'll want to make sure that your path includes the correct version of Emacs
and some helper scripts, ahead of the ancient version that comes with macOS. To
do this, edit `~/.profile` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal app to make the change take effect. Sourcing the file is
not enough, since macOS seems to perform indexing of location on program name.

_Linux_

You'll want to make sure that your path includes the correct version of Emacs
and some helper scripts. To do this, edit `~/.bashrc` and add:

```sh
export PATH=~/emacs-shared/bin:~/bin:"$PATH"
```

Restart your Terminal program to make the change take effect, or source the
file.

_Windows_

We already took care of this earlier before setting up MSYS2.

## Run bootstrap.sh

On Windows:

- You will need to do the "Set up PATH" step first, otherwise it will fail.
- For Git Bash and MSYS2 support, you may need to add something like this to
  `.bashrc` and restart any open Terminal windows:

```sh
if uname | grep "MINGW64_NT" > /dev/null 2>&1; then
    # for MSYS2
    export PATH="~/emacs-shared/bin":"/c/Windows/System32/OpenSSH":"/c/Program Files/Emacs/emacs-30.2/bin":/ucrt64/bin:"/c/Program Files/Git/bin":"$PATH"
elif uname | grep "MSYS_NT" > /dev/null 2>&1; then
    # for Git Bash
    export PATH="/c/Windows/System32/OpenSSH":"$PATH"
fi
```

- Check `.bashrc` and update any `PATH` entries which have a different version
  of Emacs to instead point to `/c/Program Files/Emacs/emacs-30.2/bin` and
  restart Terminal.
- You will want to open an "MSYS2 MinGW 64-bit" window (not Git Bash) and run
  the commands from there.

Commands to run regardless of OS:

```sh
cd ~/emacs-shared
./bootstrap.sh
```

To verify that `PATH` now has the correct entries, run `emacs --version` and
make sure it shows the version number you'd expect.

## Create a ~/.emacs.d/early-init.el file

```sh
mkdir -p ~/.emacs.d
cd ~/.emacs.d
emacs -q early-init.el  # or other editing command
```

`early-init.el` contents, change these as appropriate:

```elisp
;; Configure emacs-shared
(setq my-emacs-path    "~/emacs-shared/")
(setq my-email-address "you@example.com")
(setq my-full-name     "Your Name")

;; Note - change the following to allow mise to automatically enable specific
;; versions of tools like nodejs from .tool-versions or mise.toml files in project
;; directories.
;;
;; Default value: disable it for all directories:
;; (setq my-mise-exclude-file-regexps '(".*"))
;;
;; Insecure: enable it for all directories:
;; (setq my-mise-exclude-file-regexps '())
;;
;; Better: disable it for a directory tree that has downloaded content:
;; (setq my-mise-exclude-file-regexps '("devel/github"))

;; Load shared early init file
(setq load-prefer-newer t)
(load (concat my-emacs-path "init/early-shared-init") nil nil nil t)
```

## Create a ~/.emacs.d/init.el file

```sh
mkdir -p ~/.emacs.d
cd ~/.emacs.d
emacs -q init.el  # or other editing command
```

`init.el` contents, change these as appropriate:

```elisp
;; Authinfo location for any AI passwords
(setopt auth-sources '("~/.emacs.d/.authinfo"))

;; Load shared init file
(load (concat my-emacs-path "init/shared-init") nil nil nil t)
```

## Start Emacs

_Windows_

Open `Start Menu -> Emacs`. This should point to the file
`C:\Program Files\Emacs\emacs-30.2\bin\runemacs.exe`.

_Windows Taskbar_

To pin Emacs to the Taskbar / Quick Launch bar and have it behave correctly:

- If an older version is already there, unpin Emacs from the launcher menu
- Run Emacs from Start menu or a desktop shortcut
- Pin it
- Then right-click its Taskbar button, right-click "Emacs", click "Properties"
- Change "C:\Program Files\Emacs\emacs-30.2\bin\emacs.exe" to "C:\Program
  Files\Emacs\emacs-30.2\bin\runemacs.exe"

_macOS_

Open `Applications -> Emacs`.

_Linux_

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

Install Aspell and an Aspell dictionary for your language if you want to support
spell-checking. We've already done this for Windows in the `Install MSYS2`
section.

## (macOS only) Install docker manpages

This isn't really related to Emacs, but if you're using Docker on macOS, it
might not install the manpages. Here's how to do that:

```sh
git clone https://github.com/docker/docker.git
cd docker
make manpages
cd man
cp -R man* /usr/local/share/man/
```

## (macOS only) Update man database

To force XCode manpages to be generated (may require granting your terminal
program the ability to modify apps), along with the homebrew manpages, run the
following:

```sh
sudo /usr/libexec/makewhatis /Applications/Xcode.app/Contents/Developer/usr/share/man
sudo /usr/libexec/makewhatis /opt/homebrew/share/man
```

## (macOS only) Enable keyboard autorepeat

Run this in a Terminal window:

```sh
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false
```

Then restart any open applications to apply the change to them.

## Install grip or marked for markdown preview

By default [marked](https://marked.js.org/) is used to preview markdown pages.

Another option with slightly better output (though not apprioriate for private
data) is [grip](https://github.com/joeyespo/grip). This will send the content to
Github's API for rendering. Customize the Emacs `markdown-command` option if you
would like to use `grip`.

# Updating

To keep up-to-date on the latest `emacs-shared` changes, do:

```sh
cd ~/emacs-shared
git config submodule.recurse false
git pull
./bootstrap.sh
```

# Maintenance

The following are tasks used to maintain the repo, and not required for a
typical installation.

## Set up hooks and tools for this repo

To initialize hooks, install the latest mise dependencies, and install the
latest pnpm dependencies:

```sh
pnpm run init
```

To install the latest mise dependencies and install the latest pnpm
dependencies:

```sh
pnpm run up
```

## Build Git manpages

The Windows installer for Git doesn't include manpages. Some prebuilt ones will
be made available after running `bootstrap.sh`.

If you want them to rebuild them to reflect the latest git-for-windows changes,
do this:

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

Ensure that the `shallow = true` line is present for that submodule in
`.gitmodules` and sort the config.

Update `bootstrap.sh` to initialize the submodule and `init/shared-init.el` to
add the load path.

## Update Emacs Source Code

Do this:

```sh
pushd extra/emacs
git fetch --depth 1 origin tag emacs-30.2
git checkout emacs-30.2
popd
```

---

[Back to README.md](../README.md#documentation)
