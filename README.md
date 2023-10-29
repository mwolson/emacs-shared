# Shared Emacs settings

This is an Emacs starter kit with:

- **Modern editor features**: Autocomplete, entire project search and replace, find definition
- **Fast project search** thanks to integration with [ripgrep](https://github.com/BurntSushi/ripgrep)
- **Cross-platform support**: Tested on Linux, Windows, and macOS
- **Fullscreen by default**, with a gap left for the Dock on macOS
- **Dark color theme**
- Extensive **setup instructions**
- **Splash screen** appears while starting Emacs, and startup is optimally fast
- **Manuals** are pre-built and show up when you do `C-h i`

Differences from other Emacs starter kits:

- Principle of least surprise is a design goal
- Augments your existing `~/.emacs.d/init.el` file instead of replacing it
- Can be personalized with `setq` statements

## Documentation

* [Installation Guide](doc/install.md#installing-emacs)
* [Feature Tour](doc/tips.md#emacs-feature-tour)
* [Customization Guide](doc/customize.md#customizing-emacs)

## Screenshots

`git` integration:

![Magit Screenshot](img/magit.png?raw=true)

Interactive search:

![Magit Screenshot](img/swiper.png?raw=true)

Entire project search:

![Magit Screenshot](img/ripgrep.png?raw=true)

## License

Unless stated otherwise, the files contained in this repo may be used, distributed, and modified without restriction.

This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
