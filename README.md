# Josh Proehl's MiscDotFiles
This is how I bootstrap my favored environment and configurations onto a new machine and keep all my various machines in sync.
These configs are designed to work on both Linux (ArchLinux is my preference) and OSX.
This repo is expected to be cloned to ~/.miscdotfiles

## Prereqs

Needs to be installed before this can be bootstrapped:
* Git - To get this, plus submodules.
* Ruby (LibYAML) - For Rake
* Python2 - (For YouCompleteMe bootstraping)
* ZSH - Because it's the shell
* CMake - For YouCompleteMe's install script

For (Arch) linux:
* Xmonad and xmonad contrib - For XMonad
* base-devel - The entire group is needed for building various things

Do do this all on Arch: `pacman -Sy git ruby python zsh xmonad xmonad-contrib base-devel`


## Post Bootstrap
Things to do beyond the bootstrap file. Perhaps a bit too complex to let bootstrap do it?

* -- DEPRECATED, Plugin currently not used:  Run .vim/bundle/YouCompleteMe/install.py  (As of 2016/11 on Arch Linux script must be altered to call python2 out of env to run)
* -- DEPRECATED, Plugin currently not used   Run .vim/bundle/vimproc/make
* `asdf install` to install global versions of asdf-managed tools
* `pip install neovim` to get Neovim's Phython integration dependency

## System Changes
Arch Linux:
  Add the line "export _JAVA_AWT_WM_NONREPARENTING=1" to /etc/profile.d/jre.sh to ensure java apps all launch with that environment variable.
  `sudo mv 01-keyboard-layout.conf /etc/X11/xorg.conf.d/`
  Set primary monitor (/etc/X11/10-monitor.conf, or xrandr)

## Arch Packages
Other things Arch expects to have set up:

## Set up Aura AUR helper.
git clone https://aur.archlinux.org/aura-bin.git
cd aura-bin
makepkg -si

Regular packages
* feh
* xorg-xsetroot
* redshift
* terminus-font
* ttf-dejavu, ttf-droid, ttf-anonymous-pro
* dzen2
* conky
* sysstat (System status used by dzen-conky)
* bc (Command line calculator used by dzen/conky)
* task  (Vim config wants it)
* terminator (Xmonad configured to use as terminal)
* dmenu (app launch script/yegonesh needs)

From AUR
* rbenv  (This caused errors because of the .rbenv directory linked from the bootstrap)
* trayer-srg
* compton
* copyq
* dropbox
* yegonesh
* python2-deepin-ui (Still using this?)
* caffeine-ng-git (Switched to -git because regular version eats all your ram.)
* ttf-iosevk
* alacritty-git (Using instead of Terminator in some cases)

## Other random things
#### Beets
In order to get beets set up completely on arch linux with the dagr/nott config file we need the following:
`sudo aura -Sy beets python-flask python-mpd2 python-pyacoustid python-requests`
and
`sudo aura -Ay python-discogs-client`

## TODO:
* Script to bootsrap the manual-install fonts?
* Dweep font requires changing arch configs in /etc/font/conf.d/ to allow bitmap fonts
