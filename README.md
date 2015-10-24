# Josh Proehl's MiscDotFiles
This is how I bootstrap my favored environment and configurations onto a new machine and keep all my various machines in sync.
These configs are designed to work on both Linux (ArchLinux is my preference) and OSX.
This repo is expected to be cloned to ~/.miscdotfiles

## Prereqs

Needs to be installed before this can be bootstrapped:
* Git - To get this, plus submodules.
* Ruby (LibYAML) - For Rake
* ZSH - Because it's the shell
* CMake - For YouCompleteMe's install script

For (Arch) linux:
* Xmonad and xmonad contrib - For XMonad
* base-devel - The entire group is needed for building various things


## Post Bootstrap
Things to do beyond the bootstrap file. Perhaps a bit too complex to let bootstrap do it?

* Run .vim/bundle/YouCompleteMe/install.sh


## Arch Packages
Other things Arch expects to have set up:

Regular packages
* nitrogen
* xorg-xsetroot
* redshift
* terminus-font
* ttf-dejavu, ttf-droid, ttf-anonymous-pro

From AUR
* trayer-srg
* compton
* copyq
* dropbox
* yeganesh
* python2-deepin-ui
* caffeine-ng-git (Switched to -git because regular version eats all your ram.)

## TODO:
* Script to bootsrap the manual-install fonts?
