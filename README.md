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


## Post Bootstrap
Things to do beyond the bootstrap file. Perhaps a bit too complex to let bootstrap do it?

* Run .vim/bundle/YouCompleteMe/install.py  (As of 2016/11 on Arch Linux script must be altered to call python2 out of env to run)
* Run .vim/bundle/vimproc/make

## System Changes
Arch Linux:
  Add the line "export _JAVA_AWT_WM_NONREPARENTING=1" to /etc/profile.d/jre.sh to ensure java apps all launch with that environment variable.
  `sudo mv 01-keyboard-layout.conf /etc/X11/xorg.conf.d/`
  Set primary monitor (/etc/X11/10-monitor.conf, or xrandr)

## Arch Packages
Other things Arch expects to have set up:

Regular packages
* nitrogen
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

From AUR
* trayer-srg
* compton
* copyq
* dropbox
* yegonesh
* python2-deepin-ui
* caffeine-ng-git (Switched to -git because regular version eats all your ram.)

## TODO:
* Script to bootsrap the manual-install fonts?
* Dweep font requires changing arch configs in /etc/font/conf.d/ to allow bitmap fonts
