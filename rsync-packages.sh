#!/usr/bin/env sh
rsync -a --delete ~/.emacs.d/elpa/ ~/.emacs.d/"elpa-$1/" --filter='-p *.elc' --filter='-p __pycache__/'
