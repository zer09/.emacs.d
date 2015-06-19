#!/usr/bin/env sh
rsync -a --delete ~/.emacs.d/elpa/ ~/.emacs.d/"elpa-$1/" --exclude='*.elc' --exclude='__pycache__/'
