#!/bin/sh

configPath="$HOME/.config/wotd"

if echo $PATH | grep -qm 1 "\\(^\|:\\)$HOME\/.local\/bin\\($\|:\\)"
then
	path="$HOME/.local/bin"
else
	path="/usr/local/bin"
fi

if ! [ -d $path ]; then mkdir -p $path; fi
if ! [ -d $configPath ]; then mkdir -p $configPath; fi

ghc wotd.hs &&
cp wotd $path &&
cp dictionary.txt $configPath

