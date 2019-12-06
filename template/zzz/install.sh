#!/usr/bin/env bash

sudo dnf install i3 i3lock feh network-manager-applet xbacklight zsh -y

#chsh -s /usr/bin/bash
#chsh -s /usr/bin/zsh

cp -R wallpaper.jpg ~/Pictures/wallpaper.jpg

cp -R .config/i3/config ~/.config/i3/config
cp -R .config/i3status/config ~/.config/i3status/config

cp -R .fonts ~/.fonts

cp -R .vimrc ~/.vimrc
vim +PluginInstall +qall

cp -R .tmux.conf ~/.tmux.conf

cp -R .gitconfig ~/.gitconfig
echo "Make sure to add your email/name to ~/.gitconfig"

sudo cp -R etc/X11/xorg.conf /etc/X11/xorg.conf
