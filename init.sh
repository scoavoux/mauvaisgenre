#!/bin/bash
# Exemple : https://git.drees.fr/drees_code_public/ressources/tutos/-/blob/diffusion/contenu/init.sh
# On enregistre tous les logs dans log.out pour pouvoir déboguer
exec 3>&1 4>&2
trap 'exec 2>&4 1>&3' 0 1 2 3
exec 1>log.out 2>&1

# Install libraries for python/spacyr
apt update
apt install libncurses-dev libreadline-dev libffi-dev

# Install rust and xan
curl https://sh.rustup.rs -sSf | sh -s -- -y
source "$HOME/.cargo/env"
cargo install xan

# Go into
cd ~/work/mauvaisgenre
R -f init.R

mkdir data/temp
mkdir output
mkdir output/omni1
mkdir output/omni2
# Access to files created by this script
chown -R ${USERNAME}:${GROUPNAME} ${HOME}
