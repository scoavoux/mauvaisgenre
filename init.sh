#!/bin/bash
chown -R ${USERNAME}:${GROUPNAME} ${HOME}
cd ~/work/mauvaisgenre
R -f init.R
