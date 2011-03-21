#!/bin/bash
set -o errexit -o nounset -o pipefail

debs=( libev-dev libgnutls-dev )

aptitude install --assume-yes "${debs[@]}"
cabal install snap-server -flibev -fgnutls

