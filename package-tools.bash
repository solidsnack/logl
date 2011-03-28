#!/bin/bash

function tasks {
  case "${1:-}" in
    extensions) find . -name '*.hs' -print0 |
                xargs -0 sed -rn '/^\{-# LANGUAGE/,/  #-\}/ {
                                    /^............ / { s/// ; p }
                                  }' | sort | uniq ;;
    *)          echo 'No such task.' 1>&2 ; exit 0 ;;
  esac
}

set -o errexit -o nounset -o pipefail
tasks "$@"

