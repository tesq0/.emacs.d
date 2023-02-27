#!/bin/sh

nix build github:uthar/dev#emacs
nix-env -i ./result/
rm result
