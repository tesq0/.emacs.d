#!/bin/sh

nix build ./packages-flake
mkdir -p vendor/nix
cp -r result/* vendor/nix/
rm result


