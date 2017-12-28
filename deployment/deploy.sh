#!/usr/bin/env sh
nix-shell ./nixdeploy/shell.nix --command "\
    nixdeploy deploy ./deploy.nix \
    --argstr \"host:${1}\" \
    "
