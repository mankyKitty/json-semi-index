#! /usr/bin/env bash
ghcid -c 'cabal new-repl' --test 'Lib.canHazClose' --warnings
