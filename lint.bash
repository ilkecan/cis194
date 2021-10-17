#!/usr/bin/env bash
hlint .
brittany --write-mode=inplace *.hs
