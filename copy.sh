#!/bin/bash

for i in `ls -A`; do
  if [[ $i != `basename $0` && $i != ".git" && $i != "settings" && $i != ".emacs.d" ]]; then
    echo $i
    diff -r ~/$i $i
  fi
done
