#!/bin/bash

ignores=(`basename $0` .git settings .emacs.d)
diff_opts="-r -x cache -x *.elc"

function my_diff() {
  file=$1

  if ! diff $diff_opts ~/$file $file > /dev/null; then
    echo '--- '$file
    diff $diff_opts ~/$file $file
    echo; echo; echo
  fi
}

for i in `ls -A`; do
  if [[ ! ${ignores[*]} =~ $i ]]; then
    my_diff $i
  fi
done

for i in `ls .emacs.d`; do
  my_diff ".emacs.d/$i"
done
