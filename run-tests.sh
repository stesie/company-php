#! /bin/sh
set -x
RV=0

cd tests
for file in *.el; do
  emacs -batch  -l package -f package-initialize -l ert  -L "$PWD"/.. -L "$PWD" -l "$file"  -f ert-run-tests-batch-and-exit || RV=1
done

exit $RV
