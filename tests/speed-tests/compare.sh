# Compare output of a run with output of other runs, ignoring times.

a=$1
sed "s/^Time:.*/Time:/" <$a >tmp.a
shift

while [ x$1 != x ]; do
  echo COMPARING $a WITH $1
  sed "s/^Time:.*/Time:/" <$1 >tmp.b
  diff -u tmp.a tmp.b
  shift
done

rm tmp.a tmp.b
