#!/bin/bash

TIMEOUT=150

BASEDIR=`dirname $0`
TOPDIR=$BASEDIR/../..
MAPS=`cat $BASEDIR/maps`
LAST_COMMIT_FILE=~/.icfp2012.last_commit

WIKIDIR=~/icfp2012.wiki
WIKIPAGE=$WIKIDIR/Scores.md

short_commit() {
  echo $current_commit | head -c10
}

add_header() {
  perl -i -pe "m@>([^<]*)</th></tr>@;s@>([^<]*)</th></tr>@>\$1</th><th>$1</th></tr>@ unless \$1 eq \"$1\";" $WIKIPAGE
}

record_score() {
  map=$1
  score="$2"
  pushd $WIKIDIR
  git pull --rebase
  
  perl -i -pe "s@($map<.*)</tr>@\$1<td>$score</td></tr>@" $WIKIPAGE
  git add $WIKIPAGE
  git commit -a -m "Score for $map of $current_commit"
  git push
  popd
}

last_commit=`cat $LAST_COMMIT_FILE`
while true; do
  git pull
  current_commit=`git rev-parse HEAD`
  if [ "$last_commit" != "$current_commit" ]; then
    echo "New commit, checking build..."
    if make -C $TOPDIR -q; then
      echo "Nothing changed."
    else
      echo "New build, building..."
      make -C $TOPDIR
      add_header `short_commit`
      for map in $MAPS; do
        score=`$BASEDIR/ranking.rb -t $TIMEOUT -f -m $map | perl -pe 's/^.*? //'`
        echo $score
        record_score $map "$score"
      done
    fi
  fi
  last_commit=$current_commit
  echo $current_commit > $LAST_COMMIT_FILE
  sleep 10
done
