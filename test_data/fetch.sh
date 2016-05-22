#!/bin/bash

for i in {0..28} ; do node --harmony hypelm-test_data.js $i >> data.json ; done

cat > ../test/TestData.elm << EOF
module TestData exposing (testData)

import List

testData : List (String, List String)
testData =
EOF

echo -n "  [" >> ../test/TestData.elm

first=1
while read line ; do
  if [ "$first" == "1" ] ; then
    echo "$line" | sed 's/\("[^"]\+"\):\s*\(\[[^]]\+\]\),\?/(\1,\2),/g' \
                 | sed 's/^.\(.*\)..$/\1/' \
                 | sed 's/,(/\n  , (/g' \
                 | sed 's/^(/ (/g' \
                 >> ../test/TestData.elm
  else
    echo "$line" | sed 's/\("[^"]\+"\):\s*\(\[[^]]\+\]\),\?/(\1,\2),/g' \
                 | sed 's/^.\(.*\)..$/,\1/' \
                 | sed 's/,(/\n  , (/g' \
                 | sed 's/^(/    (/g' \
                 >> ../test/TestData.elm
  fi

  first=0
done < data.json

echo "  ]" >> ../test/TestData.elm
