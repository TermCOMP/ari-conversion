#!/bin/bash
for i in $(find $1 -name *.ari)
do
    echo $i
    stack exec trs-conversion -- --from ARI --to XTC "$i" > /tmp/ari/converted.xml
    xmllint --format /tmp/ari/converted.xml | sed '0,/<trs>$/d' > /tmp/ari/converted_pretty.xml
    old_name=$(echo $i | sed s/'\/repos\/TPDB-ARI\/'/'\/repos\/TPDB\/'/g | sed s/'.ari'$/'.xml'/g)
    xmllint --format "$old_name" | sed '0,/<trs>$/d' | sed 's/<strategy>.*<\/strategy>//g' | sed -z 's/<metainformation>\n.*\n.*<\/metainformation>//g' | sed -z 's/<startterm>\n.*\n.*<\/startterm>//g' > /tmp/ari/orig_pretty.xml
    diff -wB /tmp/ari/converted_pretty.xml /tmp/ari/orig_pretty.xml
done
