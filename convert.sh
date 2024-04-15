#!/bin/bash
for i in $(find $1 -name *.xml)
do
    echo $i
    new_name=$(echo $i | sed s/'\/repos\/TPDB\/'/'\/repos\/TPDB-ARI\/'/g | sed s/'.xml'$/'.ari'/g)
    new_dir=$(dirname $new_name)
    mkdir -p $new_dir
    stack exec trs-conversion -- --from XTC --to ARI "$i" > $new_name;
done
