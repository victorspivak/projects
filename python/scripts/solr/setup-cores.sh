#!/usr/bin/env bash
SOLR_HOME=/home/victor/tools/solr/solr

function setup_core () {
    d="$SOLR_HOME/server/solr/$Core"
    conf="$SOLR_HOME/server/solr/$Core/conf"
    echo "Setting: $d"
    if [ ! -d "$d" ]; then
        echo -e "\tmake $Core"
        mkdir "$d"
    elif [ -d "$conf" ]; then
        echo -e "\tremoving $conf"
        rm -rf "$conf"
    fi
    echo -e "\tcopy data/solr/conf to $conf"
    cp -r data/solr/conf "$d"
}

if [ -z "$SOLR_HOME" ]; then
    echo "The SOLR_HOME variable is required"
    exit -1
fi

cores=(Core1 Core2 Core3 Core4)
for Core in "${cores[@]}"; do
    setup_core
done



