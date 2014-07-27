hbase:
    ./hbase/bin/start-hbase.sh  --> start
    ./hbase/bin/stop-hbase.sh   --> stop
    ./hbase/bin/hbase shell     --> run shell

To run Solr:
    copy solr-template/solr directory to ~/data/solr
    in terminal window change current folder to /opt/solr/example
    java -Dsolr.solr.home=/home/victor/data/solr -jar start.jar
    http://localhost:8983/solr
    query: Amount_d:[100 TO 1000]

curl http://localhost:8180/types/Claim | python -mjson.tool

curl http://localhost:8180/objects/Claim/ClaimID-00000174 | python -mjson.tool
