curl localhost:8080/person -i
curl localhost:8080/person -i -X POST --header "Content-Type:application/json" -d'{"firstName":"Victor","lastName":pivak","age":21}'
