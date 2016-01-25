import http.client
import json


class SolrCommands:
    def __init__(self, host, core, dump_responses=False, dump_results=False):
        self.host = host
        self.core = core
        self.path = '/solr/%s/' % self.core
        self.dump_responses = dump_responses
        self.dump_results = dump_results
        self.connection = http.client.HTTPConnection(host)

    def index_solr(self, books):
        library_json = json.dumps([ob.__dict__ for ob in books])
        headers = {'Content-type': 'application/json'}
        self.connection.request('POST', self.path + 'update?commit=true', library_json, headers)
        response = self.connection.getresponse().read().decode()
        if self.dump_responses:
            print(response)

    def query_solr(self, query):
        headers = {'Content-type': 'application/x-www-form-urlencoded'}
        self.connection.request('POST', self.path + 'query', query, headers)
        decoded = json.loads(self.connection.getresponse().read().decode())
        result = decoded['response']
        if self.dump_results:
            print('Query Result: %s ' % result['numFound'])
            for doc in result['docs']:
                print('\t %s --> %s' % (doc['id'], doc['title']))

    def cleanup_core(self):
        headers = {'Content-type': 'text/xml'}
        command = '<delete><query>*:*</query></delete>'
        self.connection.request('POST', self.path + 'update?commit=true', command, headers)
        response = self.connection.getresponse().read().decode()
        if self.dump_responses:
            print(response)
