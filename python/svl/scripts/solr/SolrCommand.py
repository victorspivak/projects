import http.client
import json


class SolrCommands:
    def __init__(self, host, dump_responses=False, dump_results=False):
        self.host = host
        self.dump_responses = dump_responses
        self.dump_results = dump_results
        self.connection = http.client.HTTPConnection(host)

    @staticmethod
    def make_path(core):
        return '/solr/%s/' % core

    def index_solr(self, core, books):
        chunk_size = 10000
        for chunk in [(books[i:i+chunk_size]) for i in range(0, len(books), chunk_size)]:
            self.index_chunk(core, chunk)

    def index_chunk(self, core, chunk):
        chunk_json = json.dumps([ob.__dict__ for ob in chunk])
        headers = {'Content-type': 'application/json'}
        self.connection.request('POST', self.make_path(core) + 'update?commit=true', chunk_json, headers)
        response = self.connection.getresponse().read().decode()
        if self.dump_responses:
            print(response)

    def query_solr(self, core, query, dump_results=False):
        headers = {'Content-type': 'application/x-www-form-urlencoded'}
        self.connection.request('POST', self.make_path(core) + 'query', query, headers)
        decoded = self.connection.getresponse().read().decode()
        json_resp = json.loads(decoded)
        result = json_resp['response']
        if self.dump_results or dump_results:
            print('Query Result: %s ' % result['numFound'])
            for doc in result['docs']:
                print('\t %s --> %s' % (doc['id'], doc['title']))
        return result

    def cleanup_core(self, core):
        headers = {'Content-type': 'text/xml'}
        command = '<delete><query>*:*</query></delete>'
        self.connection.request('POST', self.make_path(core) + 'update?commit=true', command, headers)
        response = self.connection.getresponse().read().decode()
        if self.dump_responses:
            print(response)
