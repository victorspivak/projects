# !/usr/bin/python3

from svl.scripts.solr.SolrDataClasses import *
import json
import http.client


def make_book_vd(_id, title, author, body, vd):
    vdid = "vd-" + _id
    return [Book(_id, title, author, body, vdid), VisibilityDescriptor(vdid, vd)]


#http://stackoverflow.com/questions/4561113/python-list-conversion

library = []
library.extend(make_book_vd("id1", "Three Musketeers", "A Duma", "It is a book text. FindMeToken", ["G1", "G2", "G3", "U1"]))
library.extend(make_book_vd("id2", "20 years later", "A Duma", "It is a book text", ["G1", "G4", "G5", "G6", "U2"]))
library.extend(make_book_vd("id3", "10 years later", "A Duma", "It is a book text", ["G1", "G6", "G7", "U3"]))
library.extend(make_book_vd("id4", "War and Piece", "Tolstoj", "It is a book text. FindMeToken", ["G1", "G7", "G8", "U1", "U4"]))
library.extend(make_book_vd("id5", "Twelve chairs", "Il'f, Petrov", "It is a book text. FindMeToken",["G1", "G7", "G8", "U1", "U4"]))

connection = http.client.HTTPConnection("localhost:8983")
core = "Core1"

def index_solr(library):
    library_json = json.dumps([ob.__dict__ for ob in library])
    #print(library_json)
    headers = {'Content-type': 'application/json'}
    connection.request('POST', '/solr/{}/update?commit=true'.format(core), library_json, headers)
    response = connection.getresponse()
    print(response.read().decode())


def query_solr(query):
    headers = {'Content-type': 'application/x-www-form-urlencoded'}
    connection.request('POST', '/solr/{}/query'.format(core), query, headers)
    response = connection.getresponse()
    print(response.read().decode())

index_solr(library)
query_solr("q=*FindMeToken*&fq={!join from=id to=vdid}vd:(G1 g31 u11)&fl=id, title")
query_solr("q=*FindMeToken*&fq={!join from=id to=vdid}vd:(G8 g31 u11)&fl=id, title")
