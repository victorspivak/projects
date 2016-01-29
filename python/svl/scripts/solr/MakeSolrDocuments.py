# !/usr/bin/python3

from svl.scripts.solr.SolrDataClasses import *
from svl.commons.RandomText import *
from svl.commons.Performance import *
from svl.scripts.solr.SolrCommand import *
from collections import Counter
from os.path import expanduser
import random
import pickle


class Library:
    def __init__(self, max_users_groups_count):
        self.books = []
        self.combined_books = []
        self.books_integer = []
        self.combined_books_integer = []
        self.id_to_integer = dict()
        self.used_users_groups = Counter()
        self.max_users_groups_count = max_users_groups_count
        self.groups = list(set([random_id('00Gx0', 15) for group_index in range(max_users_groups_count)]))
        self.users = list(set([random_id('005x0', 15) for user_index in range(max_users_groups_count)]))
        self.used_words = get_used_words()
        self.max_vd_length = 0
        self.max_user_vd_length = 0
        last_int = self.convert_id_to_integer(self.groups, self.id_to_integer, 0)
        self.convert_id_to_integer(self.users, self.id_to_integer, last_int)

    def add_books(self, count, init_words, use_first_group_user, min_count, max_count):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []
        for index in range(count):
            self.books.extend(self.make_book_vd(init_words, self.make_group_user_list(init_vd, min_count, max_count)))
        self.used_words = get_used_words()

    def convert_other_modes(self):
        self.combined_books = []
        self.books_integer = []
        self.combined_books_integer = []

        for book, desc in zip(*[iter(self.books)] * 2):
            self.combined_books.append(BookVdCombined(book, desc))
            vdi = self.convert_id_list_to_integer(desc.vd, self.id_to_integer)

            vd_integer = VisibilityDescriptorInteger(desc.id, vdi)
            self.books_integer.extend([book, vd_integer])
            self.combined_books_integer.append(BookVdCombinedInteger(book, vd_integer))

    @staticmethod
    def convert_id_to_integer(ids, id_map, start):
        for _id in ids:
            start += 1
            id_map[_id] = start

        return start

    @staticmethod
    def convert_id_list_to_integer(ids, id_map):
        return list([str(id_map[_id]) for _id in ids])

    @staticmethod
    def make_book_vd(init_words, vd):
        book_id = random_id('001x0', 15)
        title = random_text(1, 5)
        author = random_text(1, 3)
        body = random_text(10, 100) + ' ' + init_words
        vd_id = "vd-" + book_id
        return [Book(book_id, title, author, body, vd_id), VisibilityDescriptor(vd_id, vd)]

    def make_group_user_list(self, init_vd, min_count, max_count):
        g_count = random.randint(min_count, max_count)
        u_count = random.randint(min_count, max_count)
        vd = set(init_vd)
        self.make_random_item(vd, g_count, self.groups)
        self.make_random_item(vd, g_count + u_count, self.users)
        self.max_vd_length = max(self.max_vd_length, len(vd))
        return list(vd)

    def make_random_item(self, vd, count, items):
        while len(vd) < count:
            item = random.choice(items)
            vd.add(item)
            self.used_users_groups[item] += 1

    def make_used_group_user_list(self, use_first_group_user, min_count, max_count):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []
        count = min(random.randint(min_count, max_count), len(top_used_users_groups))
        vd = set(init_vd)
        while len(vd) < count:
            item = random.choice(top_used_users_groups)
            vd.add(item)

        self.max_user_vd_length = max(self.max_user_vd_length, len(vd))
        return list(vd)

    def get_used_users_groups(self):
        return self.used_users_groups

    def get_books(self):
        return self.books

    def get_combined_books(self):
        return self.combined_books

    def get_books_integer(self):
        return self.books_integer

    def get_combined_books_integer(self):
        return self.combined_books_integer

    def get_used_words(self):
        return self.used_words

    def dump_stats(self):
        print('Library: %d docs max VD length %d  max user VD length %d  used groups/users %d' %
              (len(self.books), self.max_vd_length, self.max_user_vd_length, len(self.used_users_groups)))


def populate_library():
    library.add_books(150000, '', True, 1, 10)
    library.add_books(10000, '', True, 10, 25)
    library.add_books(1000, '', True, 25, 100)
    library.add_books(100, '', True, 100, 1000)
    library.add_books(50, '', True, 1000, 2500)
    library.add_books(10, 'FindMeToken', True, 10, 25)
    with open(filename, 'wb') as f:
        pickle.dump(library, f)


def load_library():
    with open(filename, 'rb') as f:
        l = pickle.load(f)
    return l


rebuild_library = False

search_host = 'localhost:1234'
solr_core1 = SolrCommands(search_host, 'Core1', False, False)
solr_core2 = SolrCommands(search_host, 'Core2', False, False)
solr_core3 = SolrCommands(search_host, 'Core3', False, False)
solr_core4 = SolrCommands(search_host, 'Core4', False, False)

filename = '%s/tmp/library.dmp' % expanduser("~")

if rebuild_library:
    library = Library(10000)
    populate_library()
    solr_core1.cleanup_core()
    timing("Indexing %f", lambda: solr_core1.index_solr(library.get_books()))

    library.convert_other_modes()
    solr_core2.cleanup_core()
    timing("Combined Indexing %f", lambda: solr_core2.index_solr(library.get_combined_books()))

    solr_core3.cleanup_core()
    timing("Combined Indexing %f", lambda: solr_core3.index_solr(library.get_books_integer()))

    solr_core4.cleanup_core()
    timing("Combined Indexing %f", lambda: solr_core4.index_solr(library.get_combined_books_integer()))

else:
    library = load_library()

used_words = library.get_used_words()
most_used_words = list(key for key, value in used_words.most_common(100))
top_used_users_groups = list(key for key, value in library.get_used_users_groups().most_common(20000))


# print(used_words.most_common(100))
# print(library.groups[0])


def query_all_modes(tokens, user_vd, dump_query=False):
    join = '{!join from=id to=vdid}'
    vd_integers = library.convert_id_list_to_integer(user_vd, library.id_to_integer)
    sharing_constrain = 'vd:(%s)' % ' '.join(user_vd)
    sharing_constrain_integer = 'vdi:(%s)' % ' '.join(vd_integers)
    fields = 'id, title'

    query1 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain, fields)
    query2 = 'q=%s&fq=%s%s&fl=%s' % (tokens, '', sharing_constrain, fields)
    query3 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain_integer, fields)
    query4 = 'q=%s&fq=%s%s&fl=%s' % (tokens, '', sharing_constrain_integer, fields)

    if dump_query:
        print('Jquery: ' + query1)
        print('Cquery: ' + query2)
        print('JIquery: ' + query3)
        print('CIquery: ' + query4)

    timing('J query %f Result Count %d  VD length: ' + str(len(user_vd)),
           lambda: solr_core1.query_solr(query1, dump_query),
           lambda m, t, r: m % (t, r['numFound']))
    timing('C query %f Result Count %d  VD length: ' + str(len(user_vd)),
           lambda: solr_core2.query_solr(query2, dump_query),
           lambda m, t, r: m % (t, r['numFound']))
    timing('JIquery %f Result Count %d  VD length: ' + str(len(user_vd)),
           lambda: solr_core3.query_solr(query3, dump_query),
           lambda m, t, r: m % (t, r['numFound']))
    timing('CIquery %f Result Count %d  VD length: ' + str(len(user_vd)),
           lambda: solr_core4.query_solr(query4, dump_query),
           lambda m, t, r: m % (t, r['numFound']))
    print('-' * 100)


search_term = random.choice(most_used_words)
print('Search term: %s %d' % (search_term, used_words[search_term]))

query_all_modes('*%s*' % search_term,
                library.make_used_group_user_list(True, 10, 30), True)

query_all_modes('*FindMeToken* *%s*' % search_term,
                library.make_used_group_user_list(True, 10, 30), True)

print('=' * 100)

for i in range(20):
    vd = library.make_used_group_user_list(True, 10, 3000)
    search_tokens = '*%s %s %s %s %s*' % (random.choice(most_used_words), random.choice(most_used_words),
                                          random.choice(most_used_words), random.choice(most_used_words),
                                          random.choice(most_used_words))
    query_all_modes(search_tokens, vd)

library.dump_stats()
