# !/usr/bin/python3

from svl.scripts.solr.SolrDataClasses import *
from svl.commons.RandomText import *
from svl.commons.Performance import *
from svl.scripts.solr.SolrCommand import *
from collections import Counter
from collections import defaultdict
from os.path import expanduser
import random
import pickle


# noinspection PyUnusedLocal
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

    def add_books(self, count, init_words, use_first_group_user, min_vd_size, max_vd_size):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []
        for index in range(count):
            self.add_book(init_words, init_vd, max_vd_size, min_vd_size)

        self.used_words = get_used_words()

    def add_book(self, init_words, init_vd, max_vd_size, min_vd_size):
        book_id = random_id('001x0', 15)
        title = random_text(1, 5)
        author = random_text(1, 3)
        body = random_text(10, 100) + ' ' + init_words
        vd_id = "vd-" + book_id
        vd = self.make_group_user_list(init_vd, min_vd_size, max_vd_size)
        book = Book(book_id, title, author, body, vd_id)
        desc = VisibilityDescriptor(vd_id, vd)
        self.books.extend([book, desc])
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

    def make_used_group_user_list(self, use_first_group_user, min_count, max_count, seed_vd=list()):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []
        init_vd.extend(seed_vd)
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


def get_library():
    if rebuild_library:
        cleanup_all_indexes()
        lib = Library(10000)
        populate_library(lib)
        timing("J  Indexing %f", lambda: solr_core.index_solr(core1, lib.get_books()))
        timing("JF Indexing %f", lambda: solr_core.index_solr(core11, lib.get_books()))
        timing("C  Indexing %f", lambda: solr_core.index_solr(core2, lib.get_combined_books()))
        timing("JI Indexing %f", lambda: solr_core.index_solr(core3, lib.get_books_integer()))
        timing("CI Indexing %f", lambda: solr_core.index_solr(core4, lib.get_combined_books_integer()))
    else:
        lib = load_library()

    return lib


def load_library():
    with open(filename, 'rb') as f:
        lib = pickle.load(f)
    return lib


def cleanup_all_indexes():
    for core in cores:
        solr_core.cleanup_core(core)


def populate_library(lib):
    lib.add_books(150000, '', True, 1, 10)
    lib.add_books(10000, '', True, 10, 25)
    lib.add_books(1000, '', True, 25, 100)
    lib.add_books(100, '', True, 100, 1000)
    lib.add_books(50, '', True, 1000, 2500)
    lib.add_books(10, 'FindMeToken', True, 10, 25)
    with open(filename, 'wb') as f:
        pickle.dump(lib, f)


def random_queries(dump_query=False):
    if random_query_count > 0:
        print('=' * 100)

        for i in range(random_query_count):
            user_vd = library.make_used_group_user_list(True, 10, 3000)
            search_tokens = '*%s %s %s %s %s*' % (random.choice(most_used_words), random.choice(most_used_words),
                                                  random.choice(most_used_words), random.choice(most_used_words),
                                                  random.choice(most_used_words))

            query_all_modes(search_tokens, user_vd, False, dump_query)


def special_queries():
    if run_special_queries:
        search_term = random.choice(most_used_words)
        print('Search term: %s %d' % (search_term, used_words[search_term]))

        query_all_modes('*%s*' % search_term,
                        library.make_used_group_user_list(True, 10, 30), False, True)

        query_all_modes('*FindMeToken* *%s*' % search_term,
                        library.make_used_group_user_list(True, 10, 30), False, True)


def query(query_id, core, q, user_vd, dump_query, log_stats):
    timing_result = timing(query_id + ' query %f Result Count %d  VD length: ' + str(len(user_vd)),
                           lambda: solr_core.query_solr(core, q, dump_query),
                           lambda m, t, r: m % (t, r['numFound']))

    if log_stats:
        query_run_stats[query_id][len(user_vd)].append(timing_result[1])


def query_all_modes(tokens, user_vd, log_stats, dump_query=False):
    join = '{!join from=id to=vdid}'
    vd_integers = library.convert_id_list_to_integer(user_vd, library.id_to_integer)
    sharing_constrain = 'vd:(%s)' % ' '.join(user_vd)
    sharing_constrain_integer = 'vdi:(%s)' % ' '.join(vd_integers)
    fields = 'id, title'

    query1 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain, fields)
    query2 = 'q=%s&fq=%s&fl=%s' % (tokens, sharing_constrain, fields)
    query3 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain_integer, fields)
    query4 = 'q=%s&fq=%s&fl=%s' % (tokens, sharing_constrain_integer, fields)

    if dump_query:
        print('Jquery: ' + query1)
        print('Cquery: ' + query2)
        print('JIquery: ' + query3)
        print('CIquery: ' + query4)

    query('J ', core1, query1, user_vd, dump_query, log_stats)
    query('JF', core11, query1, user_vd, dump_query, log_stats)
    query('C ', core2, query2, user_vd, dump_query, log_stats)
    query('JI', core3, query3, user_vd, dump_query, log_stats)
    query('CI', core4, query4, user_vd, dump_query, log_stats)

    print('-' * 100)


def query_misc_user_vd(dump_query=False):
    search_tokens = '*%s %s %s %s %s*' % (random.choice(most_used_words), random.choice(most_used_words),
                                          random.choice(most_used_words), random.choice(most_used_words),
                                          random.choice(most_used_words))
    if query_misc_user_vd_count > 0:
        print('=' * 100)
        for i in range(query_misc_user_vd_count):
            user_vd = []
            for vd_count in user_vd_counts:
                user_vd = library.make_used_group_user_list(False, vd_count, vd_count, user_vd)
                query_all_modes(search_tokens, user_vd, True, dump_query)


search_host = 'localhost:1234'
filename = '%s/tmp/library.dmp' % expanduser("~")
solr_core = SolrCommands(search_host, False, False)

core1 = 'Core1'
core2 = 'Core2'
core3 = 'Core3'
core4 = 'Core4'
core11 = 'Core11'
cores = [core1, core2, core3, core4, core11]
user_vd_counts = [10, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000, 4000]

rebuild_library = False

run_special_queries = False
random_query_count = 0
query_misc_user_vd_count = 10

query_run_stats = defaultdict(lambda: defaultdict(list))
library = get_library()

used_words = library.get_used_words()
most_used_words = list(key for key, value in used_words.most_common(100))
top_used_users_groups = list(key for key, value in library.get_used_users_groups().most_common(20000))

special_queries()
random_queries()
query_misc_user_vd()

all_query_ids = list(query_run_stats.keys())
all_query_ids.sort()
for key1 in all_query_ids:
    query_user_id_stats = list(query_run_stats[key1].keys())
    query_user_id_stats.sort()

    for key2 in query_user_id_stats:
        stats = ['%6.3f' % e for e in query_run_stats[key1][key2]]
        print("%s %5d -> %s" % (key1, key2, str(stats)))

library.dump_stats()
