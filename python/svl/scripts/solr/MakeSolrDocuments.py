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

        chunk_size = 100000
        for chunk in range(0, count, chunk_size):
            for index in range(chunk, min(count, chunk + chunk_size)):
                self.add_book(init_words, init_vd, max_vd_size, min_vd_size)

            print('Indexing chunk: %d documents' % len(self.combined_books))
            self.indexing()

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
        vdi = self.convert_id_list_to_integer(vd, self.id_to_integer)
        vd_integer = VisibilityDescriptorInteger(vd_id, vdi)
        self.books_integer.extend([book, vd_integer])
        self.combined_books_integer.append(BookVdCombinedInteger(book, vd_integer))

    def indexing(self):
        timing("J  Indexing %f", lambda: solr_core.index_solr(core1, self.books))
        timing("C  Indexing %f", lambda: solr_core.index_solr(core2, self.combined_books))
        timing("JI Indexing %f", lambda: solr_core.index_solr(core3, self.books_integer))
        timing("CI Indexing %f", lambda: solr_core.index_solr(core4, self.combined_books_integer))
        self.books = []
        self.combined_books = []
        self.books_integer = []
        self.combined_books_integer = []

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
    if full_run:
        lib.add_books(500000, '', True, 1, 10)
        lib.add_books(20000, '', True, 10, 25)
        lib.add_books(2000, '', True, 25, 100)
        lib.add_books(1000, '', True, 100, 1000)
        lib.add_books(500, '', True, 1000, 2500)
        lib.add_books(100, '', True, 2500, 5000)
        lib.add_books(10, 'FindMeToken', True, 10, 25)
    else:
        lib.add_books(1000, '', True, 1, 10)
        lib.add_books(10, 'FindMeToken', True, 10, 25)

    with open(filename, 'wb') as f:
        pickle.dump(lib, f)


def random_queries(dump_query=0):
    if random_query_count > 0:
        print('=' * 100)

        for i in range(random_query_count):
            user_vd = library.make_used_group_user_list(True, 10, 3000)
            search_tokens = random_most_used_words(library.get_used_words(), 100, 1, 5)

            query_all_models(search_tokens, user_vd, False, dump_query)


def special_queries():
    if run_special_queries:
        search_term = random_most_used_words(library.get_used_words(), 100, 1, 1)
        print('Search term: %s %d' % (search_term, used_words[search_term]))

        query_all_models('*%s*' % search_term,
                         library.make_used_group_user_list(False, 10, 30), False, 2)

        query_all_models('*FindMeToken* *%s*' % search_term,
                         library.make_used_group_user_list(True, 10, 30), False, 2)


def query_timing_formatter(m, t, r):
    return m % (t, r['numFound'])


def query_timing_formatter_nop(m, t, r):
    return ''


def query(query_id, core, q, user_vd, dump_query, log_stats):
    timing_result = timing(query_id + ' query %f Result Count %d  VD length: ' + str(len(user_vd)),
                           lambda: solr_core.query_solr(core, q, True if dump_query > 1 else False),
                           query_timing_formatter if dump_query > 0 else query_timing_formatter_nop)

    if log_stats:
        query_run_stats[query_id][len(user_vd)].append(timing_result[1])


def query_all_models(tokens, user_vd, log_stats, dump_query=0):
    join = '{!join from=id to=vdid}'
    vd_integers = library.convert_id_list_to_integer(user_vd, library.id_to_integer)
    sharing_constrain = 'vd:(%s)' % ' '.join(user_vd)
    sharing_constrain_integer = 'vdi:(%s)' % ' '.join(vd_integers)
    fields = 'id, title'

    query1 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain, fields)
    query2 = 'q=%s&fq=%s&fl=%s' % (tokens, sharing_constrain, fields)
    query3 = 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain_integer, fields)
    query4 = 'q=%s&fq=%s&fl=%s' % (tokens, sharing_constrain_integer, fields)

    if dump_query > 1:
        print('J  query: ' + query1)
        print('C  query: ' + query2)
        print('JI query: ' + query3)
        print('CI query: ' + query4)

    query('J ', core1, query1, user_vd, dump_query, log_stats)
    query('C ', core2, query2, user_vd, dump_query, log_stats)
    query('JI', core3, query3, user_vd, dump_query, log_stats)
    query('CI', core4, query4, user_vd, dump_query, log_stats)

    if dump_query > 0:
        print('-' * 100)


def query_misc_user_vd(dump_query=0):
    search_tokens = random_most_used_words(library.get_used_words(), 100, 1, 5)
    if query_misc_user_vd_count > 0:
        print('=' * 100)
        for i in range(query_misc_user_vd_count):
            user_vd = []
            for vd_count in user_vd_counts:
                user_vd = library.make_used_group_user_list(False, vd_count, vd_count, user_vd)
                query_all_models(search_tokens, user_vd, True, dump_query)


def average_queries_stats():
    if run_average_queries_stats:
        average_stats = dict()
        for data_model in data_models:
            average_stats[data_model] = dict()

            for vd_size in user_vd_counts:
                raw_stats = query_run_stats[data_model][vd_size]
                average_stats[data_model][vd_size] = sum(raw_stats) / len(raw_stats)
                print("%s \t %5d \t %s" % (data_model, vd_size, '\t '.join(['%6.3f' % e for e in raw_stats])))

        print('=' * 100)
        print('%3s\t   %s' % ('   ', '\t '.join(['%6d' % k for k in user_vd_counts])))
        for data_model in data_models:
            raw_stats = average_stats[data_model]
            formatted = '\t '.join(['%6.3f' % raw_stats[k] for k in user_vd_counts])
            print('%3s\t  %s' % (data_model, formatted))


def set_limits():
    global user_vd_counts
    global query_misc_user_vd_count
    if not full_run:
        user_vd_counts = user_vd_counts[1:4]
        query_misc_user_vd_count = 2

search_host = 'localhost:4444'
filename = '%s/tmp/library.dmp' % expanduser("~")
solr_core = SolrCommands(search_host, False, False)

core1 = 'Core1'
core2 = 'Core2'
core3 = 'Core3'
core4 = 'Core4'

cores = [core1, core2, core3, core4]
data_models = ['J ', 'C ', 'JI', 'CI']
user_vd_counts = [10, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000, 4000]

rebuild_library = False
full_run = True

run_special_queries = False
run_average_queries_stats = True
random_query_count = 0
query_misc_user_vd_count = 20

query_run_stats = defaultdict(lambda: defaultdict(list))
library = get_library()

used_words = library.get_used_words()
top_used_users_groups = list(key for key, value in library.get_used_users_groups().most_common(20000))

set_limits()
special_queries()
random_queries()
query_misc_user_vd(0)


average_queries_stats()

print('=' * 100)
library.dump_stats()
