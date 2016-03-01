# !/usr/bin/python3

from svl.scripts.solr.SolrDataModel import *
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
        self.used_users_groups = Counter()
        self.max_users_groups_count = max_users_groups_count
        self.groups = list(set([random_id('00Gx0', 15) for group_index in range(max_users_groups_count)]))
        self.users = list(set([random_id('005x0', 15) for user_index in range(max_users_groups_count)]))
        self.used_words = get_used_words()
        self.max_vd_length = 0
        self.max_user_vd_length = 0
        self.total_books_count = 0

    def add_books(self, count, init_words, use_first_group_user, min_vd_size, max_vd_size):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []

        chunk_size = 10000
        for chunk in range(0, count, chunk_size):
            for index in range(chunk, min(count, chunk + chunk_size)):
                self.add_book(init_words, init_vd, max_vd_size, min_vd_size)

            print('Indexing chunk: %d documents' % data_models[0].books_count)
            self.indexing()

        self.used_words = get_used_words()

    def add_book(self, init_words, init_vd, max_vd_size, min_vd_size):
        book_id = random_id('001x0', 15)
        title = random_text(1, 5)
        author = random_text(1, 3)
        body = random_text(10, 100) + ' ' + init_words
        vd = self.make_group_user_list(init_vd, min_vd_size, max_vd_size)
        for data_model in data_models:
            data_model.add_book(book_id, title, author, body, vd)

    def indexing(self):
        self.total_books_count += data_models[0].books_count
        for data_model in data_models:
            data_model.indexing(solr_core)

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
              (self.total_books_count, self.max_vd_length, self.max_user_vd_length, len(self.used_users_groups)))


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
    for data_model in data_models:
        solr_core.cleanup_core(data_model.core)


def populate_library(lib):
    if full_run:
        populate_library_misc_model(lib, 'FindMeToken')
        # populate_library_many_targets_model(lib, 'FindMeToken')
        # populate_library_small_targets_model(lib, 'FindMeToken')

        lib.add_books(10, 'FindMeToken', True, 10, 25)
    else:
        lib.add_books(1000, '', True, 1, 10)
        lib.add_books(10, 'FindMeToken', True, 10, 25)

    with open(filename, 'wb') as f:
        pickle.dump(lib, f)


def populate_library_misc_model(lib, init_words):
    lib.add_books(500000, init_words, True, 1, 10)
    lib.add_books(20000, init_words, True, 10, 25)
    lib.add_books(2000, init_words, True, 25, 100)
    lib.add_books(1000, init_words, True, 100, 1000)
    lib.add_books(500, init_words, True, 1000, 2500)
    lib.add_books(100, init_words, True, 2500, 5000)


def populate_library_many_targets_model(lib, init_words):
    lib.add_books(100000, init_words, True, 2500, 5000)


def populate_library_small_targets_model(lib, init_words):
    lib.add_books(1000000, init_words, True, 1, 10)


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

        query_all_models('%s' % search_term,
                         library.make_used_group_user_list(False, 10, 30), False, 2)

        query_all_models('FindMeToken %s' % search_term,
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
    if dump_query > 1:
        for data_model in data_models:
            print('%s query: %s' % (data_model.model_id, data_model.query(tokens, user_vd, fields)))

    for data_model in data_models:
        solr_query = data_model.query(tokens, user_vd, fields)
        query(data_model.model_id, data_model.core, solr_query, user_vd, dump_query, log_stats)

    if dump_query > 0:
        print('-' * 100)


def query_misc_user_vd(dump_query=0):
    if query_misc_user_vd_count > 0:
        for i in range(query_misc_user_vd_count):
            search_tokens = random_most_used_words(library.get_used_words(), 100, 1, 5)

            query('NS', data_models[0].core, 'q=%s&fl=%s' % (search_tokens, fields), [], dump_query, False)
            print('=' * 100)

            user_vd = []
            for vd_count in user_vd_counts:
                user_vd = library.make_used_group_user_list(False, vd_count, vd_count, user_vd)
                query_all_models(search_tokens, user_vd, True, dump_query)


def average_queries_stats():
    if run_average_queries_stats:
        average_stats = dict()
        for data_model in data_models:
            average_stats[data_model.model_id] = dict()

            for vd_size in user_vd_counts:
                raw_stats = query_run_stats[data_model.model_id][vd_size]
                average_stats[data_model.model_id][vd_size] = sum(raw_stats) / len(raw_stats)
                print("%s \t %5d \t %s" % (data_model.model_id, vd_size, '\t '.join(['%6.3f' % e for e in raw_stats])))

        print('=' * 100)
        print('%3s\t   %s' % ('   ', '\t '.join(['%6d' % k for k in user_vd_counts])))
        for data_model in data_models:
            raw_stats = average_stats[data_model.model_id]
            formatted = '\t '.join(['%6.3f' % raw_stats[k] for k in user_vd_counts])
            print('%3s\t  %s' % (data_model.model_id, formatted))


def set_limits():
    global user_vd_counts
    global query_misc_user_vd_count
    if not full_run:
        user_vd_counts = user_vd_counts[1:4]
        query_misc_user_vd_count = 2


search_host = 'localhost:4444'
filename = '%s/tmp/library.dmp' % expanduser("~")
solr_core = SolrCommands(search_host, False, False)
fields = 'id, title'

data_models = [
    JoinModel('Core1')
    , CombinedModel('Core2')
    , JoinIntegerModel('Core3')
    # , JoinIntegerSingleValueModel('Core4')
]
user_vd_counts = [10, 100, 250, 500, 750, 1000, 1500, 2000, 2500, 3000, 4000]

rebuild_library = True
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
query_misc_user_vd(1)

average_queries_stats()

print('=' * 100)
library.dump_stats()
