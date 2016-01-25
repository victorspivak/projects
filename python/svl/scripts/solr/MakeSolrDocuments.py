# !/usr/bin/python3

from svl.scripts.solr.SolrDataClasses import *
from svl.commons.RandomText import *
from svl.commons.Performance import *
from svl.scripts.solr.SolrCommand import *
from collections import Counter
import random
import pickle


class Library:
    def __init__(self, max_users_groups_count):
        self.books = []
        self.used_users_groups = Counter()
        self.max_users_groups_count = max_users_groups_count
        self.groups = list(set([random_id('00Gx0', 15) for group_index in range(max_users_groups_count)]))
        self.users = list(set([random_id('005x0', 15) for user_index in range(max_users_groups_count)]))
        self.used_words = get_used_words()
        self.max_vd_length = 0
        self.max_user_vd_length = 0

    def add_books(self, count, init_words, use_first_group_user, min_count, max_count):
        init_vd = [self.groups[0], self.users[0]] if use_first_group_user else []
        for index in range(count):
            self.books.extend(self.make_book_vd(init_words, self.make_group_user_list(init_vd, min_count, max_count)))
        self.used_words = get_used_words()

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

    def get_used_words(self):
        return self.used_words

    def dump_stats(self):
        print('Library: %d docs max VD length %d  max user VD length %d  used groups/users %d' %
              (len(self.books), self.max_vd_length, self.max_user_vd_length, len(self.used_users_groups)))


def populate_library():
    library.add_books(10000, '', True, 1, 10)
    library.add_books(5000, '', True, 10, 25)
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

solr = SolrCommands('localhost:8983', 'Core1')
filename = '/home/victor/tmp/library.dmp'

if rebuild_library:
    library = Library(10000)
    populate_library()
    solr.cleanup_core()
    timing("Indexing %f", lambda: solr.index_solr(library.get_books()))
else:
    library = load_library()

used_words = library.get_used_words()
most_used_words = list(key for key, value in used_words.most_common(20))
top_used_users_groups = list(key for key, value in library.get_used_users_groups().most_common(20000))

for i in range(10):
    user_vd = library.make_used_group_user_list(True, 10, 3000)

    tokens = '*FindMeToken* %s' % random.choice(most_used_words)
    join = '{!join from=id to=vdid}vd:(%s)' % ' '.join(user_vd)
    fields = 'id, title'
    timing('Quering %f for VD: ' + str(len(user_vd)), lambda: solr.query_solr('q=%s&fq=%s&fl=%s' % (tokens, join, fields)))

library.dump_stats()
