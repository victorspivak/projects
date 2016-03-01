from svl.scripts.solr.SolrDataClasses import *
from svl.commons.Performance import *


class SolrDataModel:
    def __init__(self, model_id, core):
        self.model_id = model_id
        self.core = core
        self.books = []
        self.books_count = 0

    def model_id(self):
        return self.model_id

    def core(self):
        return self.core

    def books_count(self):
        return self.books_count

    def indexing(self, solr_core):
        timing(self.model_id + ' Indexing %f', lambda: solr_core.index_solr(self.core, self.books))
        self.books = []

    def __str__(self):
        return 'SolrDataModel: %s core: %s' % (self.model_id, self.core)


class JoinModel(SolrDataModel):
    def __init__(self, core):
        super().__init__('J ', core)

    def add_book(self, book_id, title, author, body, vd):
        self.books_count += 1
        vd_id = "vd-" + book_id
        book = Book(book_id, title, author, body, vd_id)
        desc = VisibilityDescriptor(vd_id, vd)
        self.books.extend([book, desc])

    def query(self, tokens, user_vd, fields):
        join = '{!join from=id to=vdid}'
        sharing = 'vd:(%s)' % ' '.join(user_vd)

        return 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing, fields)


class CombinedModel(SolrDataModel):
    def __init__(self, core):
        super().__init__('C ', core)

    def add_book(self, book_id, title, author, body, vd):
        self.books_count += 1
        self.books.append(BookVdCombined(book_id, title, author, body, vd))

    def query(self, tokens, user_vd, fields):
        sharing = 'vd:(%s)' % ' '.join(user_vd)

        return 'q=%s&fq=%s&fl=%s' % (tokens, sharing, fields)


class JoinIntegerModel(SolrDataModel):
    vdref_factory = 0

    def __init__(self, core):
        super().__init__('JI', core)

    def add_book(self, book_id, title, author, body, vd):
        self.books_count += 1
        vd_id = "vd-" + book_id
        JoinIntegerModel.vdref_factory += 1
        book_integer = BookInteger(book_id, title, author, body, JoinIntegerModel.vdref_factory)
        vd_integer = VisibilityDescriptorInteger(vd_id, vd, JoinIntegerModel.vdref_factory)
        self.books.extend([book_integer, vd_integer])

    def query(self, tokens, user_vd, fields):
        join = '{!join from=vdidi to=vdrefi}'
        sharing_constrain = 'vd:(%s)' % ' '.join(user_vd)

        return 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing_constrain, fields)


class JoinIntegerSingleValueModel(SolrDataModel):
    vdref_factory = 0
    vdid_factory = 0

    def __init__(self, core):
        super().__init__('SV', core)

    def add_book(self, book_id, title, author, body, vd):
        self.books_count += 1
        JoinIntegerSingleValueModel.vdref_factory += 1
        book_integer = BookInteger(book_id, title, author, body, JoinIntegerSingleValueModel.vdref_factory)

        self.books.append(book_integer)
        for item in vd:
            JoinIntegerSingleValueModel.vdid_factory += 1
            self.books.append(VisibilityDescriptorIntegerSingleValue(JoinIntegerSingleValueModel.vdid_factory, item,
                                                                     JoinIntegerSingleValueModel.vdref_factory))

    def query(self, tokens, user_vd, fields):
        join = '{!join from=vdidi to=vdrefi}'
        sharing = 'vdsv:(%s)' % ' '.join(user_vd)

        return 'q=%s&fq=%s%s&fl=%s' % (tokens, join, sharing, fields)
