class Book:
    def __init__(self, _id, title, author, body, vdid):
        self.id = _id
        self.title = title
        self.author = author
        self.body = body
        self.vdid = vdid

    def id(self):
        return self.id

    def title(self):
        return self.title

    def author(self):
        return self.author

    def body(self):
        return self.body

    def vdid(self):
        return self.vdid

    def __str__(self):
        return 'Book: {} {} {}  vdid: {}'.format(self.id, self.title, self.author, self.vdid)


class VisibilityDescriptor:
    def __init__(self, _id, vd):
        self.id = _id
        self.vd = vd

    def id(self):
        return self.id

    def vd(self):
        return self.vd

    def __str__(self):
        return 'VD: {} Targets {}'.format(self.id, self.vd)


class VisibilityDescriptorInteger:
    def __init__(self, _id, vdi):
        self.id = _id
        self.vdi = vdi

    def id(self):
        return self.id

    def vdi(self):
        return self.vdi

    def __str__(self):
        return 'VD: {} Targets {}'.format(self.id, self.vdi)


class BookVdCombined:
    def __init__(self, book: Book, vd: VisibilityDescriptor):
        self.id = book.id
        self.title = book.title
        self.author = book.author
        self.body = book.body
        self.vd = vd.vd

    def id(self):
        return self.id

    def title(self):
        return self.title

    def author(self):
        return self.author

    def body(self):
        return self.body

    def vd(self):
        return self.vd

    def __str__(self):
        return 'Book: {} {} {}  vdid: {}'.format(self.id, self.title, self.author, self.vd)


class BookVdCombinedInteger:
    def __init__(self, book: Book, vd: VisibilityDescriptorInteger):
        self.id = book.id
        self.title = book.title
        self.author = book.author
        self.body = book.body
        self.vdi = vd.vdi

    def id(self):
        return self.id

    def title(self):
        return self.title

    def author(self):
        return self.author

    def body(self):
        return self.body

    def vdi(self):
        return self.vdi

    def __str__(self):
        return 'Book: {} {} {}  vdid: {}'.format(self.id, self.title, self.author, self.vdi)
