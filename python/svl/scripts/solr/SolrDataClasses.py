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
        return 'Book: %s %s %s  vdid: %s' % (self.id, self.title, self.author, self.vdid)


class VisibilityDescriptor:
    def __init__(self, _id, vd):
        self.id = _id
        self.vd = vd

    def id(self):
        return self.id

    def vd(self):
        return self.vd

    def __str__(self):
        return 'VD: %s Targets %s' % (self.id, self.vd)


class BookInteger:
    def __init__(self, _id, title, author, body, vdrefi):
        self.id = _id
        self.title = title
        self.author = author
        self.body = body
        self.vdrefi = vdrefi

    def id(self):
        return self.id

    def title(self):
        return self.title

    def author(self):
        return self.author

    def body(self):
        return self.body

    def vdrefi(self):
        return self.vdrefi

    def __str__(self):
        return 'Book: %s %s %s  vdrefi: %d' % (self.id, self.title, self.author, self.vdrefi)


class VisibilityDescriptorInteger:
    def __init__(self, _id, vd, vdidi):
        self.id = _id
        self.vd = vd
        self.vdidi = vdidi

    def id(self):
        return self.id

    def vd(self):
        return self.vd

    def vdidi(self):
        return self.vdidi

    def __str__(self):
        return 'VD Integer: %s %d Targets %s' % (self.id, self.vdidi, self.vd)


class VisibilityDescriptorIntegerSingleValue:
    def __init__(self, _id, vdsv, vdidi):
        self.id = _id
        self.vdsv = vdsv
        self.vdidi = vdidi

    def id(self):
        return self.id

    def vdsv(self):
        return self.vdsv

    def vdidi(self):
        return self.vdidi

    def __str__(self):
        return 'VD Integer SV: %s %d Targets %s' % (self.id, self.vdidi, self.vdsv)


class BookVdCombined:
    def __init__(self, book_id, title, author, body, vd):
        self.id = book_id
        self.title = title
        self.author = author
        self.body = body
        self.vd = vd

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
        return 'Book: %s %s %s  vdid: %s' % (self.id, self.title, self.author, self.vd)

