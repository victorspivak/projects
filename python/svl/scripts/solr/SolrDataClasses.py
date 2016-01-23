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

    def to_string(self):
        return 'Book: {} {} {}  vdid: {}'.format(self.id, self.title, self.author, self.vdid)


class VisibilityDescriptor:
    def __init__(self, _id, vd):
        self.id = _id
        self.vd = vd

    def id(self):
        return self.id

    def vd(self):
        return self.vd

    def to_string(self):
        return 'VD: {} Targets {}'.format(self.id, self.vd)


