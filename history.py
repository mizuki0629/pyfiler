#! /usr/bin/env python
# coding=utf-8

class History(object):
    def __init__(self, val):
        self._list = [val]
        self._index = 0

    def __repr__(self):
        return str(self._list) + ':' + str(self._index)

    def push(self, val):
        self._index += 1
        del self._list[self._index:]
        self._list.append(val)

    def prev(self):
        if self._index <= 0:
            raise Exception()
        self._index -= 1
        return self._list[self._index]

    def next(self):
        if self._index >= len(self._list) - 1:
            raise Exception()
        self._index += 1
        return self._list[self._index]

    def has_prev(self):
        return self._index > 0

    def has_next(self):
        return self._index < len(self._list) - 1

    @property
    def current(self):
        return self._list[self._index]

    @property
    def index(self):
        return self._index

    @property
    def list(self):
        return self._list

if __name__ == '__main__':
    hist = History()
    print(hist.has_prev())
    hist.push("1")
    hist.push("2")
    hist.push("3")
    hist.push("4")
    print(hist.current)
    print(hist.list)
    print(hist.prev())
    print(hist.list)
    print(hist.prev())
    hist.push("5")
    print(hist.list)

