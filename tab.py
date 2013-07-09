#! /usr/bin/env python
# coding=utf-8

from filerview import FilerView
import os.path

class Tab(object):
    def __init__(self, view_left=FilerView(), view_right=FilerView()):
        self.views = (view_left, view_right)
        self.displayname = os.path.basename(self.left.cwd()) + ' : ' + os.path.basename(self.right.cwd())

    def get_view_left(self):
        return self.views[0]
    left = property(get_view_left)

    def get_view_right(self):
        return self.views[1]
    right = property(get_view_right)

    def __repr__(self):
        return self.left.cwd() + ' | ' + self.right.cwd()

class Group(object):
    def __init__(self):
        self.tabs = []

def main():
    tab = Tab()
    group = Group()
    group.tabs.append(tab)
    print(group.tabs)

if __name__ == '__main__':
    main()
