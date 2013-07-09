#! /usr/bin/env python
# coding=utf-8

from base_filer import BaseFiler

class File(object):
    def __init__(self, state):
        self.state = state
        self.isselect = False

    def __repr__(self):
        return self.filename + ' : ' + str(self.isselect)

class FilerView(object):
    def __init__(self, filer=BaseFiler):
        self.filer = filer()
        self.files = []
        self.reload()

    def reload(self):
        self.files = []
        for state in self.filer.ls():
            self.files.append(File(state))

    def pushd(self, path):
        return self.filer.pushd(path)

    def popd(self):
        return self.filer.pod()

    def cwd(self):
        return self.filer.cwd

    def cwd_history(self):
        return self.filer.cwd_history

    def toggle_isselet(self, index):
        self.files[index].isselect = self.files[index].isselect ^ True

    def toggle_isselect_all(self):
        for i in range(len(self.files)):
            self.toggle_isselet(i)

    def __repr__(self):
        return self.cwd()

def main():
    fview = FilerView()
    fview.toggle_isselet(0)
    fview.toggle_isselect_all()

if __name__ == '__main__':
    main()
