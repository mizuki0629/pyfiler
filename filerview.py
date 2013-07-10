#! /usr/bin/env python
# coding=utf-8

from base_filer import BaseFiler
import os.path
from  PyQt4.QtCore import Qt # TODO qtに依存しない形にすること

class Subject(object):
    def __init__(self):
        self.observers = []

    def register_observer(self, observer):
        self.observers.append(observer)

    def remove_observer(self, observer):
        self.observers.delete(observer)

    def notify_observers(self):
        for o in self.observers:
            o.update(self)

def Notify(func):
    def notify_observers_after_original_function(self, *args, **kwargs):
        result = func(self, *args, **kwargs)
        self.notify_observers()
        return result
    return notify_observers_after_original_function

class FileViewModel(object):
    def __init__(self, state):
        self.state = state
        self.isselect = False

    def __repr__(self):
        return self.filename + ' : ' + str(self.isselect)

class FilerViewModel(Subject):
    def __init__(self, filer=BaseFiler):
        Subject.__init__(self)
        self.filer = filer()
        self.files = []
        self.cursor = 0
        self.reload()

    def _up_cursor(self):
        if self.cursor >= 0:
            self.cursor = self.cursor - 1

    def _down_cursor(self):
        if self.cursor < len(self.files) - 1:
            self.cursor = self.cursor + 1

    cursor_file = property(lambda self : self.files[self.cursor])
    cursor_file_abspath = property(lambda self : self.cursor_file.state['abspath'])

    def Reload(func):
        def reload_after_original_function(self, *args, **kwargs):
            result = func(self, *args, **kwargs)
            self.cursor = 0
            self.files = []
            for state in self.filer.ls():
                self.files.append(FileViewModel(state))
            return result
        return reload_after_original_function

    @Notify
    def cursor_up(self):
        self._up_cursor()

    @Notify
    def cursor_down(self):
        self._down_cursor()

    @Notify
    def cursor_first(self):
        self.cursor = 0

    @Notify
    def cursor_last(self):
        self.cursor = len(self.files) - 1

    @Notify
    @Reload
    def reload(self):
        pass

    @Notify
    @Reload
    def pushd(self, path):
        self.filer.pushd(path)

    @Notify
    @Reload
    def popd(self):
        self.filer.popd()

    @Notify
    @Reload
    def chdir_parent(self):
        self.filer.chdir('../')

    @Notify
    @Reload
    def chdir_or_execute(self):
        self.filer.chdir_or_execute(self.cursor_file_abspath)

    def cwd(self):
        return self.filer.cwd

    def cwd_history(self):
        return self.filer.cwd_history

    @Notify
    def toggle_isselet_up(self):
        self._toggle_isselet(self.cursor)
        self._up_cursor()

    @Notify
    def toggle_isselet_down(self):
        self._toggle_isselet(self.cursor)
        self._down_cursor()

    def _toggle_isselet(self, index):
        self.files[index].isselect = self.files[index].isselect ^ True

    @Notify
    def toggle_isselect_all(self):
        for i in range(len(self.files)):
            self._toggle_isselet(i)

    def __repr__(self):
        return self.cwd()

class TwoScreenFilerViewModel(Subject):
    def __init__(self, view_left=FilerViewModel(), view_right=FilerViewModel()):
        Subject.__init__(self)
        self.views = (view_left, view_right)
        self.displayname = os.path.basename(self.left.cwd()) + ' : ' + os.path.basename(self.right.cwd())
        self.current = self.left

    @Notify
    def change_focus_left(self):
        self.current = self.left

    @Notify
    def change_focus_right(self):
        self.current = self.right

    @Notify
    def change_focus(self):
        if self.current == self.left:
            self.current = self.right
        else:
            self.current = self.left

    def get_view_left(self):
        return self.views[0]
    left = property(get_view_left)

    def get_view_right(self):
        return self.views[1]
    right = property(get_view_right)

    def __repr__(self):
        return self.left.cwd() + ' | ' + self.right.cwd()

class TabViewModel(object):
    def __init__(self):
        self.tabs = []

class KeyEventHandler(object):
    def __init__(self, viewmodel):
        self.viewmodel = viewmodel

    # TODO qtに依存しない形にすること
    def on_key_press(self, event):
        key = event.key()
        shift = event.modifiers() & Qt.ShiftModifier
        if shift:
            if key == Qt.Key_Space:
                self.viewmodel.current.toggle_isselet_up()
            elif key == Qt.Key_G:
                self.viewmodel.current.cursor_last()
            elif key == Qt.Key_H:
                self.viewmodel.current.chdir_parent()
        else:
            if key == Qt.Key_A:
                self.viewmodel.current.toggle_isselect_all()
            elif key == Qt.Key_K:
                self.viewmodel.current.cursor_up()
            elif key == Qt.Key_J:
                self.viewmodel.current.cursor_down()
            elif key == Qt.Key_L:
                self.viewmodel.current.chdir_or_execute()
            elif key == Qt.Key_Tab:
                self.viewmodel.change_focus()
            elif key == Qt.Key_H:
                self.viewmodel.current.chdir_parent()
            elif key == Qt.Key_R:
                self.viewmodel.current.reload()
            elif key == Qt.Key_G:
                self.viewmodel.current.cursor_first()
            if key == Qt.Key_Space:
                self.viewmodel.current.toggle_isselet_down()


def main():
    fview = FilerViewModel()
    fview._toggle_isselet(0)
    fview.toggle_isselect_all()

if __name__ == '__main__':
    main()
