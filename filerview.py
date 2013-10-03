#! /usr/bin/env python
# coding=utf-8

import logging
from base_filer import BaseFiler
import os.path
from PyQt4.QtCore import Qt  # TODO qtに依存しない形にすること


class Subject(object):
    class Event(object):
        def __init__(self, kind, **kwargs):
            self.kind = kind
            self.opt = kwargs

    def __init__(self):
        self.observers = []

    def attach(self, observer):
        self.observers.append(observer)

    def detach(self, observer):
        self.observers.delete(observer)

    def notify(self, event):
        for o in self.observers:
            o.update(self, event)


def Notify(kind):
    def notify_decorator(func):
        def notify_observers_after_original_function(self, *args, **kwargs):
            result = func(self, *args, **kwargs)
            self.notify(Subject.Event(kind))
            return result
        return notify_observers_after_original_function
    return notify_decorator



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

    cursor_file = property(lambda self: self.files[self.cursor])
    cursor_file_abspath = property(lambda self: self.cursor_file.state['abspath'])

    def Reload(func):
        def reload_after_original_function(self, *args, **kwargs):
            result = func(self, *args, **kwargs)
            self.cursor = 0
            self.files = []
            for state in self.filer.ls():
                self.files.append(FileViewModel(state))
            return result
        return reload_after_original_function

    @Notify('cursor')
    def cursor_up(self):
        self._up_cursor()

    @Notify('cursor')
    def cursor_down(self):
        self._down_cursor()

    @Notify('cursor')
    def cursor_first(self):
        self.cursor = 0

    @Notify('cursor')
    def cursor_last(self):
        self.cursor = len(self.files) - 1

    @Notify('update')
    @Reload
    def reload(self):
        pass

    @Notify('chdir')
    @Reload
    def pushd(self, path):
        self.filer.pushd(path)

    @Notify('chdir')
    @Reload
    def popd(self):
        self.filer.popd()

    @Notify('chdir')
    @Reload
    def chdir_parent(self):
        self.filer.chdir('../')

    @Notify('chdir')
    @Reload
    def chdir_or_execute(self):
        self.filer.chdir_or_execute(self.cursor_file_abspath)

    def cwd(self):
        return self.filer.cwd

    def cwd_history(self):
        return self.filer.cwd_history

    @Notify('cursor')
    def toggle_isselet_up(self):
        self._toggle_isselet(self.cursor)
        self.notify(Subject.Event('select', indexes=[self.cursor]))
        self._up_cursor()

    @Notify('cursor')
    def toggle_isselet_down(self):
        self._toggle_isselet(self.cursor)
        self.notify(Subject.Event('select', indexes=[self.cursor]))
        self._down_cursor()

    def _toggle_isselet(self, index):
        self.files[index].isselect = self.files[index].isselect ^ True

    def toggle_isselect_all(self):
        for i in range(len(self.files)):
            self._toggle_isselet(i)
        self.notify(Subject.Event('select', indexes=range(len(self.files))))

    def __repr__(self):
        return self.cwd()


class TwoScreenFilerViewModel(Subject):
    FocusLeft = 0
    FocusRight = 1

    def __init__(self, view_left=FilerViewModel,
                 view_right=FilerViewModel):
        Subject.__init__(self)
        self.views = (view_left(), view_right())
        self.left.attach(self)
        self.right.attach(self)
        self.current = self.left
        self.focus = self.FocusLeft

    def update(self, viewmodel, event):
        if event.kind == 'chdir':
            self.notify(event)

    @Notify('update')
    def change_focus_left(self):
        self.current = self.left

    @Notify('update')
    def change_focus_right(self):
        self.current = self.right

    @Notify('update')
    def change_focus(self):
        if self.current == self.left:
            self.current = self.right
            self.focus = self.FocusRight
        else:
            self.current = self.left
            self.focus = self.FocusLeft

    def get_view_left(self):
        return self.views[0]
    left = property(get_view_left)

    def get_view_right(self):
        return self.views[1]
    right = property(get_view_right)

    def get_displayname(self):
        return os.path.basename(self.left.cwd()) + ' : ' + os.path.basename(self.right.cwd())
    displayname = property(get_displayname)

    def __repr__(self):
        return self.left.cwd() + ' | ' + self.right.cwd()


class TabViewModel(Subject):
    def __init__(self):
        Subject.__init__(self)
        self.tabs = []
        self.currentIndex = 0
        self.addTab(TwoScreenFilerViewModel())

    @Notify('add')
    def addTab(self, filervm):
        self.tabs.append(filervm)
        self.currentIndex = len(self.tabs) - 1

    def currentTab(self):
        return self.tabs[self.currentIndex]

    def removeTab(self, index):
        self.tabs.pop(index)
        if self.currentIndex >= len(self.tabs):
            self.currentIndex = len(self.tabs) - 1
        self.notify(Subject.Event('remove', index=index))

    @Notify('next')
    def nextTab(self):
        if self.currentIndex < len(self.tabs) - 1:
            self.currentIndex += 1
        else:
            self.currentIndex = 0

    @Notify('prev')
    def prevTab(self):
        if self.currentIndex > 0:
            self.currentIndex -= 1
        else:
            self.currentIndex = len(self.tabs) - 1

class KeyEventHandler(object):
    def __init__(self, viewmodel):
        self.viewmodel = viewmodel

    # TODO qtに依存しない形にすること
    def on_key_press(self, event):
        key = event.key()
        shift = event.modifiers() & Qt.ShiftModifier
        currentTab = self.viewmodel.currentTab()
        current = currentTab.current
        if shift:
            if key == Qt.Key_Space:
                current.toggle_isselet_up()
            elif key == Qt.Key_G:
                current.cursor_last()
            elif key == Qt.Key_H:
                current.chdir_parent()
        else:
            if key == Qt.Key_A:
                current.toggle_isselect_all()
            elif key == Qt.Key_K:
                current.cursor_up()
            elif key == Qt.Key_J:
                current.cursor_down()
            elif key == Qt.Key_L:
                current.chdir_or_execute()
            elif key == Qt.Key_Tab:
                currentTab.change_focus()
            elif key == Qt.Key_H:
                current.chdir_parent()
            elif key == Qt.Key_R:
                current.reload()
            elif key == Qt.Key_G:
                current.cursor_first()
            elif key == Qt.Key_N:
                self.viewmodel.nextTab()
            elif key == Qt.Key_P:
                self.viewmodel.prevTab()
            elif key == Qt.Key_D:
                self.viewmodel.removeTab(self.viewmodel.currentIndex)
            elif key == Qt.Key_T:
                self.viewmodel.addTab(TwoScreenFilerViewModel())
            if key == Qt.Key_Space:
                current.toggle_isselet_down()


def main():
    fview = FilerViewModel()
    fview._toggle_isselet(0)
    fview.toggle_isselect_all()

if __name__ == '__main__':
    main()
