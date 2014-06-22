#! /usr/bin/env python
# coding=utf-8

import command
import fileutil
import history
import imp
import keymap
import lispy
import logging
import os
import os.path
import platform
import re


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


class File(object):
    def __init__(self, state):
        self.state = state
        self.isselect = False

    def __repr__(self):
        return self.state['filename'] + ' : ' + str(self.isselect)

class Pain(Subject):
    def __init__(self):
        Subject.__init__(self)
        self.files = []
        self.state = (os.getcwd(), 0)
        self.hisotry = history.History(self.state)
        self.cursor_bak = None
        self.search_str = None
        self.reload()

    @property
    def state(self):
        return (self._cwd, self.cursor)
    @state.setter
    def state(self, val):
        self._cwd, self.cursor = val

    @property
    def cursor(self):
        return self._cursor

    @cursor.setter
    @Notify('cursor')
    def cursor(self, val):
        if val > len(self.files) - 1:
           val = len(self.files) - 1
        if val < 0:
           val = 0
        self._cursor = val

    def get_cursor_file(self):
        return self.files[self.cursor]

    cursor_file = property(lambda self: self.files[self.cursor])
    cursor_file_abspath = property(lambda self: self.cursor_file.state['abspath'])

    def Reload(func):
        def reload_after_original_function(self, *args, **kwargs):
            result = func(self, *args, **kwargs)
            self.files = list(map(lambda x: File(x), sorted(fileutil.status(self.cwd()), key=lambda x: x['filename'])))
            return result
        return reload_after_original_function

    def cursor_up(self):
        self.cursor = self.cursor - 1

    def cursor_down(self):
        self.cursor = self.cursor + 1

    def cursor_first(self):
        self.cursor = 0

    def cursor_last(self):
        self.cursor = len(self.files) - 1

    @Notify('update')
    @Reload
    def reload(self):
        pass

    def pushd(self, path):
        self.chdir(path)

    @Notify('chdir')
    @Reload
    def popd(self):
        if self.hisotry.has_prev():
            self.hisotry.current = self.state
            self.state = self.hisotry.prev()

    def chdir_parent(self):
        self.chdir('../')

    def open_assoc(self):
        fileutil.open_assoc(self.cursor_file_abspath)

    @Notify('chdir')
    @Reload
    def chdir(self, path=None):
        if path is None:
            abspath  = self.cursor_file_abspath
        else:
            abspath = self._abspath(path)
        if os.path.isdir(abspath):
            self.hisotry.current = self.state
            self.state = (abspath, 0)
            self.hisotry.push(self.state)

    def _abspath(self, path):
        if os.path.isabs(path) and os.path.exists(path):
            return path

        tmp = os.path.normpath(os.path.join(self._cwd, path))
        if os.path.exists(tmp):
            return  tmp

        tmp = os.path.normpath(os.path.expandvars(tmp))
        if os.path.exists(tmp):
            return  tmp

        path = os.path.normpath(os.path.expandvars(os.path.expanduser(path)))
        return path

    def cwd(self):
        return self._cwd

    def toggle_isselet_up(self):
        self._toggle_isselet(self.cursor)
        self.notify(Subject.Event('select', indexes=[self.cursor]))
        self.cursor = self.cursor - 1

    def toggle_isselet_down(self):
        self._toggle_isselet(self.cursor)
        self.notify(Subject.Event('select', indexes=[self.cursor]))
        self.cursor = self.cursor + 1

    def _toggle_isselet(self, index):
        self.files[index].isselect = self.files[index].isselect ^ True

    def toggle_isselect_all(self):
        for i in range(len(self.files)):
            self._toggle_isselet(i)
        self.notify(Subject.Event('select', indexes=range(len(self.files))))

    def __repr__(self):
        return self.cwd()


    def rotate_list(self, l, y=1):
       if len(l) == 0:
          return l
       y = y % len(l)    # Why? this works for negative y

       return l[y:] + l[:y]

    def search(self, sstr=None):
        if sstr is None:
            if self.search_str is None:
                return
            else:
                sstr = self.search_str
                self.cursor_bak = self.cursor
        if self.cursor_bak is None:
            self.cursor_bak = self.cursor

        for index, f in self.rotate_list(list(enumerate(self.files)), self.cursor_bak+1):
            if re.search(sstr, f.state['filename'], re.IGNORECASE):
                self.cursor = index
                return

    def rsearch(self, sstr=None):
        if sstr is None:
            if self.search_str is None:
                return
            else:
                sstr = self.search_str
                self.cursor_bak = self.cursor
        if self.cursor_bak is None:
            self.cursor_bak = self.cursor

        for index, f in reversed(self.rotate_list(list(enumerate(self.files)), self.cursor_bak)):
            if re.search(sstr, f.state['filename']):
                self.cursor = index
                return


    def search_cancel(self):
        if self.cursor_bak is not None:
            self.cursor = self.cursor_bak
            self.cursor_bak = None

    def search_enter(self, sstr):
        self.cursor_bak = None
        self.search_str = sstr


class Tab(Subject):
    FocusLeft = 0
    FocusRight = 1

    def get_current(self):
        return self._current

    def set_current(self, left_or_right):
        self._current = left_or_right
        if left_or_right is self.left:
            self.focus = self.FocusLeft
        else:
            self.focus = self.FocusRight
    current = property(get_current, set_current)

    other = property(lambda self: self.right if self.focus == self.FocusLeft else self.left)

    def __init__(self, view_left=Pain,
                 view_right=Pain):
        Subject.__init__(self)
        self.views = (view_left(), view_right())
        self.left.attach(self)
        self.right.attach(self)
        self.current = self.left

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
        else:
            self.current = self.left

    def get_view_left(self):
        return self.views[0]
    left = property(get_view_left)

    def get_view_right(self):
        return self.views[1]
    right = property(get_view_right)

    def get_displayname(self):
        return os.path.basename(self.left.cwd()) + ' | ' + os.path.basename(self.right.cwd())
    displayname = property(get_displayname)

    def __repr__(self):
        return self.left.cwd() + ' | ' + self.right.cwd()

NORMAL_MODE = 'normal'
COMMAND_MODE = 'command'
SEARCH_MODE = 'search'
SH_MODE = 'sh'

class Model(Subject):
    def __init__(self):
        Subject.__init__(self)
        self.tabs = []
        self.currentIndex = 0
        self.addTab(Tab())
        self.mode = NORMAL_MODE

    @property
    def tab(self):
        return self.currentTab()

    @property
    def pain(self):
        return self.currentTab().get_current()

    @property
    def cursorfile(self):
        return self.currentTab().get_current().cursor_file

    @property
    def selects(self):
        return self.currentTab().get_current().cursor_file

    def get_mode(self):
        return self._mode

    @Notify('mode')
    def set_mode(self, mode):
        self._mode = mode
        logging.debug(mode)

    mode = property(get_mode, set_mode)

    def commandline_edited(self, text):
        if self.mode == SEARCH_MODE:
            self.currentTab().current.search(text)

    def do_keyevent(self, key, modifiers):
        import cProfile, pstats, io
        pr = cProfile.Profile()
        pr.enable()
        try:
            if self.mode == NORMAL_MODE:
                keymap.do_keymap(keymap.normal_map,
                        keymap.Key(key, modifiers))
                return True
            elif self.mode == COMMAND_MODE:
                return keymap.do_keymap(keymap.command_map,
                        keymap.Key(key, modifiers))
            elif self.mode == SEARCH_MODE:
                return keymap.do_keymap(keymap.search_map,
                        keymap.Key(key, modifiers))
            elif self.mode == SH_MODE:
                return keymap.do_keymap(keymap.sh_map,
                        keymap.Key(key, modifiers))
            else:
                return False
        finally:
            pr.disable()
            s = io.StringIO()
            sortby = 'time'
            ps = pstats.Stats(pr, stream=s).sort_stats(sortby)
            ps.print_stats()
            #ps.print_callers()
            #logging.debug(s.getvalue())

    def tabnew(self):
        self.addTab(Tab())

    @Notify('add')
    def addTab(self, filervm):
        self.tabs.append(filervm)
        self.currentIndex = len(self.tabs) - 1

    def currentTab(self):
        return self.tabs[self.currentIndex]

    def onlyTab(self):
        i = 0
        while True:
            if len(self.tabs) <= 1:
                return
            if self.currentIndex != i:
                self.removeTab(i)
            else:
                i += 1

    def removeTab(self, index):
        if len(self.tabs) > 1:
            self.tabs.pop(index)
            if self.currentIndex >= len(self.tabs):
                self.currentIndex = len(self.tabs) - 1
            self.notify(Subject.Event('remove', index=index))

    @Notify('tabchange')
    def changeTab(self, index):
        if 0 <= index and index < len(self.tabs):
            self.currentIndex = index

    @Notify('tabchange')
    def nextTab(self, isloop=False):
        if self.currentIndex < len(self.tabs) - 1:
            self.currentIndex += 1
        elif isloop:
            self.currentIndex = 0

    @Notify('tabchange')
    def prevTab(self, isloop=False):
        if self.currentIndex > 0:
            self.currentIndex -= 1
        elif isloop:
            self.currentIndex = len(self.tabs) - 1

    def reload_commands(self):
        imp.reload(command)


def main():
    fview = Pain()
    fview._toggle_isselet(0)
    fview.toggle_isselect_all()

if __name__ == '__main__':
    main()
