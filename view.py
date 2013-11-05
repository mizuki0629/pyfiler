#! /usr/bin/env python
# coding: utf-8

import logging
import logging.handlers
import sys
import PyQt4.QtCore as QtCore
from PyQt4.QtCore import Qt
import PyQt4.QtGui as QtGui
from model import Model, Subject, COMMAND_MODE, SEARCH_MODE, SH_MODE
import command
import lispy
import os.path
import stat

header = [
        ['s', '', (QtGui.QHeaderView.Fixed, 15),],
        ['filename', 'filename', (QtGui.QHeaderView.Stretch, 0),],
        ['git_idx', 'i', (QtGui.QHeaderView.ResizeToContents, 0),],
        ['git_wk', 'w', (QtGui.QHeaderView.ResizeToContents, 0),],
        ['filemode', 'filemode', (QtGui.QHeaderView.ResizeToContents, 0),],
        ['st_mtime', '更新時間', (QtGui.QHeaderView.ResizeToContents, 0),],
        ['st_size', 'サイズ', (QtGui.QHeaderView.ResizeToContents, 0),],
        ]

SUFFIXES = {1000: [' B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'],
            1024: ['  B', 'KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB']}

def approximate_size(size, a_kilobyte_is_1024_bytes=True):
    '''Convert a file size to human-readable form.

    Keyword arguments:
    size -- file size in bytes
    a_kilobyte_is_1024_bytes -- if True (default), use multiples of 1024
                                if False, use multiples of 1000

    Returns: string

    '''
    if size < 0:
        raise ValueError('number must be non-negative')

    multiple = 1024 if a_kilobyte_is_1024_bytes else 1000
    for suffix in SUFFIXES[multiple]:
        if size < multiple:
            return '{0:.1f} {1}'.format(size, suffix)
        size /= multiple

    raise ValueError('number too large')

class FilerWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        viewmodel.attach(self)
        self.isfileiconenable = False
        self.setup_ui(viewmodel)

    def setup_ui(self, viewmodel):
        panel_layout = QtGui.QVBoxLayout()
        self.setLayout(panel_layout)
        panel_layout.setContentsMargins(0, 0, 0, 0)
        panel_layout.setSpacing(0)

        self.cwdline = QtGui.QLineEdit()
        self.cwdline.setEnabled(False)
        palette = QtGui.QPalette()
        palette.setColor(QtGui.QPalette.Base, Qt.white)
        self.cwdline.setPalette(palette)
        panel_layout.addWidget(self.cwdline)

        tablewidget = QtGui.QTableWidget()
        self.tablewidget = tablewidget
        panel_layout.addWidget(tablewidget)

        tablewidget.setSelectionMode(QtGui.QAbstractItemView.SingleSelection)
        tablewidget.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        tablewidget.setShowGrid(False)
        tablewidget.setGridStyle(QtCore.Qt.NoPen)
        tablewidget.setVerticalScrollBarPolicy(QtCore.Qt.ScrollBarAlwaysOn)

        self.update(viewmodel, Subject.Event('update'))

    def textcolor(self, file, col):
        try:
            if col == 'git_wk':
                return QtGui.QColor(255, 0, 0)
            if col == 'git_idx':
                return QtGui.QColor(0, 255, 0)

            if file.isselect:
                return QtGui.QColor(150, 150, 0)
            # directory
            elif file.state.get('filemode', '').startswith('d'):
                return QtGui.QColor(204, 255, 102)
            elif file.state.get('filemode', '').startswith('l'):
                return QtGui.QColor(162, 222, 255)
            elif file.state.get('filename', '').endswith('.lnk'):
                return QtGui.QColor(102, 162, 255)
            elif file.state.get('filemode', '').endswith('x'):
                return QtGui.QColor(255, 102, 102)
            else:
                return QtGui.QColor(255, 255, 255)
        except Exception:
            return QtGui.QColor(255, 255, 255)

    def update(self, viewmodel, event):
        if event.kind == 'cursor':
            self.tablewidget.selectRow(viewmodel.cursor)
        elif event.kind == 'select':
            for i in event.opt['indexes']:
                # 選択時
                val = '*' if viewmodel.files[i].isselect else ' '
                for j, col in enumerate(map(lambda x: x[0], header)):
                    if col == 's':
                        self.tablewidget.setItem(i, j, QtGui.QTableWidgetItem(val))
                    item = self.tablewidget.item(i, j)
                    color = self.textcolor(viewmodel.files[i], col)
                    if item is not None:
                        item.setTextColor(color)
        else:
            self.cwdline.setText(viewmodel.cwd())

            self.tablewidget.setColumnCount(len(header))
            self.tablewidget.setRowCount(len(viewmodel.files))

            for i, m in enumerate(map(lambda x: x[2], header)):
                self.tablewidget.horizontalHeader().setResizeMode(i, m[0])
                self.tablewidget.setColumnWidth(i, m[1])

            self.tablewidget.setHorizontalHeaderLabels(list(map(lambda x: x[1], header)))
            self.tablewidget.verticalHeader().setVisible(False)

            for i, f in enumerate(viewmodel.files):
                for j, col in enumerate(map(lambda x: x[0], header)):
                    item = None
                    if col in f.state:
                        if col == 'filename':
                            if self.isfileiconenable:
                                icon = QtGui.QFileIconProvider().icon(QtCore.QFileInfo(f.state['abspath']))
                                item = QtGui.QTableWidgetItem(icon, f.state[col])
                            else:
                                item = QtGui.QTableWidgetItem(f.state[col])
                        elif col == 's':
                            item = QtGui.QTableWidgetItem('*' if f.isselect else ' ')
                        elif col == 'st_size':
                            item = QtGui.QTableWidgetItem(approximate_size(int(f.state[col])))
                            item.setTextAlignment(QtCore.Qt.AlignRight | QtCore.Qt.AlignVCenter)
                        else:
                            item = QtGui.QTableWidgetItem(f.state[col])

                        item.setTextColor(self.textcolor(f, col))
                    self.tablewidget.setItem(i, j, item)
            self.tablewidget.selectRow(viewmodel.cursor)
            self.tablewidget.resizeRowsToContents()


class KeyPressEater(QtCore.QObject):

    def __init__(self, viewmodel):
        QtCore.QObject.__init__(self)
        self.viewmodel = viewmodel

    def eventFilter(self, obj, event):
        if event.type() == QtCore.QEvent.KeyPress:
            if self.viewmodel.do_keyevent(event.key(), event.modifiers()):
                return True
            else:
                return QtCore.QObject.eventFilter(self, obj, event)
        else:
            # standard event processing
            return QtCore.QObject.eventFilter(self, obj, event)


class TwoScreenFilerWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        viewmodel.attach(self)
        self.setup_ui(viewmodel)

    def update(self, viewmodel, event):
        self.views[viewmodel.focus].tablewidget.setFocus(Qt.OtherFocusReason)

    def setup_ui(self, viewmodel):
        panel_layout = QtGui.QHBoxLayout()
        panel_layout.setContentsMargins(0, 0, 0, 0)
        panel_layout.setSpacing(0)
        self.setLayout(panel_layout)

        self.leftWidget = FilerWidget(viewmodel.left)
        self.rightWidget = FilerWidget(viewmodel.right)
        self.views = (self.leftWidget, self.rightWidget)
        panel_layout.addWidget(self.leftWidget)
        panel_layout.addWidget(self.rightWidget)

        self.update(viewmodel, 'update')


class LogWidet(QtGui.QPlainTextEdit):
    def __init__(self, parent=None):
        QtGui.QPlainTextEdit.__init__(self, parent=parent)
        self.setReadOnly(True)

        #出力のフォーマットを定義
        formatter = logging.Formatter('%(message)s')
        lw = logging.StreamHandler(stream=self)
        lw.terminator = ''
        lw.setLevel(logging.INFO)
        lw.setFormatter(formatter)
        logging.getLogger().addHandler(lw)

    def write(self, msg):
        if msg != '':
            self.appendPlainText(msg)

    def flush(self):
        pass


class CommandLineWidget(QtGui.QLineEdit):
    def __init__(self, viewmodel, parent=None):
        QtGui.QLineEdit.__init__(self, parent=parent)
        self.textEdited.connect(self.text_edited)
        self.viewmodel = viewmodel
        viewmodel.attach(self)

    def update(self, model, event):
        if event.kind == 'mode':
            if model.mode == COMMAND_MODE:
                self.setFocus(Qt.OtherFocusReason)
            elif model.mode == SEARCH_MODE:
                self.setFocus(Qt.OtherFocusReason)
            elif model.mode == SH_MODE:
                self.setFocus(Qt.OtherFocusReason)
            else:
                self.clear()
                self.clearFocus()

    @QtCore.pyqtSlot(str)
    def text_edited(self, fstr):
        self.viewmodel.commandline_edited(fstr)

class CentralWidget(QtGui.QWidget):
    def __init__(self, viewmodel, eventfilter, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        self.setup_ui(viewmodel)
        viewmodel.attach(self)
        self.viewmodel = viewmodel

    @QtCore.pyqtProperty(str)
    def mode(self):
        return self.viewmodel.mode

    def update(self, viewmodel, event):
        if event.kind == 'mode':
            self.modeLable.setText(viewmodel.mode.upper())

    def setup_ui(self, viewmodel):
        panel = QtGui.QVBoxLayout()
        panel.setContentsMargins(0, 0, 0, 0)
        panel.setSpacing(0)
        self.setLayout(panel)

        self.tab = TabWidget(viewmodel)
        panel.addWidget(self.tab)

        log = LogWidet()
        log.setMaximumHeight(100)
        panel.addWidget(log)

        panel2 = QtGui.QHBoxLayout()
        panel2.setContentsMargins(0, 0, 0, 0)
        panel2.setSpacing(0)
        self.modeLable = QtGui.QLabel(viewmodel.mode.upper())
        self.commandLine = CommandLineWidget(viewmodel)
        panel2.addWidget(self.modeLable)
        panel2.addWidget(self.commandLine)
        panel.addLayout(panel2)


class TabWidget(QtGui.QTabWidget):
    class ChdirObserver():
        def __init__(self, tabwidget, index, filermodel):
            self.tabwidget = tabwidget
            self.index = index
            filermodel.attach(self)

        def update(self, viewmodel, event):
            if event.kind == 'chdir':
                self.tabwidget.setTabText(self.index, viewmodel.displayname)

    def __init__(self, viewmodel, parent=None):
        QtGui.QTabWidget.__init__(self, parent=parent)
        viewmodel.attach(self)
        self.viewmodel = viewmodel
        self.chdir_observers = []
        self.setup_ui(viewmodel)

    def update(self, viewmodel, event):
        if event.kind == 'add':
            self.addTabWithPath("./")
        elif event.kind == 'tabchange':
            self.setCurrentIndex(viewmodel.currentIndex)
        elif event.kind == 'remove':
            self.removeTab(event.opt['index'])
            self.setCurrentIndex(viewmodel.currentIndex)

    def setup_ui(self, viewmodel):
        self.addTabWithPath("./")

    def addTabWithPath(self, path):
        self.addTab(TwoScreenFilerWidget(self.viewmodel.currentTab()),
                self.viewmodel.currentTab().displayname)
        self.setCurrentIndex(self.viewmodel.currentIndex)

        chdir_observer = TabWidget.ChdirObserver(self, self.viewmodel.currentIndex,
                self.viewmodel.currentTab())
        self.viewmodel.currentTab().attach(chdir_observer)
        self.chdir_observers.append(chdir_observer)

class View(QtCore.QObject):
    def __init__(self, model):
        QtCore.QObject.__init__(self)
        self.app = QtGui.QApplication(sys.argv)
        self.kpe = KeyPressEater(model)
        self.app.installEventFilter(self.kpe)
        self.main_window = QtGui.QMainWindow()

        self.cw = CentralWidget(model, self.kpe)
        self.main_window.setCentralWidget(self.cw)
        self.load_stylesheet()

        model.attach(self)

        self.main_window.show()

    def update(self, model, event):
        if event.kind == 'mode':
            self.load_stylesheet()

    def set_style(self, style):
        self.app.setStyle(style)

    def load_stylesheet(self):
        with open('style.qss') as qss:
            self.app.setStyleSheet(qss.read())

    def set_window_title(self, title):
        self.main_window.setWindowTitle(title)

    def set_defaultfont(self, family, *args, **kwargs):
        self.app.setFont(QtGui.QFont(family, *args, **kwargs))

    def set_window_size(self, width, height):
        self.main_window.resize(width, height)

    def set_window_maximized(self):
        self.main_window.showMaximized()

    def set_fileiconenable(self, isenable):
        self.cw.tab

    def load_config(self):
        lispy.load('lib/builtins.scm')
        lispy.load(os.path.expanduser('.pyfilerrc'))

    def mainloop(self):
        self.app.exec_()


def main():
    #rootロガーを取得
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)
    #出力のフォーマットを定義
    formatter = logging.Formatter('%(asctime)s %(levelname)-8s %(filename)s#%(funcName)s(%(lineno)d) - %(message)s')

    #sys.stderrへ出力するハンドラーを定義
    sh = logging.handlers.TimedRotatingFileHandler('log/pyfiler.log', when='D', interval=1, backupCount=5)
    sh.setLevel(logging.DEBUG)
    sh.setFormatter(formatter)
    logger.addHandler(sh)

    model = Model()
    view = View(model)
    command.init(view, model)
    view.load_config()
    view.mainloop()

if __name__ == '__main__':
    main()
