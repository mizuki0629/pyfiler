#! /usr/bin/env python
# coding: utf-8

import logging
import sys
import PyQt4.QtCore as QtCore
from PyQt4.QtCore import Qt
import PyQt4.QtGui as QtGui
from filerview import KeyEventHandler
from filerview import TabViewModel
from filerview import Subject

# TODO まとめる
horizontal_header = ['s', 'filename', 'filemode', 'st_ctime', 'st_size', ]
header_resizemode = [
        (QtGui.QHeaderView.Fixed, 15),
        (QtGui.QHeaderView.Stretch, 0),
        (QtGui.QHeaderView.ResizeToContents, 0),
        (QtGui.QHeaderView.ResizeToContents, 0),
        (QtGui.QHeaderView.ResizeToContents, 0),
        ]

# TODO 適当なところに移動させる
SUFFIXES = {1000: ['KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB'],
            1024: ['KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB']}

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
        size /= multiple
        if size < multiple:
            return '{0:.1f} {1}'.format(size, suffix)

    raise ValueError('number too large')

class FilerWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        viewmodel.attach(self)
        self.setup_ui(viewmodel)

    def setup_ui(self, viewmodel):
        panel_layout = QtGui.QVBoxLayout()
        self.setLayout(panel_layout)
        panel_layout.setContentsMargins(0, 0, 0, 0)

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

        self.update(viewmodel, Subject.Event('update'))

    def update(self, viewmodel, event):
        if event.kind == 'cursor':
            self.tablewidget.selectRow(viewmodel.cursor)
        elif event.kind == 'select':
            for i in event.opt['indexes']:
                # 選択時
                if viewmodel.files[i].isselect:
                    color = QtGui.QColor(150, 150, 0)
                    val = '*'
                else:
                    color = QtGui.QColor(0, 0, 0)
                    val = ' '
                for j, col in enumerate(horizontal_header):
                    if col == 's':
                        item = QtGui.QTableWidgetItem(val)
                        self.tablewidget.setItem(i, j, item)
                    self.tablewidget.item(i, j).setTextColor(color)
        else:
            self.cwdline.setText(viewmodel.cwd())

            self.tablewidget.setColumnCount(len(horizontal_header))
            self.tablewidget.setRowCount(len(viewmodel.files))

            for i, m in enumerate(header_resizemode):
                self.tablewidget.horizontalHeader().setResizeMode(i, m[0])
                self.tablewidget.setColumnWidth(i, m[1])

            self.tablewidget.setHorizontalHeaderLabels(horizontal_header)
            self.tablewidget.verticalHeader().setVisible(False)

            for i, f in enumerate(viewmodel.files):
                for j, col in enumerate(horizontal_header):
                    item = None
                    if col in f.state:
                        if col == 'filename':
                            icon = QtGui.QFileIconProvider().icon(QtCore.QFileInfo(f.state['abspath']))
                            item = QtGui.QTableWidgetItem(icon, f.state[col])
                        elif col == 's':
                            item = QtGui.QTableWidgetItem('*' if f.isselect else ' ')
                        elif col == 'st_size':
                            item = QtGui.QTableWidgetItem(approximate_size(int(f.state[col])))
                            item.setTextAlignment(QtCore.Qt.AlignRight | QtCore.Qt.AlignVCenter)
                        else:
                            item = QtGui.QTableWidgetItem(f.state[col])
                    # 選択時
                    if f.isselect:
                        item.setBackgroundColor(QtGui.QColor(255, 255, 0))
                    self.tablewidget.setItem(i, j, item)
            self.tablewidget.selectRow(viewmodel.cursor)
            self.tablewidget.resizeRowsToContents()


class KeyPressEater(QtCore.QObject):

    commandLineMode = QtCore.pyqtSignal(bool)

    def __init__(self, handler):
        QtCore.QObject.__init__(self)
        self.handler = handler

    @QtCore.pyqtSlot(str)
    def commandEditingFinished(self, command):
        self.handler.do_command(command)

    @QtCore.pyqtSlot(bool)
    def commandLineFocussed(self, focussed):
        self.handler.commandLineMode = focussed
        self.commandLineMode.emit(focussed)

    # TODO セミコロンを押したときにどうやってコマンドラインにフォーカス移す？
    def eventFilter(self, obj, event):
        if event.type() == QtCore.QEvent.KeyPress:
            return self.handler.on_key_press(event)
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

        #rootロガーを取得
        logger = logging.getLogger()
        logger.setLevel(logging.DEBUG)
        #出力のフォーマットを定義
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')

        #sys.stderrへ出力するハンドラーを定義
        sh = logging.StreamHandler()
        sh.setLevel(logging.DEBUG)
        sh.setFormatter(formatter)
        logger.addHandler(sh)

        lw = logging.StreamHandler(stream=self)
        lw.terminator = ''
        lw.setLevel(logging.DEBUG)
        lw.setFormatter(formatter)
        logger.addHandler(lw)

    def write(self, msg):
        if msg != '':
            self.appendPlainText(msg)

    def flush(self):
        pass


class CommandLineWidget(QtGui.QLineEdit):
    focussed = QtCore.pyqtSignal(bool)
    commandEditingFinished = QtCore.pyqtSignal(str)

    def __init__(self, parent=None):
        QtGui.QLineEdit.__init__(self, parent=parent)
        self.returnPressed.connect(self.command_editing_finished)

    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Escape:
            self.clearFocus()
        else:
            QtGui.QLineEdit.keyPressEvent(self, event)

    @QtCore.pyqtSlot()
    def command_editing_finished(self):
        self.commandEditingFinished.emit(self.text())
        self.clearFocus()

    def focusInEvent(self, e):
        self.focussed.emit(True)

    def focusOutEvent(self, e):
        self.focussed.emit(False)
        self.clear()


class CentralWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        self.setup_ui(viewmodel)

    @QtCore.pyqtSlot(bool)
    def commandlog(self, focussed):
        logging.debug("focus " + str(focussed))

    def setup_ui(self, viewmodel):
        panel = QtGui.QVBoxLayout()
        panel.setContentsMargins(0, 0, 0, 0)
        self.setLayout(panel)

        tab = TabWidget(viewmodel)
        panel.addWidget(tab)

        log = LogWidet()
        log.setMaximumHeight(100)
        panel.addWidget(log)

        self.commandLine = CommandLineWidget()
        panel.addWidget(self.commandLine)
        self.commandLine.focussed.connect(self.commandlog)


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
        elif event.kind == 'prev':
            self.setCurrentIndex(viewmodel.currentIndex)
        elif event.kind == 'next':
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


def main():
    app = QtGui.QApplication(sys.argv)
    vm = TabViewModel()
    handler = KeyEventHandler(vm)
    kpe = KeyPressEater(handler)
    app.installEventFilter(kpe)
    main_window = QtGui.QMainWindow()
    main_window.setWindowTitle("pyfiler")

    cw = CentralWidget(vm)
    main_window.setCentralWidget(cw)

    cw.commandLine.focussed.connect(kpe.commandLineFocussed)
    cw.commandLine.commandEditingFinished.connect(kpe.commandEditingFinished)
    #kpe.comanndLineMode.connect()

    main_window.show()
    app.exec_()

if __name__ == '__main__':
    main()
