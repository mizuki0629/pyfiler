#! /usr/bin/env python
# coding: utf-8

import sys
import PyQt4.QtCore as QtCore
from PyQt4.QtCore import Qt
import PyQt4.QtGui as QtGui
from filerview import KeyEventHandler
from filerview import TwoScreenFilerViewModel

horizontal_header = ['filename', 'filemode', 'st_ctime',
                     'st_atime', 'st_size', ]


class FilerWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        viewmodel.register_observer(self)
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

        self.update(viewmodel)

    def update(self, viewmodel):
        self.cwdline.setText(viewmodel.cwd())

        self.tablewidget.setColumnCount(len(horizontal_header))
        self.tablewidget.setRowCount(len(viewmodel.files))

        self.tablewidget.setHorizontalHeaderLabels(horizontal_header)
        self.tablewidget.verticalHeader().setVisible(False)

        for i, f in enumerate(viewmodel.files):
            for j, col in enumerate(horizontal_header):
                item = None
                if col in f.state:
                    if col == 'filename':
                        icon = QtGui.QFileIconProvider().icon(QtCore.QFileInfo(f.state['abspath']))
                        item = QtGui.QTableWidgetItem(icon, f.state[col])
                    else:
                        item = QtGui.QTableWidgetItem(f.state[col])
                # 選択時
                if f.isselect:
                    item.setBackgroundColor(QtGui.QColor(255, 255, 0))
                self.tablewidget.setItem(i, j, item)
        self.tablewidget.selectRow(viewmodel.cursor)


class KeyPressEater(QtCore.QObject):
    def __init__(self, handler):
        QtCore.QObject.__init__(self)
        self.handler = handler

    def eventFilter(self, obj, event):
        if event.type() == QtCore.QEvent.KeyPress:
            self.handler.on_key_press(event)
            return True
        else:
            # standard event processing
            return QtCore.QObject.eventFilter(self, obj, event)


class TwoScreenFilerWidget(QtGui.QWidget):
    def __init__(self, viewmodel, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        viewmodel.register_observer(self)
        self.setup_ui(viewmodel)

    def update(self, viewmodel):
        self.views[viewmodel.focus].tablewidget.setFocus(Qt.OtherFocusReason)

    def setup_ui(self, viewmodel):
        panel_layout = QtGui.QHBoxLayout()
        self.setLayout(panel_layout)
        panel_layout.setContentsMargins(0, 0, 0, 0)

        self.leftWidget = FilerWidget(viewmodel.left)
        self.rightWidget = FilerWidget(viewmodel.right)
        self.views = (self.leftWidget, self.rightWidget)
        panel_layout.addWidget(self.leftWidget)
        panel_layout.addWidget(self.rightWidget)

        self.update(viewmodel)


def main():
    app = QtGui.QApplication(sys.argv)
    vm = TwoScreenFilerViewModel()
    handler = KeyEventHandler(vm)
    kpe = KeyPressEater(handler)
    app.installEventFilter(kpe)
    tt = TwoScreenFilerWidget(vm)
    main_window = QtGui.QMainWindow()
    main_window.setWindowTitle("pyfiler")
    main_window.setCentralWidget(tt)

    main_window.show()
    app.exec_()

if __name__ == '__main__':
    main()
