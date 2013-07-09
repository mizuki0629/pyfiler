#! /usr/bin/env python
# coding: utf-8

import sys
import PyQt4.QtCore as QtCore
import PyQt4.QtGui as QtGui
import filerview

horizontal_header = ['filename', 'filemode', 'st_ctime', 'st_atime', 'st_size', ]

class FilerWidget(QtGui.QWidget):
    def __init__(self, filer, parent=None):
        QtGui.QWidget.__init__(self, parent=parent)
        self.filer = filer
        self.setup_ui()

    def setup_ui(self):
        panel_layout = QtGui.QHBoxLayout()
        self.setLayout(panel_layout)
        panel_layout.setContentsMargins(0, 0, 0, 0)

        tablewidget = QtGui.QTableWidget()
        panel_layout.addWidget(tablewidget)

        tablewidget.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        tablewidget.setShowGrid(False)
        tablewidget.setGridStyle(QtCore.Qt.NoPen)
        tablewidget.setColumnCount(len(horizontal_header))
        tablewidget.setRowCount(len(self.filer.files))

        tablewidget.setHorizontalHeaderLabels(horizontal_header)
        tablewidget.verticalHeader().setVisible(False)

        for i, f in enumerate(self.filer.files):
            for j, col in enumerate(horizontal_header):
                if col == 'filename':
                    icon = QtGui.QFileIconProvider().icon(QtCore.QFileInfo(f.state['abspath']))
                    tablewidget.setItem(i, j, QtGui.QTableWidgetItem(icon, f.state[col]))
                else:
                    tablewidget.setItem(i, j, QtGui.QTableWidgetItem(f.state[col]))

def main():
    app = QtGui.QApplication(sys.argv)

    filer = filerview.FilerView()

    panel = QtGui.QWidget()
    leftfiler = FilerWidget(filer, panel)
    rightfiler = FilerWidget(filer, panel)

    panel_layout = QtGui.QHBoxLayout()
    panel_layout.addWidget(leftfiler)
    panel_layout.addWidget(rightfiler)
    panel_layout.setContentsMargins(0, 0, 0, 0)
    panel.setLayout(panel_layout)

    main_window = QtGui.QMainWindow()
    main_window.setWindowTitle("pyfiler")
    main_window.setCentralWidget(panel)

    main_window.show()
    app.exec_()

if __name__ == '__main__':
    main()
