from PyQt4 import QtGui, QtCore
from datetime import datetime

def format_time(value):
    if value:
        d = datetime.strptime(value, "%Y-%m-%dT%H:%M:%S.%fZ")
        return unicode(datetime.strftime(d, "%c"), "utf-8")
    else:
        return "<undefined>"

def mk_line_editor(parent, name, title, value, readonly=False):
    editor = QtGui.QLineEdit(parent)
    parent.layout.addRow(title, editor)
    if isinstance(value, int):
        value = str(value)
    elif isinstance(value, str):
        value = unicode(value, "utf-8")
    if value:
        editor.setText(value)
    editor.setReadOnly(readonly)
    return editor

def mk_checkbox(parent, name, title, value, readonly=False):
    checkbox = QtGui.QCheckBox(parent)
    parent.layout.addRow(title, checkbox)
    st = QtCore.Qt.Checked if value else QtCore.Qt.Unchecked
    checkbox.setCheckState(st)
    checkbox.setEnabled(not readonly)
    return checkbox

