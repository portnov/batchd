from PyQt5 import QtGui, QtCore, QtWidgets
from datetime import datetime

def format_time(value):
    if value:
        try:
            d = datetime.strptime(value, "%Y-%m-%dT%H:%M:%S.%fZ")
        except ValueError:
            if len(value) > 26:
                d = datetime.strptime(value[0:26], "%Y-%m-%dT%H:%M:%S.%f")
            else:
                raise
        return datetime.strftime(d, "%c")#.encode( "utf-8")
    else:
        return "<undefined>"

def mk_line_editor(parent, name, title, value, readonly=False):
    editor = QtWidgets.QLineEdit(parent)
    parent.layout.addRow(title, editor)
    if isinstance(value, int):
        value = str(value)
    elif isinstance(value, str):
        value = value
    if value:
        editor.setText(value)
    editor.setReadOnly(readonly)
    return editor

def mk_checkbox(parent, name, title, value, readonly=False):
    checkbox = QtWidgets.QCheckBox(parent)
    parent.layout.addRow(title, checkbox)
    st = QtCore.Qt.Checked if value else QtCore.Qt.Unchecked
    checkbox.setCheckState(st)
    checkbox.setEnabled(not readonly)
    return checkbox

