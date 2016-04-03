
from PyQt4 import QtGui, QtCore

class InputFileWidget(QtGui.QWidget):
    def __init__(self, parent=None, readonly=False):
        QtGui.QWidget.__init__(self, parent)
        layout = QtGui.QHBoxLayout()
        self.line = QtGui.QLineEdit(self)
        layout.addWidget(self.line)
        if not readonly:
            self.button = QtGui.QPushButton(QtGui.QIcon.fromTheme('document-open'), "Browse...", self)
            layout.addWidget(self.button)
            self.button.clicked.connect(self._on_browse)
        self.setLayout(layout)

    def _on_browse(self):
        path = QtGui.QFileDialog.getOpenFileName()
        if path:
            self.line.setText(path)

    def text(self):
        return self.line.text()
    
    def setText(self, path):
        self.line.setText(path)

    def setReadOnly(self, val):
        self.line.setReadOnly(val)

class OutputFileWidget(QtGui.QWidget):
    def __init__(self, parent=None, readonly=False):
        QtGui.QWidget.__init__(self, parent)
        layout = QtGui.QHBoxLayout()
        self.line = QtGui.QLineEdit(self)
        layout.addWidget(self.line)
        if not readonly:
            self.button = QtGui.QPushButton(QtGui.QIcon.fromTheme('document-save'), "Browse...", self)
            layout.addWidget(self.button)
            self.button.clicked.connect(self._on_browse)
        self.setLayout(layout)

    def _on_browse(self):
        path = QtGui.QFileDialog.getSaveFileName()
        if path:
            self.line.setText(path)

    def text(self):
        return self.line.text()
    
    def setText(self, path):
        self.line.setText(path)

    def setReadOnly(self, val):
        self.line.setReadOnly(val)

class IntEditor(QtGui.QSpinBox):
    def setText(self, text):
        val = int(text)
        self.setValue(val)

def create_widget(param, parent=None, readonly=False):
    title = param['title']

    param_type = param['type']

    if param_type == 'InputFile':
        widget = InputFileWidget(parent, readonly)
    elif param_type == 'OutputFile':
        widget = OutputFileWidget(parent, readonly)
    elif param_type == 'String':
        widget = QtGui.QLineEdit(parent)
    elif param_type == 'Integer':
        widget = IntEditor(parent)
    else:
        raise Exception("Unknown parameter type: " + param_type)

    dflt = param['default']
    if dflt:
        widget.setText(dflt)

    widget.setReadOnly(readonly)
    
    return title, widget

def create_form(params, widgets_dict, parent=None, readonly=False):
    result = QtGui.QWidget(parent)
    layout = QtGui.QFormLayout()
    result.setLayout(layout)

    for param in params:
        title, widget = create_widget(param, parent=parent, readonly=readonly)
        widgets_dict[param['name']] = widget
        layout.addRow(title, widget)

    return result

