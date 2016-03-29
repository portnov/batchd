#!/usr/bin/python

import sys
import requests
import json
from PyQt4 import QtGui

class InputFileWidget(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        layout = QtGui.QHBoxLayout()
        self.line = QtGui.QLineEdit(self)
        layout.addWidget(self.line)
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

class OutputFileWidget(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)
        layout = QtGui.QHBoxLayout()
        self.line = QtGui.QLineEdit(self)
        layout.addWidget(self.line)
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

def create_widget(param, parent=None):
    title = param['title']

    param_type = param['type']

    if param_type == 'InputFile':
        widget = InputFileWidget(parent)
    elif param_type == 'OutputFile':
        widget = OutputFileWidget(parent)
    elif param_type == 'String':
        widget = QtGui.QLineEdit(parent)
    elif param_type == 'Integer':
        widget = QtGui.QSpinBox()
    else:
        raise Exception("Unknown parameter type: " + param_type)

    dflt = param['default']
    if dflt:
        widget.setText(dflt)
    
    return title, widget

def create_form(params, widgets_dict, parent=None):
    result = QtGui.QWidget(parent)
    layout = QtGui.QFormLayout()
    result.setLayout(layout)

    for param in params:
        title, widget = create_widget(param)
        widgets_dict[param['name']] = widget
        layout.addRow(title, widget)

    return result

def get_job_types(url):
    rs = requests.get(url + "/type")
    return json.loads(rs.text)

def get_queues(url):
    rs = requests.get(url + "/queue")
    return json.loads(rs.text)

def do_enqueue(url, qname, typename, params):
    rq = dict(queue = qname, type=typename, params=params)
    rs = requests.put(url+ "/queue/" + qname, data=json.dumps(rq))
    print(rs.text)

class GUI(QtGui.QMainWindow):
    def __init__(self, url, types, queues):
        QtGui.QMainWindow.__init__(self)

        self.url = url

        central_widget = QtGui.QWidget(self)

        self.layout = QtGui.QVBoxLayout()
        central_widget.setLayout(self.layout)

        self.setCentralWidget(central_widget)

        self.type_popup = QtGui.QComboBox(self)
        self.types = types
        for t in types:
            self.type_popup.addItem(t['name'])
        self.type_popup.currentIndexChanged.connect(self._on_select_type)
        self.layout.addWidget(self.type_popup)

        self.queue_popup = QtGui.QComboBox(self)
        self.queues = queues
        for q in queues:
            self.queue_popup.addItem(q['name'])
        self.layout.addWidget(self.queue_popup)

        ok = QtGui.QPushButton("Ok", self)
        ok.clicked.connect(self._on_ok)
        self.layout.addWidget(ok)

        self.param_widgets = {}
        self.form = None

        self._on_select_type(0)

    def _on_select_type(self, idx):
        jobtype = self.types[idx]
        self.param_widgets = {}
        form = create_form(jobtype['params'], self.param_widgets, self)

        if self.form:
            self.form.hide()
            self.layout.removeWidget(self.form)
            del self.form
        self.form = form
        self.layout.insertWidget(2, form)
        self.form.show()

    def _on_ok(self):
        queue = unicode( self.queue_popup.currentText() )
        typename = unicode( self.type_popup.currentText() )
        jobtype = self.types[self.type_popup.currentIndex()]
        params = {}
        for name, widget in self.param_widgets.iteritems():
            params[name] = unicode(widget.text())
        do_enqueue(self.url, queue, typename, params)

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    URL='http://localhost:9681'
    types = get_job_types(URL)
    queues = get_queues(URL)
    gui = GUI(URL, types, queues)
    gui.show()
    sys.exit(app.exec_())

