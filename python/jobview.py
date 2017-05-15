
from PyQt4 import QtGui, QtCore

import common
import jobedit

class JobView(QtGui.QDialog):
    def __init__(self, job, jobtype, parent=None):
        QtGui.QDialog.__init__(self, parent)
        self.job = job
        self.jobtype = jobtype
        self.layout = QtGui.QFormLayout()
        self.setLayout(self.layout)

        self.id_editor = self._line_editor('id', "ID:")
        self.seq_editor = self._line_editor('seq', "Seq:")
        self.type_editor = self._line_editor('type', "Type:")
        self.status_editor = self._line_editor('status', "Status:")

        job_params = job['params']
        print job_params
        for param in jobtype['params']:
            title, widget = jobedit.create_widget(param, parent=self, readonly=True)
            name = param['name']
            widget.setText(job_params.get(name, ""))
            self.layout.addRow(title, widget)

        self.create_time_editor = self._time_editor('create_time', "Created:")
        self.result_time_editor = self._time_editor('result_time', "Finished:")
        self.exitcode_editor = self._line_editor('exit_code', "Exit code:")

        self.stdout_editor = self._text_editor('stdout', "Output:")
        self.stderr_editor = self._text_editor('stderr', "Errors:")

    def _line_editor(self, name, title):
        return common.mk_line_editor(self, name, title, self.job[name], readonly=True)

    def _time_editor(self, name, title):
        editor = QtGui.QLineEdit(self)
        self.layout.addRow(title, editor)
        editor.setText(common.format_time(self.job[name]))
        editor.setReadOnly(True)
        return editor

    def _text_editor(self, name, title):
        editor = QtGui.QTextEdit(self)
        self.layout.addRow(title, editor)
        value = self.job[name]
        if isinstance(value, str):
            value = unicode(value, "utf-8")
        if value:
            editor.setText(value)
        editor.setReadOnly(True)
        return editor

