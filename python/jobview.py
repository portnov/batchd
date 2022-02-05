
from PyQt5 import QtGui, QtCore, QtWidgets

import common
import jobedit

class JobView(QtWidgets.QDialog):
    def __init__(self, job, jobtype, parent=None):
        QtWidgets.QDialog.__init__(self, parent)
        self.job = job
        self.jobtype = jobtype
        self.layout = QtWidgets.QFormLayout()
        self.setLayout(self.layout)

        self.id_editor = self._line_editor('id', "ID:")
        self.seq_editor = self._line_editor('seq', "Seq:")
        self.type_editor = self._line_editor('type', "Type:")
        self.host_editor = self._line_editor('host_name', "Host:")
        self.status_editor = self._line_editor('status', "Status:")

        job_params = job['params']
        print(job_params)
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
        editor = QtWidgets.QLineEdit(self)
        self.layout.addRow(title, editor)
        editor.setText(common.format_time(self.job[name]))
        editor.setReadOnly(True)
        return editor

    def _text_editor(self, name, title):
        editor = QtWidgets.QTextEdit(self)
        self.layout.addRow(title, editor)
        value = self.job[name]
        if isinstance(value, str):
            value = value
        if value:
            editor.setText(value)
        editor.setReadOnly(True)
        return editor

