
from PyQt5 import QtGui, QtCore, QtWidgets

import common
import jobedit

class JobView(QtWidgets.QDialog):
    def __init__(self, job, jobtype, job_results, parent=None):
        QtWidgets.QDialog.__init__(self, parent)
        self.job = job
        self.jobtype = jobtype
        self.job_results = job_results
        self.layout = QtWidgets.QFormLayout()
        self.setLayout(self.layout)

        self.id_editor = self._line_editor('id', "ID:")
        self.seq_editor = self._line_editor('seq', "Seq:")
        self.type_editor = self._line_editor('type', "Type:")
        self.host_editor = self._line_editor('host_name', "Host:")
        self.status_editor = self._line_editor('status', "Status:")

        job_params = job['params']
        for param in jobtype['params']:
            title, widget = jobedit.create_widget(param, parent=self, readonly=True)
            name = param['name']
            widget.setText(job_params.get(name, ""))
            self.layout.addRow(title, widget)

        self.create_time_editor = self._time_editor('create_time', "Created:")
        self.result_time_editor = self._time_editor('result_time', "Finished:")
        self.exitcode_editor = self._line_editor('exit_code', "Exit code:")

        self.stdout_editor = self._text_editor("Output:")
        self.stderr_editor = self._text_editor("Errors:")
        self._setup_outputs()

    def _setup_outputs(self):
        stdout = []
        stderr = []
        for item in self.job_results:
            if item['stdout']:
                stdout.append(item['stdout'])
            if item['stderr']:
                stderr.append(item['stderr'])
        self.stdout_editor.setText("\n".join(stdout))
        self.stderr_editor.setText("\n".join(stderr))

    def _line_editor(self, name, title):
        return common.mk_line_editor(self, name, title, self.job[name], readonly=True)

    def _time_editor(self, name, title):
        editor = QtWidgets.QLineEdit(self)
        self.layout.addRow(title, editor)
        editor.setText(common.format_time(self.job[name]))
        editor.setReadOnly(True)
        return editor

    def _text_editor(self, title):
        editor = QtWidgets.QTextEdit(self)
        self.layout.addRow(title, editor)
        editor.setReadOnly(True)
        return editor

