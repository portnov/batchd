
from PyQt5 import QtGui, QtCore, QtWidgets

import common
from batchd.client import InsufficientRightsException

class QueueEditor(QtWidgets.QDialog):
    def __init__(self, parent, queue=None):
        QtWidgets.QDialog.__init__(self, parent)
        self.parent = parent
        self.queue = queue
        if self.queue is None:
            self.queue = dict()

        self.layout = QtWidgets.QFormLayout()
        self.setLayout(self.layout)

        self.name_editor = self._line_editor('name', "Name:")
        self.title_editor = self._line_editor('title', "Title:")
        self.enabled_editor = common.mk_checkbox(self, 'enabled', "Enabled:", False)

        self.schedule_popup = QtWidgets.QComboBox(self)
        for schedule in parent.client.get_schedules():
            self.schedule_popup.addItem(schedule['name'])
        self.layout.addRow("Schedule:", self.schedule_popup)

        buttons = QtWidgets.QDialogButtonBox(QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel)
        self.layout.addWidget(buttons)
        buttons.accepted.connect(self._on_ok)
        buttons.rejected.connect(self.reject)

    def _on_ok(self):
        self.queue['name'] = self.name_editor.text().strip()
        self.queue['title'] = self.title_editor.text().strip()
        self.queue['enabled'] = (self.enabled_editor.checkState() == QtCore.Qt.Checked)
        self.queue['schedule_name'] = self.schedule_popup.currentText()
        if not self.queue['name']:
            QtWidgets.QMessageBox.warning(self, "batch client", "Name must be specified", QtWidgets.QMessageBox.Close)
            return
        if not self.queue['title']:
            QtWidgets.QMessageBox.warning(self, "batch client", "Title must be specified", QtWidgets.QMessageBox.Close)
            return
            
        print(self.queue)
        try:
            self.parent.client.new_queue(self.queue)
        except InsufficientRightsException as e:
            QtWidgets.QMessageBox.critical(self, "batch client", "Insufficient privileges to " + str(e), QtWidgets.QMessageBox.Close)
            self.reject()
        else:
            self.accept()
    
    def _line_editor(self, name, title):
        return common.mk_line_editor(self, name, title, self.queue.get(name, None), readonly=False)


