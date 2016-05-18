
from PyQt4 import QtGui, QtCore

import common

class QueueEditor(QtGui.QDialog):
    def __init__(self, parent, queue=None):
        QtGui.QDialog.__init__(self, parent)
        self.parent = parent
        self.queue = queue
        if self.queue is None:
            self.queue = dict()

        self.layout = QtGui.QFormLayout()
        self.setLayout(self.layout)

        self.name_editor = self._line_editor('name', "Name:")
        self.title_editor = self._line_editor('title', "Title:")
        self.enabled_editor = common.mk_checkbox(self, 'enabled', "Enabled:", False)

        self.schedule_popup = QtGui.QComboBox(self)
        for schedule in parent.get_schedules():
            self.schedule_popup.addItem(schedule['name'])
        self.layout.addRow("Schedule:", self.schedule_popup)

        buttons = QtGui.QDialogButtonBox(QtGui.QDialogButtonBox.Ok | QtGui.QDialogButtonBox.Cancel)
        self.layout.addWidget(buttons)
        buttons.accepted.connect(self._on_ok)
        buttons.rejected.connect(self.reject)

    def _on_ok(self):
        self.queue['name'] = unicode( self.name_editor.text() )
        self.queue['title'] = unicode( self.title_editor.text() )
        self.queue['enabled'] = (self.enabled_editor.checkState() == QtCore.Qt.Checked)
        self.queue['schedule_name'] = unicode( self.schedule_popup.currentText() )
        print self.queue
        self.parent.new_queue(self.queue)
        self.accept()
    
    def _line_editor(self, name, title):
        return common.mk_line_editor(self, name, title, self.queue.get(name, None), readonly=False)


