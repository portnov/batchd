#!/usr/bin/python

import sys
import os
import getpass
from os.path import isfile, join, dirname
from PyQt5 import QtGui, QtCore, QtWidgets

import queuetable
import jobview
import jobedit
import queues as qeditor
from batchd.client import Client, InsufficientRightsException

APPDIR = dirname(sys.argv[0])

def labelled(label, constructor, parent=None):
    result = QtWidgets.QWidget(parent)
    layout = QtWidgets.QHBoxLayout()
    result.setLayout(layout)
    lbl = QtWidgets.QLabel(label)
    layout.addWidget(lbl)
    widget = constructor(result)
    layout.addWidget(widget)
    return result, widget

def get_icon(name):
    path = join(APPDIR, "icons", name)
    return QtGui.QIcon(path)
    
class LoginBox(QtWidgets.QDialog):
    def __init__(self, url, cfg, parent=None):
        QtWidgets.QDialog.__init__(self, parent)

        self.url = url
        self.client = None

        self.config = cfg

        form = QtWidgets.QFormLayout()
        vbox = QtWidgets.QVBoxLayout()
        self.setLayout(vbox)
        self.login = QtWidgets.QLineEdit(self)
        if 'username' in cfg:
            username = cfg['username']
        else:
            username = getpass.getuser()
        self.login.setText(username)
        self.password = QtWidgets.QLineEdit(self)
        self.password.setEchoMode(QtWidgets.QLineEdit.Password)
        if 'password' in cfg:
            self.password.setText(cfg['password'])
        form.addRow("User name:", self.login)
        form.addRow("Password:", self.password)
        vbox.addLayout(form)
        bbox = QtWidgets.QDialogButtonBox(self)
        ok = QtWidgets.QPushButton('Ok')
        ok.clicked.connect(self.on_ok)
        cancel = QtWidgets.QPushButton('Cancel')
        cancel.clicked.connect(self.on_cancel)
        bbox.addButton(ok, QtWidgets.QDialogButtonBox.AcceptRole)
        bbox.addButton(cancel, QtWidgets.QDialogButtonBox.RejectRole)
        vbox.addWidget(bbox)
        self.setAttribute(QtCore.Qt.WA_DeleteOnClose)

    def on_ok(self):
        try:
            client = Client.from_config(self.config)
            client.username = self.login.text()
            client.password = self.password.text()
            client.get_queues()
            self.client = client
            self.accept()
        except InsufficientRightsException as e:
            print(e)

    def on_cancel(self):
        self.client = None
        self.reject()

class GUI(QtWidgets.QMainWindow):
    def __init__(self, client):
        QtWidgets.QMainWindow.__init__(self)

        self.url = client.manager_url
        self.client = client

        central_widget = QtWidgets.QWidget(self)

        self.layout = QtWidgets.QVBoxLayout()
        central_widget.setLayout(self.layout)

        self.setCentralWidget(central_widget)

        wrapper = QtWidgets.QWidget(self)
        hbox = QtWidgets.QHBoxLayout()
        wrapper.setLayout(hbox)
        lbl = QtWidgets.QLabel("Queue:", wrapper)
        hbox.addWidget(lbl)
        self.queue_popup = QtWidgets.QComboBox(wrapper)
        hbox.addWidget(self.queue_popup, stretch=1)

        self._fill_queues()
        self.queue_popup.currentIndexChanged.connect(self._on_select_queue)
        self.layout.addWidget(wrapper)

        queue_buttons = QtWidgets.QToolBar(self)
        queue_buttons.addAction(get_icon("list-add.svg"), "New queue", self._on_add_queue)
        self.enable_queue = QtWidgets.QAction(get_icon("checkbox.svg"), "Enable", self)
        self.enable_queue.setCheckable(True)
        self.enable_queue.toggled.connect(self._on_queue_toggle)
        queue_buttons.addAction(self.enable_queue)
        hbox.addWidget(queue_buttons)

        self.queue_info = QtWidgets.QLabel(self)
        self.layout.addWidget(self.queue_info)

        buttons = QtWidgets.QToolBar(self)
        buttons.addAction(get_icon("quickview.svg"), "View", self._on_view)
        buttons.addAction(get_icon("edit-delete.svg"), "Delete", self._on_delete)
        self.layout.addWidget(buttons)

        self.qtable = queuetable.Table(parent=self)
        self.qtable.doubleClicked.connect(self._on_qtable_doubleclick)
        self.layout.addWidget(self.qtable)

        wrapper, self.type_popup = labelled("Job type:", QtWidgets.QComboBox, self)
        self.types = types = self.client.get_job_types()
        self.type_by_name = {}
        for t in types:
            name = t['name']
            title = t.get('title', name)
            if not title:
                title = name
            item = QtGui.QStandardItem(name)
            item.setData(title, QtCore.Qt.DisplayRole)
            self.type_popup.model().appendRow(item)
            self.type_by_name[name] = t
        self.type_popup.currentIndexChanged.connect(self._on_select_type)
        self.layout.addWidget(wrapper)

        ok = QtWidgets.QPushButton(get_icon("list-add.svg"), "Add", self)
        ok.clicked.connect(self._on_ok)
        self.layout.addWidget(ok)

        self.param_widgets = {}
        self.form = None

        self._on_select_type(0)
        self._on_select_queue(0)

        timer = QtCore.QTimer(self)
        timer.timeout.connect(self._on_timer)
        timer.start(5*1000)

    def _fill_queues(self):
        self.queue_popup.clear()
        self.queues = queues = self.client.get_queues()
        for q in queues:
            enabled = "*" if q['enabled'] else " "
            title = "[{0}] {1}".format(enabled, q['title'])
            self.queue_popup.addItem(title, q['name'])

    def _on_view(self):
        job = self.qtable.currentJob()
        self._view_job(job)

    def _on_qtable_doubleclick(self, model_index):
        job = self.qtable.jobByIndex(model_index)
        self._view_job(job)

    def _view_job(self, job):
        job_results = self.client.get_job_results(job['id'])
        jobtype = self.type_by_name[job['type']]
        dlg = jobview.JobView(job, jobtype, job_results, parent=self)
        dlg.exec_()

    def _on_queue_toggle(self):
        enabled = self.enable_queue.isChecked()
        print(enabled)

    def _on_delete(self):
        buttons = QtWidgets.QMessageBox.Yes | QtWidgets.QMessageBox.No
        job = self.qtable.currentJob()
        job_id = job['id']
        ok = QtWidgets.QMessageBox.question(self, "Delete?",
                                        "Are you really sure you want to delete job #{}?".format(job_id),
                                        buttons)
        if ok == QtWidgets.QMessageBox.Yes:
            print("Deleting!")
            self.client.delete_job(job_id)
            self._refresh_queue()
        else:
            print("do not delete")

    def _on_select_type(self, idx):
        jobtype = self.types[idx]
        self.param_widgets = {}
        form = jobedit.create_form(jobtype['params'], self.param_widgets, self)

        if self.form:
            self.form.hide()
            self.layout.removeWidget(self.form)
            del self.form
        self.form = form
        self.layout.insertWidget(5, form)
        self.form.show()

    def _on_add_queue(self):
        dlg = qeditor.QueueEditor(self)
        dlg.exec_()
        self._fill_queues()

    def _on_select_queue(self, idx):
        self._refresh_queue(idx)

    def _on_timer(self):
        self._refresh_queue()

    def _refresh_queue(self, idx=None):
        if idx is None:
            idx = self.queue_popup.currentIndex()

        if len(self.queues) == 0:
            print("No queues.")
            return

        queue = self.queues[idx]
        schedule = queue['schedule_name']
        host = queue['host_name']
        if not host:
            host = "*"
        stats = self.client.get_queue_stats(queue['name'])
        new = stats.get('new', 0)
        processing = stats.get('processing', 0)
        done = stats.get('done', 0)
        failed = stats.get('failed', 0)
        info = "Schedule: {}\nHost: {}\nNew/Processing/Done: {} / {} / {}\nFailed: {}".format(schedule, host, new, processing, done, failed)
        self.queue_info.setText(info)
        self.enable_queue.setChecked(queue['enabled'])

        jobs = self.client.get_jobs(queue['name'])
        self.qtable.setJobs(jobs)

    def _on_ok(self):
        queue_idx = self.queue_popup.currentIndex()
        queue_name = self.queues[queue_idx]['name']
        #typename =  self.type_popup.currentText()
        jobtype = self.types[self.type_popup.currentIndex()]
        typename = jobtype['name']
        params = {}
        for name, widget in self.param_widgets.items():
            params[name] = widget.text()
        self.client.do_enqueue(queue_name, typename, None, params)
        self._refresh_queue()

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    cfg = Client.load_config()
    client = Client.from_config(cfg)

    auth_ok = False
    if client.need_password:
        login_box = LoginBox(client.manager_url, cfg)
        if login_box.exec_():
            client = login_box.client
            auth_ok = True
    else:
        auth_ok = True

    if auth_ok:
        gui = GUI(client)
        gui.show()
        sys.exit(app.exec_())

