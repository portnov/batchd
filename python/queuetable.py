
from PyQt5 import QtGui, QtCore, QtWidgets

import common

class Field(object):
    def __init__(self, name, title):
        self.name = name
        self.title = title

    def show(self, value):
        return value

class TimeField(Field):
    def show(self, value):
        return common.format_time(value)

class Model(QtCore.QAbstractTableModel):
    def __init__(self, parent, *fields):
        QtCore.QAbstractTableModel.__init__(self)
        self.jobs = []
        self.fields = fields

    def rowCount(self, parent):
        return len(self.jobs)
    
    def columnCount(self, parent):
        return len(self.fields)
    
    def data(self, index, role):
        if role == QtCore.Qt.DisplayRole and index.isValid():
            job = self.jobs[index.row()]
            names = [f.name for f in self.fields]
            #columns = ['id', 'seq', 'type', 'status']
            value = job[names[index.column()]]
            return self.fields[index.column()].show(value)
        if role == QtCore.Qt.BackgroundColorRole:
            job = self.jobs[index.row()]
            if job['status'] == 'Failed':
                return QtGui.QColor(228, 122, 122)
            elif job['status'] == 'Done':
                return QtGui.QColor(132, 181, 97)
    
    def headerData(self, section, orientation, role):
        if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
            return self.fields[section].title
        if orientation == QtCore.Qt.Vertical and role == QtCore.Qt.DisplayRole:
            return section+1
    
    def setupModelData(self, jobs):
        self.beginResetModel()
        self.jobs = jobs
        self.endResetModel()

class Table(QtWidgets.QTableView):
    def __init__(self, jobs=None, parent=None):
        QtWidgets.QTableView.__init__(self, parent)
        self.model = Model(self,
                           Field('id', "ID"),
                           Field('seq', "Seq"),
                           Field('type', "Type"),
                           Field('status', "Status"),
                           TimeField('create_time', "Created"),
                           TimeField('result_time', "Finished"))
        self.setModel(self.model)
        if jobs is None:
            jobs = []
        self.model.setupModelData(jobs)
        self.setSelectionBehavior(QtWidgets.QAbstractItemView.SelectRows)

    def currentJob(self):
        idx = self.currentIndex()
        return self.model.jobs[idx.row()]

    def setJobs(self, jobs):
        self.model.setupModelData(jobs)


