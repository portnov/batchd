
from datetime import datetime
from PyQt4 import QtGui, QtCore

class Field(object):
    def __init__(self, name, title):
        self.name = name
        self.title = title

    def show(self, value):
        return value

class TimeField(Field):
    def show(self, value):
        if value:
            d = datetime.strptime(value, "%Y-%m-%dT%H:%M:%S.%fZ")
            return unicode(datetime.strftime(d, "%c"), "utf-8")
        else:
            return "<undefined>"

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

class Table(QtGui.QTableView):
    def __init__(self, jobs=None, parent=None):
        QtGui.QTableView.__init__(self, parent)
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

    def setJobs(self, jobs):
        self.model.setupModelData(jobs)


