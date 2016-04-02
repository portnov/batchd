
from PyQt4 import QtGui, QtCore


class Model(QtCore.QAbstractTableModel):
    def __init__(self, parent):
        QtCore.QAbstractTableModel.__init__(self)
        self.jobs = []

    def rowCount(self, parent):
        return len(self.jobs)
    
    def columnCount(self, parent):
        return 4
    
    def data(self, index, role):
        if role == QtCore.Qt.DisplayRole and index.isValid():
            job = self.jobs[index.row()]
            columns = ['id', 'seq', 'type', 'status']
            return job[columns[index.column()]]
        if role == QtCore.Qt.BackgroundColorRole:
            job = self.jobs[index.row()]
            if job['status'] == 'Failed':
                return QtGui.QColor(228, 122, 122)
            elif job['status'] == 'Done':
                return QtGui.QColor(132, 181, 97)
    
    def headerData(self, section, orientation, role):
        if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
            return ["ID", "Seq", "Type", "Status"][section]
        if orientation == QtCore.Qt.Vertical and role == QtCore.Qt.DisplayRole:
            return section+1
    
    def setupModelData(self, jobs):
        self.beginResetModel()
        self.jobs = jobs
        self.endResetModel()

class Table(QtGui.QTableView):
    def __init__(self, jobs=None, parent=None):
        QtGui.QTableView.__init__(self, parent)
        self.model = Model(self)
        self.setModel(self.model)
        if jobs is None:
            jobs = []
        self.model.setupModelData(jobs)

    def setJobs(self, jobs):
        self.model.setupModelData(jobs)


