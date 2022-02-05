import sys
import PyQt5.QtGui as QtGui
import PyQt5.QtCore as QtCore

class Model(QtCore.QAbstractTableModel):
	def __init__(self, parent):
		QtCore.QAbstractTableModel.__init__(self)
		self.kolonki = []
		self.rowItem =[]
	def rowCount(self, parent):
		return len(self.rowItem)
	
	def columnCount(self, parent):
		return len(self.kolonki)
	
	def data(self, index, role):
		if role == QtCore.Qt.DisplayRole and index.isValid():
			return self.rowItem[index.row()][index.column()]
		if role == QtCore.Qt.BackgroundColorRole:
			if "e" in self.rowItem[index.row()]:
				return QtGui.QColor(255, 0, 0)
	
	def headerData(self, section, orientation, role):
		if orientation == QtCore.Qt.Horizontal and role == QtCore.Qt.DisplayRole:
			return self.kolonki[section]
		if orientation == QtCore.Qt.Vertical and role == QtCore.Qt.DisplayRole:
			return section+1
	
	def setupModelData(self, kolonki, data):
		self.beginResetModel()
		self.kolonki = kolonki
		map(self.rowItem.append, data)
		self.endResetModel()
class MainWindow(QtGui.QMainWindow):
	def __init__(self, parent):
		QtGui.QMainWindow.__init__(self, parent)
		self.table = QtGui.QTableView(self)
		self.table.setAlternatingRowColors(True)
		model = Model(self.table)
		data = [
			['a','b','c'],
			['d','e','y']
		]
		model.setupModelData(['1', '2', '3'], data)
		self.table.setModel(model)
		self.setCentralWidget(self.table)
if __name__ == "__main__":
	app = QtGui.QApplication(sys.argv)
	Window = MainWindow(None)
	Window.show()
	sys.exit(app.exec_())
