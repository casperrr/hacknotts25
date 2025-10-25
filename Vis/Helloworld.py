import moderngl

from PyQt5.QtCore import Qt, QTimer
from PyQt5.QtGui import QSurfaceFormat
from PyQt5.QtWidgets import QApplication, QOpenGLWidget, QShortcut

class Window(QOpenGLWidget):

    frame_rate = 61

    def __init__(self):
	    super().__init__()

	    fmt = QSurfaceFormat()
	    fmt.setVersion(3, 3)
	    fmt.setProfile(QSurfaceFormat.CoreProfile)
	    fmt.setDefaultFormat(fmt)
	    fmt.setSamples(4)
	    self.setFormat(fmt)

	    self.t = None

	    QShortcut(Qt.Key_Escape, self, self.quit)

    def quit(self):
        self.exit()
        self.close()
    
    @classmethod
    def run(cls):
        app = QApplication([])
        main = cls()
        main.show()
        app.exit(app.exec())

if __name__ == '__main__':
    Window.run()