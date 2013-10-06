import lispy
from PyQt4.QtCore import Qt  # TODO qtに依存しない形にすること
normal_map = {}
lispy.global_env.update({'normal-map':normal_map})
command_map = {}
lispy.global_env.update({'command-map':command_map })

qt_keymap = {
        Qt.Key_H: 'h',
        Qt.Key_T: 't',
        }
