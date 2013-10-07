import lispy
import logging
import platform
import keymap


def Command(func):
    def wrapped(*args):
        return func(*args)
    lispy.global_env.update({func.__name__: wrapped})
    return wrapped

def Command_(func):
    def wrapped(*args):
        return func(*args)
    lispy.global_env.update({func.__name__.replace('_', '-'): wrapped})
    return wrapped

def CommandName(*names):
    def command_decorator(func):
        def wrapped(*args):
            return func(*args)
        commands = {}
        for n in names:
            commands[n] = wrapped
        lispy.global_env.update(commands)
        return wrapped
    return command_decorator

try:
    model
except NameError:
    model = None

try:
    view
except NameError:
    view = None


def init(v, m):
    global model, view
    view, model = v, m

@Command
def tabclose():
    model.removeTab(model.currentIndex)

@Command
def tabnew():
    model.tabnew()


@Command
def tabnext(num=1):
    for n in range(num):
        model.nextTab()


@Command
def tabprev(num=1):
    for n in range(num):
        model.prevTab()


@Command
def tabcount():
    return len(model.tabs)


@Command
def tabchange(num):
    model.changeTab(num)


@Command
def tabfirst():
    model.changeTab(0)


@Command
def tablast():
    model.changeTab(len(model.tabs) - 1)


@Command
def pwd():
    return model.currentTab().current.cwd()


@Command_
def select_down():
    return model.currentTab().current.toggle_isselet_down()


@Command_
def reload_commands():
    model.reload_commands()


@Command_
def set_default_font(*args):
    view.set_defaultfont(*args)


@Command_
def set_window_size(*args):
    view.set_window_size(*args)

@Command_
def set_window_maximized(*args):
    view.set_window_maximized(*args)


@Command_
def set_window_title(*args):
    view.set_window_title(*args)


@Command_
def reload_config(*args):
    view.load_config(*args)


@Command_
def define_key(kmap, keystr, action):
    keymap.add_keymap(kmap, keystr, action)

@CommandName('windows?')
def is_windows():
    return "Windows" == platform.system()

@CommandName('osx?')
def is_osx():
    return "Darwin" == platform.system()

@Command_
def platform_system():
    return platform.system()

@Command_
def cursor_up():
    return model.currentTab().current.cursor_up()

@Command_
def cursor_down():
    return model.currentTab().current.cursor_down()

@Command_
def cd_or_exec():
    return model.currentTab().current.chdir_or_execute()

@Command_
def change_focus():
    return model.currentTab().change_focus()

@Command_
def cd_parent():
    return model.currentTab().current.chdir_parent()

@Command_
def reload_view():
    return model.currentTab().current.reload()

@Command_
def cursor_first():
    return model.currentTab().current.cursor_first()

@Command_
def cursor_last():
    return model.currentTab().current.cursor_last()

@Command_
def select_up():
    return model.currentTab().current.toggle_isselet_up()

@Command_
def select_all():
    return model.currentTab().current.toggle_isselect_all()

