import lispy
import logging
import platform


def Command(func):
    def wrapped(*args):
        return func(*args)
    lispy.global_env.update({func.__name__: wrapped})
    return wrapped


def Command_(*names):
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


@Command_('select-down')
def select_down():
    return model.currentTab().current.toggle_isselet_down()


@Command_('reload-commands')
def reload_commands():
    model.reload_commands()


@Command_('set-default-font')
def set_default_font(*args):
    view.set_defaultfont(*args)


@Command_('set-window-size')
def set_window_size(*args):
    view.set_window_size(*args)


@Command_('set-window-title')
def set_window_title(*args):
    view.set_window_title(*args)


@Command_('reload-config')
def reload_config(*args):
    view.load_config(*args)


@Command_('define-key')
def define_key(keymap, key, action):
    keymap[key] = action


@Command_('windows?')
def is_windows():
    return "Windows" == platform.system()

@Command_('osx?')
def is_osx():
    return "Darwin" == platform.system()

@Command_('platform-system')
def platform_system():
    return platform.system()
