import lispy
import keymap
import logging
import subprocess
import io


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
    lispy.global_env.update({'filer-view': view})
    lispy.global_env.update({'filer-model': model})

def do_command(cmd):
    logging.debug(cmd)
    try:
        val = lispy.eval(lispy.parse(cmd))
        if val is not None:
            logging.info(lispy.to_string(val))
    except Exception as e:
        logging.exception(e)
    return True

# python - lispy binding
@Command_
def py_method_call (methodname, self, *args):
    return getattr(self.__class__, methodname)(self, *args)

@Command_
def py_call_with_module(modulename, funcname, *args):
    return getattr(globals()[modulename], funcname)(*args)

@Command_
def py_call(funcname, *args):
    return globals()['__builtins__'][funcname](*args)

@Command_
def py_attr(attrname, self):
    return getattr(self, attrname)

@Command_
def py_import(modname):
    globals()[modname] = __import__(modname, globals(), locals(), [], 0)

@Command_
def py_class(modname, classname):
    return getattr(globals()[modname], classname)


@Command_
def sh_call(cwd, args):
    with io.StringIO() as out:
        ret = subprocess.call(args, stdout=out, cwd=cwd)
        return out.getvalue()

@Command_
def sh_popen(cwd, args):
    subprocess.Popen(args, cwd=cwd)

# TODO 以下　lisp置き換え

@Command_
def define_key(kmap, keystr, action):
    keymap.add_keymap(kmap, keystr, action)

@Command_
def set_filter(func):
    model.currentTab().current.filter = func
    model.currentTab().current.reload()

@Command_
def get_file_state(key, file):
    return file.state[key]

