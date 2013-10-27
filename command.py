import lispy
import keymap
import logging
import subprocess
import tkinter
import os.path
import platform

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
    lispy.global_env.update({'view': view})
    lispy.global_env.update({'model': model})

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
def py_attr(attrname, self):
    return getattr(self, attrname)


@Command_
def sh_call(cwd, args):
    # TODO 判定関数を共通化すること
    isshell = 'Windows' == platform.system()
    if isshell:
        encoding = 'shift_jis'
    else:
        encoding = 'utf-8'
    # TODO windowsﾌときにstdoutの文字コードを変更すること
    with subprocess.Popen(args, cwd=cwd,
            stderr=subprocess.PIPE, stdout=subprocess.PIPE, shell=isshell) as proc:
        rcd = proc.wait()
        return proc.stdout.read().decode(encoding=encoding)

@Command_
def sh_popen(cwd, args):
    subprocess.Popen(args, cwd=cwd)

# TODO 以下　lisp置き換え

@Command_
def set_filter(func):
    model.currentTab().current.filter = func
    model.currentTab().current.reload()

@Command_
def clipboard():
    return tkinter.Text().clipboard_get()

@Command_
def set_clipboard(text_u):
    tkinter.Text().clipboard_clear()
    tkinter.Text().clipboard_append(text_u)

