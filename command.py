import shlex
import platform
import subprocess
from logging import getLogger,StreamHandler,DEBUG, Formatter


logger = getLogger(__name__)

def add_globals(self):
    self.update({
        'tes': lambda x: x,
        })
    return self

global_env = add_globals({})

def command(func):
    def wrapped(*args):
        return func(*args)
    global_env.update({func.__name__: wrapped})
    return wrapped

@command
def hoge(x):
    return "thoge"

def dispatch(cmdstr, env=global_env):
    """文字列からコマンドにdispatchする
    結果はloggingnに出力する
    """
    args = shlex.split(cmdstr)
    if len(args) < 1:
        return

    # "!"から始まる場合はshellを呼び出す
    if args[0].startswith('!'):
        args[0] = args[0].lstrip('!')
        ret = call_shell('.', args)
        if ret[0] != 0:
            logger.error(ret[2])
        else:
            logger.info(ret[1])
        return

    else:
        if not args[0] in env:
            logger.error("'" + args[0] + "' was not difined")
            return

        func = env[args[0]]
        if len(args) < 2:
            ret = func()
        else:
            ret = func(*args[1:])
        logger.info(ret)
        return

def call_shell(cwd, args):
    """shellを呼び出す"""
    isshell = 'Windows' == platform.system()
    if isshell:
        encoding = 'shift_jis'
    else:
        encoding = 'utf-8'
    with subprocess.Popen(args, cwd=cwd,
            stderr=subprocess.PIPE, stdout=subprocess.PIPE, shell=isshell) as proc:
        return [proc.wait(), proc.stdout.read().decode(encoding=encoding), proc.stderr.read().decode(encoding=encoding)]


if __name__ == '__main__':
    handler = StreamHandler()
    handler.setLevel(DEBUG)
    handler.setFormatter(Formatter('%(levelname)-8s %(filename)s#%(funcName)s(%(lineno)d) - %(message)s'))

    logger.setLevel(DEBUG)
    logger.addHandler(handler)
    dispatch("hoge 2")
