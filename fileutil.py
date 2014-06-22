#! /usr/bin/env python
# coding=utf-8

import os
import os.path
import shutil
import stat
import datetime
import subprocess
import logging
import platform

def gitstatus(path, encoding='utf-8'):
    ps = subprocess.Popen(['git', 'status', '-s'],
            cwd=path,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
    ps.wait()
    return { gs[3:]: {'git_idx':gs[0], 'git_wk':gs[1]} for gs in map(lambda x:x.decode(encoding=encoding).rstrip('\n'), ps.stdout.readlines())}

class WinFileStatus(object):
    def stat(self, cwd, lis):
        import win32con
        dic = {}
        dic['filename'] = lis[8]
        dic['abspath'] = os.path.join(cwd, dic['filename'])
        dic['st_size'] = str((int(lis[4]) << 32) + int(lis[5]))
        dic['st_atime'] = datetime.datetime.strftime(lis[2], '%Y/%m/%d %H:%M:%S')
        dic['st_mtime'] = datetime.datetime.strftime(lis[3], '%Y/%m/%d %H:%M:%S')
        mode = ['-', 'r', 'w', '-', 'r', 'w', '-', 'r', 'w', '-', ]
        fileattr = lis[0]
        if fileattr & win32con.FILE_ATTRIBUTE_DIRECTORY:
            mode[0] = 'd'
            mode[3] = 'x'
            mode[6] = 'x'
            mode[9] = 'x'
        if fileattr & win32con.FILE_ATTRIBUTE_REPARSE_POINT:
            mode[0] = 'l'
        if fileattr & win32con.FILE_ATTRIBUTE_READONLY:
            mode[2] = '-'
            mode[5] = '-'
            mode[8] = '-'
        if dic['filename'].endswith(".exe") or dic['filename'].endswith(".cmd"):
            mode[3] = 'x'
            mode[6] = 'x'
            mode[9] = 'x'

        dic['filemode'] = '%s%s%s%s%s%s%s%s%s%s' % tuple(mode)
        return dic

    def __call__(self, cwd):
        import win32api
        return {f[8]: self.stat(cwd, f) for f in win32api.FindFiles(os.path.join(cwd, '*')) if f[8] != '.'}

def strftime(time, fmt='%Y/%m/%d %H:%M:%S'):
    return datetime.datetime.strftime(
            datetime.datetime.utcfromtimestamp(time),
            fmt)

def status(path):
    def st(path):
        dic = {}
        dic['abspath'] = path
        dic['filename'] = os.path.basename(path)
        try:
            lst = os.lstat(path)
            dic['filemode'] = stat.filemode(lst.st_mode)
            dic['st_mode'] = lst.st_mode
            dic['st_ino'] = lst.st_ino
            dic['st_dev'] = lst.st_dev
            dic['st_nlink'] = lst.st_nlink
            dic['st_uid'] = lst.st_uid
            dic['st_gid'] = lst.st_gid
            dic['st_size'] = str(lst.st_size)
            dic['st_atime'] = strftime(lst.st_atime)
            dic['st_mtime'] = strftime(lst.st_mtime)
            dic['st_ctime'] = strftime(lst.st_ctime)
        except Exception as e:
            logging.exception(e)
        return dic

    lis = os.listdir(path)
    lis.insert(0, '..')
    return [st(os.path.join(path, f)) for f in lis]

def copy(src, dst):
    # 上書きコピー禁止
    if src == dst and os.path.isfile(src) and os.path.isfile(dst):
        raise IOError()
    if os.path.isdir(dst) and os.path.exists(os.path.join(dst, os.path.basename(src))):
        raise IOError()

    if os.path.isfile(src):
        shutil.copy2(src, dst)
        logging.info('copy "' + src + '" -> "' + dst + '"')
    elif os.path.isdir(src):
        dst = os.path.join(dst, os.path.basename(src))
        shutil.copytree(src, dst)
        logging.info('copy "' + src + '" -> "' + dst + '"')
    else:
        raise IOError()

def remove(path):
    try:
        from send2trash import send2trash
        send2trash(path)
        logging.info('trash "' + path + '"')
    except ImportError as e:
        if os.path.isfile(path):
            os.remove(path)
        elif os.path.isdir(path):
            shutil.rmtree(path)
        else:
            raise IOError()
        logging.info('remove "' + path + '"')

def open_assoc(path):
    """関連付けで開く"""
    if 'Darwin' == platform.system():
        subprocess.Popen(['open', abspath])
    elif 'Windows' == platform.system():
        if os.path.isdir(path):
            subprocess.Popen(["explorer", path], shell=True )
        else:
            subprocess.Popen([path], shell=True )

def shortcut(srcpath, dstpath):
    import win32com.client
    if os.path.isdir(dstpath):
        dstpath = os.path.join(dstpath, os.path.basename(srcpath))
    root, ext = os.path.splitext(dstpath)
    if ext != '.lnk' and not root.endswith('.lnk'):
        dstpath += '.lnk'
    shell = win32com.client.Dispatch("WScript.Shell")
    shortcut = shell.CreateShortCut(dstpath)
    shortcut.Targetpath = srcpath
    shortcut.Save()

def targetpath(path):
    import win32com.client
    shell = win32com.client.Dispatch("WScript.Shell")
    shortcut = shell.CreateShortCut(path)
    if os.path.isdir(shortcut.Targetpath):
        return shortcut.Targetpath

def main():
    print(status('.'))

if __name__ == '__main__':
    main()

