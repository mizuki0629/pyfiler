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


class BaseFiler(object):
    def __init__(self, path=os.getcwd()):
        self.cwd_history = list()
        self._cwd = os.getcwd()
        self.chdir(path)

    def get_cwd(self):
        return self._cwd
    cwd = property(get_cwd)

    def _abspath(self, path):
        path = os.path.expandvars(os.path.expanduser(path))
        if os.path.isabs(path):
            return path
        else:
            return os.path.normpath(os.path.join(self.cwd, path))

    # TODO utcfromtimestampで渡して、UIで変換したほうが良い？
    def strftime(self, time):
        #return str(datetime.datetime.utcfromtimestamp(time))
        return datetime.datetime.strftime(datetime.datetime.utcfromtimestamp(time), '%Y/%m/%d %H:%M:%S')

    def stat(self, path):
        dic = {}
        dic['abspath'] = self._abspath(path)
        dic['filename'] = os.path.basename(path)
        try:

            st = os.lstat(self._abspath(path))
            dic['filemode'] = stat.filemode(st.st_mode)
            dic['st_mode'] = st.st_mode
            dic['st_ino'] = st.st_ino
            dic['st_dev'] = st.st_dev
            dic['st_nlink'] = st.st_nlink
            dic['st_uid'] = st.st_uid
            dic['st_gid'] = st.st_gid
            dic['st_size'] = str(st.st_size)
            dic['st_atime'] = self.strftime(st.st_atime)
            dic['st_mtime'] = self.strftime(st.st_mtime)
            dic['st_ctime'] = self.strftime(st.st_ctime)
        except PermissionError:
            pass
        return dic

    def ls(self):
        lis = sorted(os.listdir(self.cwd), key=str.lower)
        lis.insert(0, '..')
        return (self.stat(f) for f in lis)

    def open_assoc(self, path):
        abspath = self._abspath(path)
        if 'Windows' == platform.system():
            if os.path.isdir(abspath):
                subprocess.Popen(["explorer", abspath], shell=True )
            else:
                subprocess.Popen([abspath], shell=True )
        elif 'Darwin' == platform.system():
            subprocess.Popen(['open', abspath])

    def chdir(self, path):
        abspath = self._abspath(path)
        if os.path.isdir(abspath):
            self.cwd_history.append(self._cwd)
            self._cwd = abspath
            return self._cwd
        else:
            root, ext = os.path.splitext(path)
            if ext == '.lnk' and 'Windows' == platform.system():
                import win32com.client
                shell = win32com.client.Dispatch("WScript.Shell")
                shortcut = shell.CreateShortCut(abspath)
                if os.path.isdir(shortcut.Targetpath):
                    self.cwd_history.append(self._cwd)
                    self._cwd = shortcut.Targetpath
                    return self._cwd


    def popd(self):
        if len(self.cwd_history) > 0:
            self._cwd = self.cwd_history.pop()
            print(self._cwd)

    def rename(self, src, dst):
        abs_src = self._abspath(src)
        abs_dst = self._abspath(dst)
        os.rename(abs_src, abs_dst)

    def move(self, src, dst):
        abs_src = self._abspath(src)
        abs_dst = self._abspath(dst)
        if os.path.exists(os.path.join(abs_dst, os.path.basename(abs_src))):
            raise IOError()
        shutil.move(abs_src, abs_dst)

    def copy(self, src, dst):
        abs_src = self._abspath(src)
        abs_dst = self._abspath(dst)

        if os.path.isfile(abs_src):
            shutil.copy(abs_src, abs_dst)
        elif os.path.isdir(abs_src):
            shutil.copytree(abs_src, abs_dst)
        else:
            raise IOError()

    def remove(self, path):
        abs_path = self._abspath(path)
        if os.path.isfile(abs_path):
            os.remove(abs_path)
        elif os.path.isdir(abs_path):
            shutil.rmtree(abs_path)
        else:
            raise IOError()

    def create_symlink(self, srcpath, dstpath):
        srcpath = self._abspath(srcpath)
        dstpath = self._abspath(dstpath)
        if os.path.isdir(dstpath):
            dstpath = os.path.join(dstpath, os.path.basename(srcpath))
        os.symlink(srcpath, dstpath)

    def create_shortcut(self, srcpath, dstpath):
        import win32com.client
        srcpath = self._abspath(srcpath)
        dstpath = self._abspath(dstpath)
        if os.path.isdir(dstpath):
            dstpath = os.path.join(dstpath, os.path.basename(srcpath))
        root, ext = os.path.splitext(dstpath)
        if ext != '.lnk' and not root.endswith('.lnk'):
            dstpath += '.lnk'
        shell = win32com.client.Dispatch("WScript.Shell")
        shortcut = shell.CreateShortCut(dstpath)
        shortcut.Targetpath = srcpath
        shortcut.Save()

def main():
    filer = BaseFiler()
    print(filer.ls())

if __name__ == '__main__':
    main()
