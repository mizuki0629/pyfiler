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

def marge_dict(*args):
    dst = {}
    for src in args:
        dst.update(src)
    return dst


class DecorateStatus(object):
    def __init__(self, org=None):
        self.org = org

    def status(self, cwd):
        pass

    def __call__(self, cwd):
        ls = self.status(cwd)
        return {k: marge_dict(v, ls.get(k, {})) for k, v in self.org(cwd).items()}


class GitStatus(DecorateStatus):
    def status(self, cwd):
        ps = subprocess.Popen(['git', 'status', '-s'],
                cwd=cwd,
                stdout=subprocess.PIPE)
        ps.wait()
        return { gs[3:]: {'git_wk':gs[0], 'git_idx':gs[1]} for gs in map(lambda x:x.decode(encoding='utf-8').rstrip('\n'), ps.stdout.readlines())}

class WinFileStatus(object):
    def stat(self, cwd, lis):
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
        import win32con
        return {f[8]: self.stat(cwd, f) for f in win32api.FindFiles(os.path.join(cwd, '*')) if f[8] != '.'}

class FileStatus(object):
    def stat(self, abspath):
        dic = {}
        dic['abspath'] = abspath
        dic['filename'] = os.path.basename(abspath)
        try:
            st = os.lstat(abspath)
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
        except Exception as e:
            logging.exception(e)
        return dic

    def strftime(self, time):
        return datetime.datetime.strftime(
                datetime.datetime.utcfromtimestamp(time),
                '%Y/%m/%d %H:%M:%S')

    def __call__(self, cwd):
        lis = sorted(os.listdir(cwd), key=str.lower)
        lis.insert(0, '..')
        return {f: self.stat(os.path.join(cwd, f)) for f in lis}

class BaseFiler(object):
    def __init__(self, path=os.getcwd()):
        self.cwd_history = list()
        self._cwd = os.getcwd()
        self.chdir(path)
        self.status = GitStatus(FileStatus())

    def get_cwd(self):
        return self._cwd
    cwd = property(get_cwd)

    def _abspath(self, path):
        if os.path.isabs(path) and os.path.exists(path):
            return path

        tmp = os.path.normpath(os.path.join(self.cwd, path))
        if os.path.exists(tmp):
            return  tmp

        tmp = os.path.normpath(os.path.expandvars(tmp))
        if os.path.exists(tmp):
            return  tmp

        path = os.path.normpath(os.path.expandvars(os.path.expanduser(path)))
        return path

    def ls(self):
        return self.status(self.cwd).values()

    def open_assoc(self, path):
        abspath = self._abspath(path)
        if 'Darwin' == platform.system():
            subprocess.Popen(['open', abspath])

    def chdir(self, path):
        abspath = self._abspath(path)
        if os.path.isdir(abspath):
            self.cwd_history.append(self._cwd)
            self._cwd = abspath
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
        logging.info('move "' + abs_src + '" -> "' + abs_dst + '"')

    def copy(self, src, dst):
        src = self._abspath(src)
        dst = self._abspath(dst)

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

    def remove(self, path):
        path = self._abspath(path)
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

    def create_symlink(self, srcpath, dstpath):
        srcpath = self._abspath(srcpath)
        dstpath = self._abspath(dstpath)
        if os.path.isdir(dstpath):
            dstpath = os.path.join(dstpath, os.path.basename(srcpath))
        os.symlink(srcpath, dstpath)

class WindowsFiler(BaseFiler):
    def __init__(self, path=os.getcwd()):
        BaseFiler.__init__(self, path)
        self.status = GitStatus(WinFileStatus())

    def open_assoc(self, path):
        abspath = self._abspath(path)
        if os.path.isdir(abspath):
            subprocess.Popen(["explorer", abspath], shell=True )
        else:
            subprocess.Popen([abspath], shell=True )

    def chdir(self, path):
        abspath = self._abspath(path)
        if os.path.isdir(abspath):
            self.cwd_history.append(self._cwd)
            self._cwd = abspath
            return self._cwd
        else:
            _, ext = os.path.splitext(path)
            if ext == '.lnk':
                import win32com.client
                shell = win32com.client.Dispatch("WScript.Shell")
                shortcut = shell.CreateShortCut(abspath)
                if os.path.isdir(shortcut.Targetpath):
                    self.cwd_history.append(self._cwd)
                    self._cwd = shortcut.Targetpath
                    return self._cwd

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
    pass


if __name__ == '__main__':
    main()
