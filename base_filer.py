#! /usr/bin/env python
# vim: coding=utf-8

import os
import os.path
import shutil

class BaseFiler(object):
	def __init__(self, path=os.getcwd()):
		self.cwd_history = list()
		self._cwd = os.getcwd()
		self.chdir(path)

	def get_cwd(self):
		return self._cwd
	cwd = property(get_cwd)

	def _abspath(self, path):
		if os.path.isabs(path):
			return path
		else:
			return os.path.normpath(os.path.join(self.cwd, path))

	def stat(self, path):
		return os.stat(self._abspath(path))

	def ls(self):
		return [(f, os.stat(f)) for f in os.listdir(self.cwd)]

	def chdir(self, path):
		abspath = self._abspath(path)
		if os.path.isdir(abspath):
			self._cwd = abspath
			return self._cwd
		else:
			raise IOError()

	def pushd(self, path):
		self.cwd_history.append(self.cwd)
		return self.chdir(path)

	def popd(self):
		return self.chdir(self.cwd_history.pop())

	def rename(self, src, dst):
		abs_src = self._abspath(src)
		abs_dst = self._abspath(dst)
		os.rename(abs_src, abs_dst)

	def move(self, src, dst):
		abs_src = self._abspath(src)
		abs_dst = self._abspath(dst)
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

def main():
	filer = BaseFiler()
	filer.remove('filer')
	print filer.cwd

if __name__ == '__main__':
	main()
