#!/usr/bin/python3

import sys
import os, signal
from multiprocessing import Process
import subprocess

PROC = None
PROC_CMD = ''
PROC_ARGS = []
PROC_ARGV = []

def handler(signum, frame):
	global PROC

	if signum == signal.SIGINT:
		print("Bye")
		sys.exit(0)

	print('Reloading', signum)
	PROC.terminate()
	restart()


def run(argv):
	# print(argv)
	subprocess.call(argv)


def restart():
	global PROC
	global PROC_ARGV
	PROC = Process(target=run, args=(PROC_ARGV,))
	PROC.start()

if __name__ == "__main__":
	PROC_CMD = sys.argv[1]
	PROC_ARGS =  sys.argv[2:]
	PROC_ARGV = sys.argv[1:]

	# print(PROC_CMD)
	# print(PROC_ARGS)

	signal.signal(signal.SIGINT, handler)
	signal.signal(signal.SIGTERM, handler)
	signal.signal(signal.SIGTSTP, handler)
	restart()
	while True:
		pass

