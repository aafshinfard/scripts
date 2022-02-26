#!/usr/bin/env bash

import os
import sys
from os.path import join
import shutil
import copy
import subprocess
import argparse
import math
import uuid, base64

ARGS_FILE = 'args'

file_args = {}
if os.path.exists(ARGS_FILE):
  with open(ARGS_FILE, 'r') as f:
    for line in f:
      if line.strip() == '': continue
      tokens = line.strip().split('=')
      if tokens[0][0] == '#': continue
      if len(tokens[1]) >= 2 and tokens[1][0] == '\"' and tokens[1][-1] == '\"': tokens[1] = tokens[1][1:-1]
      if tokens[1] == '': tokens[1] = None
      file_args[tokens[0]] = tokens[1]
else:
  print("No args file found!")
  sys.exit(-1)

args = file_args
assert 'threads' in args
assert 'kc_end' in args
assert 'kc_step' in args
assert 'k_start' in args
assert 'k_end' in args
assert 'k_step' in args
assert 'B' in args



conda_env rust-env

memusg -o ${reads}.rust-mdbg.multik.memusg -t utils/multik ${reads}.fq.gz -t$t
