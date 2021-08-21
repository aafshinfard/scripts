#!/usr/bin/env python3

import os
import sys
import argparse
import shutil
import subprocess
from os.path import join

ARGS_FILE = 'args'
args = {
  'name' : 'assembly'
}

for file in os.listdir('.'):
  if (file != ARGS_FILE and
     not file.startswith('abyss-run') and
     not file.startswith('quast-analysis') and
      not file.startswith('quast-detailed') and
     not file.startswith('busco-analysis') and
     not file.endswith('.time') and
     not file.endswith('.tsv') and
     not file.startswith('{}-stats'.format(args['name'])) and
     not file.endswith('-1.fa') and
     not file.endswith('-1.dot') and
     not file.endswith('-8.fa') and
     not file.endswith('-scaffolds.fa')):
    if os.path.isdir(file):
      shutil.rmtree(file, ignore_errors=True)
    else:
      os.remove(file)
