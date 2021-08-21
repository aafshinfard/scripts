#!/usr/bin/env python3

import os
import sys
import argparse
import shutil
import subprocess
from os.path import join

def parse_arguments():
        "Parse the command line arguments."
        argparser = argparse.ArgumentParser()
        argparser.add_argument(
            "-t", "--threads", action="store", dest="threads", type=int,
            default=min(16, os.cpu_count()),
            help="number of threads [16 or number of CPU]")
        argparser.add_argument(
            "--name", action="store", dest="name", type=str, default="assembly",
            help="prefix name of assemblies to keep.")
        argparser.add_argument(
            "--mode", action="store", dest="mode", type=str, default="normal",
            choices=["deep", "normal", "light"],
            help="Mode of cleaning the files/dirs."
                 "Accepted values are: deep (only \"name\"-8.fa), normal (+ quast results), or light (+ \"name\"-1.fa) [normal].")
        return argparser.parse_args()
      
      
      

ARGS_FILE = 'args'
args = {
  'name' : 'assembly'
  'deep': 'no'
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
