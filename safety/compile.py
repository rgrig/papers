import os
import re
import sys

def bibtex(f):
  return os.spawnlp(os.P_WAIT, 'bibtex', 'bibtex', f)

def latex(f):
  return os.spawnlp(os.P_WAIT, 'pdflatex', 'pdflatex', f)

def grep(fn, r):
  f = open(fn, 'r')
  r = re.compile(r)
  for l in f:
    if r.search(l):
      f.close()
      return True
  f.close()
  return False

def compile(f):
  log = '%s.log' % f
  if latex(f) != 0:
    return
  if grep(log, 'undefined citations'):
    if bibtex(f) != 0:
      return
    latex(f)
  count = 0
  while grep(log, '[Rr]erun'):
    latex(f)
    count += 1
    if count > 5:
      print('I run pdflatex many times for %s. Something is wrong.' % f)
      print('Please take a look at %s and see why it says "rerun".' % log)
      return

for f in sys.argv[1:]:
  if f[-4:] == '.tex':
    f = f[:-4]
  compile(f)
