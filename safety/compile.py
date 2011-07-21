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
  for i in range(10):
    if latex(f) != 0:
      return
    if grep(log, 'No file.*bbl'):
      if bibtex(f) != 0:
        return
    if not grep(log, '[Rr]erun'):
      return
  print('I run pdflatex 10 times for %s. Something is wrong.' % f)
  print('Please take a look at %s and see why it says "rerun".' % log)

for f in sys.argv[1:]:
  if f[-4:] == '.tex':
    f = f[:-4]
  compile(f)
