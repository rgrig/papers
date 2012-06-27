import os
import re
import sys

def bibtex(f):
  return os.spawnlp(os.P_WAIT, 'bibtex', 'bibtex', f)

def latex(f):
  return os.spawnlp(os.P_WAIT, 'pdflatex', 'pdflatex', f)

def grep(fn, pat, pp = False):
  r = 0
  f = open(fn, 'r')
  pat = re.compile(pat)
  for l in f:
    if pat.search(l):
      r += 1
      if pp:
        print l,
  f.close()
  return r

def compile(f):
  log = '%s.log' % f
  if latex(f) != 0:
    return
  wp, wn = None, grep(log, 'Warning')
  if grep(log, 'Citation .* undefined'):
    if bibtex(f) != 0:
      return
    latex(f)
    wp, wn = wn, grep(log, 'Warning')
  while wp == None or wp > wn:
    latex(f)
    wp, wn = wn, grep(log, 'Warning')
  if wn > 0:
    print '\n\n***\nRemaining warnings:'
    grep(log, 'Warning', True)

for f in sys.argv[1:]:
  if f[-4:] == '.tex':
    f = f[:-4]
  compile(f)
