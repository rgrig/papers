import sys
import urllib

if len(sys.argv) != 3:
  print('usage: wget.py url file')
  sys.exit(1)

urllib.urlretrieve(sys.argv[1], sys.argv[2])
