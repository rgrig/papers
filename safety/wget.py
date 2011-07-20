import sys
if sys.version_info[0] == 3:
  from urllib.request import urlopen
else:
  from urllib import urlopen

if len(sys.argv) != 3:
  print('usage: wget.py url file')
  sys.exit(1)

n = urlopen(sys.argv[1])
l = open(sys.argv[2], 'wb')
l.write(n.read())
n.close()
l.close()
