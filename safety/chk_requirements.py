import os
import sys

tools = [
    (['javac', '-version'], {'Ubuntu' : 'openjdk-6-jdk', 'Fedora': 'java-1.6.0-openjdk', 'MacPorts' : 'java-1.6.0-openjdk'}),
    (['pygmentize', '-V'], {'Ubuntu' : 'python-pygments', 'Fedora' : 'python-pygments', 'MacPorts' : 'py32-pygments', 'other' : 'easy_install Pygments'}),
    (['pdflatex', '-version'], {'Ubuntu' : 'texlive-full', 'Fedora' : 'texlive-latex', 'MacPorts' : 'texlive +full'})]
# I assume bibtex is in the same packages as pdflatex

install = {
    'Ubuntu' : 'apt-get install',
    'Fedora' : 'yum install',
    'MacPorts' : 'port install',
    'other' : ''}

for _, ps in tools:
  for p, _ in ps.items():
    if p not in install:
      print('INTERNAL: %s not in install' % p)
      sys.exit(1)

for cmd, pkgs in tools:
  if os.spawnlp(os.P_WAIT, cmd[0], *cmd) != 0:
    print('You need %s.' % cmd[0])
    if os.name == 'nt':
      print("Sorry, I can't tell you how to install it")
    else:
      for platform, package in pkgs.items():
        print('  %s: sudo %s %s' % (platform, install[platform], package))
