# http://blog.ufsoft.org/2009/1/29/ipython-and-virtualenv
# using "python -c ‘import IPython; IPython.Shell.IPShell().mainloop()" instead
# in either case, tab-completion is broken for import hitting virtualenv

import site
import sys
from os import environ
from os.path import join
from sys import version_info

if 'VIRTUAL_ENV' in environ:
    virtual_env = join(environ.get('VIRTUAL_ENV'),
                       'lib',
                       'python%d.%d' % version_info[:2],
                       'site-packages')
    sys.path.insert(0, virtual_env)
    site.addsitedir(virtual_env)
    print virtual_env
    del virtual_env
del site, sys, environ, join, version_info
