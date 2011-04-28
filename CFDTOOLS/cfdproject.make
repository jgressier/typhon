#
default: this.tools this.library this.modules

PRJINCDIR=$(PRJDIR)/../include
PRJINCOPT=-I$(PRJINCDIR)
PRJLIBDIR=$(PRJDIR)/lib
PRJEXEDIR=$(PRJDIR)/Tools
CONFIGDIR=$(PRJDIR)/../config

