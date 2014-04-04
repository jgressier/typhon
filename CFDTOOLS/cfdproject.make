####### directories + default targets + compilation options
#
.PHONY: default

default: this.library this.modules this.tools

PRJINCDIR = $(PRJDIR)/../include
CONFIGDIR = $(PRJDIR)/../config

PRJEXEDIR = $(PRJDIR)/Tools

