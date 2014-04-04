TARGETS = seq omp mpi

.PHONY: help all clean PRGEXT_REQUIRED

TGDIRS = \
        Base       \
        Math       \
        Models     \
        Mesh       \
        FileFormat \

clean.TGDIRS = $(addprefix clean.,$(TGDIRS))

clean.TARGETS = $(addprefix clean.,$(TARGETS))

.PHONY: $(TGDIRS) clean.lib $(clean.TGDIRS) $(clean.TARGETS)

help:
	@echo "-----------------------------------------------------------------"
	@echo "CFDtools project"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> CFDtools libraries and modules"
	@echo "Options:"
	@echo "  opt=optim  -> Optimized options (default)"
	@echo "  opt=debug  -> Debugging options"
	@echo "  opt=profil -> Profiling options"
	@echo "-----------------------------------------------------------------"

PRGEXT_REQUIRED: help
	@echo "================================================================="
	@echo "Extension option is required"
	@echo "  PRGEXT=prgext"
	@echo "with prgext in ( $(TARGETS) )"
	@echo "================================================================="
	@exit 1

clean.GOALS := $(filter $(clean.TARGETS),$(MAKECMDGOALS))
other.GOALS := $(filter-out $(clean.GOALS),$(MAKECMDGOALS))
clean.PRGEXT := $(patsubst clean.%,%,$(clean.GOALS))

ifdef clean.GOALS
#
ifdef other.GOALS
$(info clean GOALS = $(clean.GOALS))
$(info other GOALS = $(other.GOALS))
$(error This is incompatible...)
endif # def other.GOALS
#
ifdef PRGEXT
ifneq ($(PRGEXT),$(clean.PRGEXT))
$(info PRGEXT = $(PRGEXT))
$(info GOALS  = $(clean.GOALS))
$(error This is incompatible...)
endif
else  # ndef PRGEXT
PRGEXT := $(clean.PRGEXT)
endif # def/ndef PRGEXT
#
endif # def clean.GOALS

$(clean.TARGETS): clean

ifeq ($(filter $(TARGETS),$(PRGEXT)),)
$(filter-out help,$(MAKECMDGOALS)) all: PRGEXT_REQUIRED
endif

all: $(TGDIRS)

$(TGDIRS):
	@cd $@ ; $(MAKE) opt=$(opt) PRGEXT=$(PRGEXT)

clean: $(clean.TGDIRS)

$(clean.TGDIRS): clean.%:
	@cd $* ; $(MAKE) clean PRGEXT=$(PRGEXT)

