TARGETS = seq omp mpi

.PHONY: help all clean PRGEXT_REQUIRED

TGDIRS = \
        Base       \
        Math       \
        Models     \
        Mesh       \
        FileFormat \

clean.TGDIRS = $(addprefix clean,$(TGDIRS))

.PHONY: $(TGDIRS) cleanlib $(clean.TGDIRS)

help:
	@echo "-----------------------------------------------------------------"
	@echo "CFDtools project"
	@echo "-----------------------------------------------------------------"
	@echo "Targets:"
	@echo "  all       -> CFDtools libraries and modules"
	@echo "Options:"
	@echo "  opt=optim -> Optimized options (default)"
	@echo "  opt=debug -> Debugging options"
	@echo "  opt=prof  -> Profiling options"
	@echo "-----------------------------------------------------------------"

PRGEXT_REQUIRED: help
	@echo "================================================================="
	@echo "Extension option is required"
	@echo "  PRGEXT=prgext"
	@echo "with prgext in ( $(TARGETS) )"
	@echo "================================================================="
	@exit 1

ifeq ($(filter $(TARGETS),$(PRGEXT)),)
all: PRGEXT_REQUIRED
endif

all: $(TGDIRS)

$(TGDIRS):
	@cd $@ ; $(MAKE) opt=$(opt) PRGEXT=$(PRGEXT)

clean: cleanlib $(clean.TGDIRS)

cleanlib:
	@rm Lib.$(PRGEXT).$(opt.$(opt))/*.a

$(clean.TGDIRS): clean%:
	@cd $* ; $(MAKE) clean PRGEXT=$(PRGEXT)

