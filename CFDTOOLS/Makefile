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

all:
	@(cd Base ;       make opt=$(opt))
	@(cd Models ;     make opt=$(opt))
	@(cd Mesh ;       make opt=$(opt))
	@(cd FileFormat ; make opt=$(opt))
