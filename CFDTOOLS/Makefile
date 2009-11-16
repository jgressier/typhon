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
	@(cd Base ;       gmake opt=$(opt))
	@(cd Models ;     gmake opt=$(opt))
	@(cd Mesh ;       gmake opt=$(opt))
	@(cd FileFormat ; gmake opt=$(opt))
