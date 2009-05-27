
FC = ifort
FINC = -I../Include
FLIB = -L../Obj -L../Lib
FLIBSPEC = -lmpich -lt_math -lt_mesh -lt_modcom

SRCLIST = $(wildcard *.f90)
RAWLIST = $(patsubst %.f90,%,$(SRCLIST))
EXELIST = $(patsubst %,%.exe,$(RAWLIST))
RUNLIST = $(patsubst %,%.run,$(RAWLIST))

CHECK_ID = 0

CHECK = $(filter check,$(sort $(MAKECMDGOALS)))

GOALS = $(filter-out check,$(sort $(MAKECMDGOALS)))
ifeq ($(GOALS),)
  GOALS = $(RAWLIST)
endif

TMPGOALS = $(patsubst %.exe,%,$(patsubst %.run,%,$(patsubst %.f90,%,$(GOALS))))
RAWGOALS = $(filter $(RAWLIST),$(TMPGOALS))
RUNGOALS = $(patsubst %,%.run,$(RAWGOALS))
EXEGOALS = $(patsubst %,%.exe,$(RAWGOALS))

.PHONY: run exe check list $(RAWLIST) $(RUNLIST)

ifeq ($(CHECK),check)
  RAW_DEPS = run
else
  .DEFAULT_GOAL = exe
  RAW_DEPS = exe
endif

define PROGRAM_template
  $(1): $(1).$(RAW_DEPS)
	@:
  $(1).run: exe $(1).exe
	@sep() { seq -w -s '' 40 | sed "s/$$$$1/$$$$2/g;s/$$$$/$$$$3/" ;} ; \
	 echo ; \
	 sep . = ; echo $(1).exe ; \
	 sep . - ; ./$(1).exe && sep . = '  succes' || sep . @ '  ECHEC   @@@'
endef

$(foreach prog,$(RAWGOALS),$(eval $(call PROGRAM_template,$(prog))))

run: $(RUNGOALS) ;

exe: $(EXEGOALS) ;

list:
	@echo "targets :" $(EXELIST)
	@echo "         " check list run exe

check: exe run
	@:

%.exe: %.f90
	$(FC) $(FINC) $(FLIB) $< $(FLIBSPEC) -o $@

