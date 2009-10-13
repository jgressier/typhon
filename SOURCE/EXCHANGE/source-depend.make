############################################################
##   EXCHANGE library compilation

LDIR := EXCHANGE

####### Files

# Library
EXCHSEQ_LIB = $(PRJLIB)/libt_exchseq.a
EXCHMPI_LIB = $(PRJLIB)/libt_exchmpi.a

# Modules
EXCHSEQ_MOD = #.$(MOD)      \
EXCHMPI_MOD = #.$(MOD)      \

# Objects
EXCHSEQ_OBJ := $(EXCHANGE_MOD:.$(MOD)=.o)  \
               allreduce_sum_seq.o         \
               exchange_zonal_residual_seq.o \
               exchange_zonal_timestep_seq.o \
               finalize_exch_seq.o         \
               init_exch_protocol_seq.o    \
               receivefromgrid_seq.o       \
               sendtogrid_seq.o            \

EXCHMPI_OBJ := $(EXCHANGE_MOD:.$(MOD)=.o)  \
               allreduce_sum_mpi.o         \
               exchange_zonal_residual_mpi.o \
               exchange_zonal_timestep_mpi.o \
               finalize_exch_mpi.o         \
               init_exch_protocol_mpi.o    \
               receivefromgrid_mpi.o       \
               sendtogrid_mpi.o            \

D_EXCHSEQ_OBJ := $(EXCHSEQ_OBJ:%=$(PRJOBJ)/%)
D_EXCHMPI_OBJ := $(EXCHMPI_OBJ:%=$(PRJOBJ)/%)

D_EXCHSEQ_SRC := $(EXCHSEQ_OBJ:%.o=$(LDIR)/%.f90)
D_EXCHMPI_SRC := $(EXCHMPI_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

#all: $(EXCHANGE_LIB)

$(EXCHSEQ_LIB): $(D_EXCHSEQ_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(EXCHSEQ_LIB)
	@rm -f $(EXCHSEQ_LIB)
	@$(AR) ruv $(EXCHSEQ_LIB) $(D_EXCHSEQ_OBJ)
	@echo \* Creating library index
	@$(RAN)    $(EXCHSEQ_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(EXCHSEQ_LIB) created
	@echo ---------------------------------------------------------------

$(EXCHMPI_LIB): $(D_EXCHMPI_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Compiling library $(EXCHMPI_LIB)
	@rm -f $(EXCHMPI_LIB)
	@$(AR) ruv $(EXCHMPI_LIB) $(D_EXCHMPI_OBJ)
	@echo \* Creating index library
	@$(RAN)    $(EXCHMPI_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRARY $(EXCHMPI_LIB) created
	@echo ---------------------------------------------------------------

#EXCHANGE_clean:
#	-rm $(EXCHANGE_LIB) $(D_EXCHANGE_OBJ) $(EXCHANGE_MOD) EXCHANGE/depends.make


####### Dependencies

EXCHANGE/depends.make: $(D_EXCHSEQ_SRC) $(D_EXCHMPI_SRC)
	(cd EXCHANGE ; ../$(MAKEDEPENDS))

include EXCHANGE/depends.make


