############################################################
##   Compilation de la librairie EXCHANGE

LDIR := EXCHANGE

####### Files

EXCHSEQ_LIB = $(PRJLIB)/libt_exchseq.a
EXCHMPI_LIB = $(PRJLIB)/libt_exchmpi.a

EXCHSEQ_MOD = #.$(MOD)      \
EXCHMPI_MOD = #.$(MOD)      \

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
	@echo \* Création de la librairie $(EXCHSEQ_LIB)
	@touch $(EXCHSEQ_LIB) ; rm $(EXCHSEQ_LIB)
	@$(AR) ruv $(EXCHSEQ_LIB) $(D_EXCHSEQ_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(EXCHSEQ_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(EXCHSEQ_LIB) créée
	@echo ---------------------------------------------------------------

$(EXCHMPI_LIB): $(D_EXCHMPI_OBJ)
	@echo ---------------------------------------------------------------
	@echo \* Création de la librairie $(EXCHMPI_LIB)
	@touch $(EXCHMPI_LIB) ; rm $(EXCHMPI_LIB)
	@$(AR) ruv $(EXCHMPI_LIB) $(D_EXCHMPI_OBJ)
	@echo \* Création de l\'index de la librairie
	@$(RAN)    $(EXCHMPI_LIB)
	@echo ---------------------------------------------------------------
	@echo \* LIBRAIRIE $(EXCHMPI_LIB) créée
	@echo ---------------------------------------------------------------

#EXCHANGE_clean:
#	-rm  $(EXCHANGE_LIB) $(D_EXCHANGE_OBJ) $(EXCHANGE_MOD) EXCHANGE/depends.make


####### Dependencies


EXCHANGE/depends.make: $(D_EXCHSEQ_SRC) $(D_EXCHMPI_SRC)
	(cd EXCHANGE ; ../$(MAKEDEPENDS))

include EXCHANGE/depends.make





