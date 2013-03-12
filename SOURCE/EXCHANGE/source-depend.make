############################################################
##   EXCHANGE library compilation

LDIR := EXCHANGE

####### Files

# Library
EXCHSEQ_LIB = $(PRJLIBDIR)/libt_exchseq.a
EXCHMPI_LIB = $(PRJLIBDIR)/libt_exchmpi.a

# Modules
EXCHSEQ_MOD = #.$(MOD)      \
EXCHMPI_MOD = #.$(MOD)      \

# Objects
EXCHSEQ_OBJ := $(EXCHANGE_MOD:.$(MODEXT)=.o)  \
               allreduce_sum_seq.o         \
               exchange_zonal_residual_seq.o \
               exchange_zonal_timestep_seq.o \
               finalize_exch_seq.o         \
               init_exch_protocol_seq.o    \
               receivefromgrid_seq.o       \
               sendtogrid_seq.o            \

EXCHMPI_OBJ := $(EXCHANGE_MOD:.$(MODEXT)=.o)  \
               allreduce_sum_mpi.o         \
               exchange_zonal_residual_mpi.o \
               exchange_zonal_timestep_mpi.o \
               finalize_exch_mpi.o         \
               init_exch_protocol_mpi.o    \
               receivefromgrid_mpi.o       \
               sendtogrid_mpi.o            \

libt_exchseq.objects := $(EXCHSEQ_OBJ:%=$(PRJOBJDIR)/%)
libt_exchmpi.objects := $(EXCHMPI_OBJ:%=$(PRJOBJDIR)/%)
libt_exchseq.target: $(libt_exchseq.objects)
libt_exchmpi.target: $(libt_exchmpi.objects)

libt_exchmpi.target: F90CMP = $(MPIF90C)
libt_exchmpi.target: F90OPT += $(MPIF90_FC)

D_EXCHSEQ_SRC := $(EXCHSEQ_OBJ:%.o=$(LDIR)/%.f90)
D_EXCHMPI_SRC := $(EXCHMPI_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

EXCHANGE_clean:
	-rm $(EXCHANGE_LIB) $(libt_exchseq.objects) $(libt_exchmpi.objects) $(EXCHANGE_MOD) EXCHANGE/depends.make


####### Dependencies

EXCHANGE/depends.make: $(D_EXCHSEQ_SRC) $(D_EXCHMPI_SRC)
	$(MAKEDEPENDS) EXCHANGE

include EXCHANGE/depends.make


