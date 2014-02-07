############################################################
##   EXCHANGE library compilation

LDIR := EXCHANGE

####### Files

# Library
EXCHANGE_LIB = $(PRJLIBDIR)/libt_exch.a

# Modules
EXCHANGE_MOD = #.$(MOD)      \

# Objects
EXCHANGE_OBJ := $(EXCHANGE_MOD:.$(MODEXT)=.o)  \
               allreduce_sum_mpi.o         \
               exchange_zonal_residual_mpi.o \
               exchange_zonal_timestep_mpi.o \
               finalize_exch_mpi.o         \
               init_exch_protocol_mpi.o    \
               receivefromgrid_mpi.o       \
               sendtogrid_mpi.o            \

libt_exch.objects := $(EXCHANGE_OBJ:%=$(PRJOBJDIR)/%)
libt_exch.target: $(libt_exch.objects)

#libt_exch.target: F90CMP = $(MPIF90C)
#libt_exch.target: F90OPT += $(MPIF90_FC)

D_EXCHANGE_SRC := $(EXCHANGE_OBJ:%.o=$(LDIR)/%.f90)


####### Build rules

EXCHANGE_clean:
	-rm $(EXCHANGE_LIB) $(libt_exch.objects) $(EXCHANGE_MOD) EXCHANGE/depends.make


####### Dependencies

EXCHANGE/depends.make: $(D_EXCHANGE_SRC)
	$(MAKEDEPENDS) EXCHANGE

include EXCHANGE/depends.make


