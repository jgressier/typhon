# Ce fichier est généré automatiquement
# et est susceptible d'être écrasé

$(PRJINC)/GEO3D.$(MOD): \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \

$(PRJOBJ)/GEO3D.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \

$(PRJINC)/MESHBASE.$(MOD): \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \

$(PRJOBJ)/MESHBASE.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \

$(PRJINC)/STRMESH.$(MOD): \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \

$(PRJOBJ)/STRMESH.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \

$(PRJINC)/TENSOR3.$(MOD): \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \

$(PRJOBJ)/TENSOR3.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \

$(PRJINC)/USTMESH.$(MOD): \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \

$(PRJOBJ)/USTMESH.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \

$(PRJOBJ)/calc_connface.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/calc_meshcomp.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/STRMESH.$(MOD)    \

$(PRJOBJ)/calc_ust_cell.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/calc_ust_checkface.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \

$(PRJOBJ)/calc_ust_elemvol.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/calc_ust_face.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \
                         $(PRJINC)/MESHBASE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \

$(PRJOBJ)/calc_ust_midcell.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/calc_ustmesh.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/count_struct.o: \
                         $(PRJINC)/STRMESH.$(MOD)    \
                         $(PRJINC)/STRMESH.$(MOD)    \
                         $(PRJINC)/STRMESH.$(MOD)    \

$(PRJOBJ)/extract_points.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/extract_pts_index.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/GEO3D.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/reorder_ustconnect.o: \
                         $(PRJINC)/USTMESH.$(MOD)    \

$(PRJOBJ)/test_ustmesh.o: \
                         $(PRJINC)/TYPHMAKE.$(MOD)    \
                         $(PRJINC)/OUTPUT.$(MOD)    \
                         $(PRJINC)/USTMESH.$(MOD)    \

