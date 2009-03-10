# Makefile to build web site

# Preprocesseur utilise
PRE = m4

# Le grep pour récupérer les bonnes infos
GREP = egrep -v "^ *$$"

# Les noms des fichiers a generer sans extension
SRC  = ./SRC
FIC  = index.html         \
       index_news.html    \
       index_pres.html    \
       feat.html          \
       feat_core.html     \
       feat_ns.html       \
       feat_kdif.html     \
       feat_input.html    \
       feat_output.html   \
       case.html          \
       case_shock.html    \
       case_shock_bicyl.html    \
       case_viscous.html  \
       case_amr.html      \
       case_heat.html     \
       doc.html           \
       doc_input.html     \
       doc_output.html    \
       doc_mesh.html      \
       doc_ns.html        \
       doc_kdif.html      \
       doc_scheme.html    \
       doc_coupling.html  \
       doc_amr.html       \
       doc_mpi.html       \
       dev.html           \
       dev_team.html      \
       dev_chlog.html     \
       dev_rmap.html      \
       dev_sf.html        \
       devdoc.html        \
       devdoc_core.html   \
       devdoc_zone.html   \
       devdoc_grid.html   \
       devdoc_ustmesh.html\
       get.html           \
       get_download.html  \
       get_install.html   \
       get_faq.html       \

include SRC/source-case.make

INC  = general.m4 header.m4 footer.m4 menu.m4
DIR  = HTML
DEST = $(FIC:%=$(DIR)/%) $(CASEFIC:%=$(DIR)/%)
DINC = $(INC:%=$(SRC)/%)

# Options obligatoires
OPT = -Dtimestamp="date +%d/%m/%Y" -I$(SRC)

# default target
all: site

site: $(DIR) $(DEST)
	@echo "---------------------------"
	@echo "Build Web Site"
	@echo "---------------------------"
	@mkdir -p $(DIR)
	cp -r FILES/* $(DIR)
	-find $(DIR) -type d -name .svn | xargs rm -rf

img:

$(DIR):
	@mkdir -p $(DIR)

$(DIR)/%.html:	$(SRC)/%.html.m4 $(DINC)
	@echo
	@echo "Parsing m4 file to" $*.html
	$(PRE) $(OPT) $(OPT2) $< | $(GREP) > $(DIR)/$*.html

clean:
	rm -rf ./HTML

install: site
	@echo "---------------------------"
	@echo "Install Web Site"
	@echo "---------------------------"
	chmod -R ugo+rX HTML/*
	rsync --rsh=ssh -avz ./HTML/* gressier,typhon@web.sourceforge.net:htdocs


