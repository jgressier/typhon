# Makefile to build web site

# Preprocesseur utilise
PRE = m4

# Le grep pour récupérer les bonnes infos
GREP = egrep -v "^$$"

# Les noms des fichiers a generer sans extension
SRC  = ./SRC
FIC  = index.html \
       feat.html  \

INC  = general.m4 header.m4 footer.m4 menu.m4
DIR  = ./HTML
DEST = $(FIC:%=$(DIR)/%)
DINC = $(INC:%=$(SRC)/%)

# Options obligatoires
OPT = -Dtimestamp="date +%d/%b/%Y" -I$(SRC)

# default target: both fr and eng sites
all: site

site: $(DIR) $(DEST)
	@echo "---------------------------"
	@echo "Build Web Site"
	@echo "---------------------------"
	@mkdir -p $(DIR)
	@cp -r FILES/* ./HTML

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
	@rcp ./HTML/* serv-sun1:HOME_PAGE


