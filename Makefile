## This is epidemicSize2026
## An MMED project

current: target
-include target.mk
Ignore = target.mk

vim_session:
	bash -ic "vmt README.md TODO.md"

## -include makestuff/perl.def

######################################################################

Sources += $(wildcard *.md)

group: dagmaros27.invite Vhugala-Ramabaga.invite longachanda.invite nkgomelengl.invite judeyatuwa.invite

######################################################################

autopipeR = defined

Sources += $(wildcard scripts/*.R)
## scripts/SIRD_model.Rout: scripts/SIRD_model.R

scripts/dataPlot.Rout: scripts/dataPlot.R
	$(rThere)

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

## ln -s ../makestuff . ## Do this first if you want a linked makestuff
Makefile: makestuff/00.stamp
makestuff/%.stamp: | makestuff
	- $(RM) makestuff/*.stamp
	cd makestuff && $(MAKE) pull
	touch $@
makestuff:
	git clone --depth 1 $(msrepo)/makestuff

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/simpleR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
