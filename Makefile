SHELL := /bin/bash

MAJOR := 0
MINOR := 0

PKGS := PatchBasedAMR

# Preprocessing definitions
NSPECIES := 3
HYDRO := exact # if you wish to run without hydro, comment out this line

EXEC := Rhyme-$(strip $(MAJOR)).$(strip $(MINOR))

OBJDIR := ./obj
SRCDIR := ./src
LIBDIR := ./lib
BINDIR := ./bin
INCLUDEDIR := ./include

FC := h5fc
CC := h5cc
RM := @rm -f -v

OPENMP := -fopenmp

DEFINES := -DNSPECIES=$(NSPECIES)

ifdef HYDRO
	DEFINES += -DHYDRO=$(HYDRO)
endif

FFLAGS := -Wall -cpp -dM -ffree-line-length-none $(DEFINES)

INCLUDES := -I $(INCLUDEDIR)
INCLUDES += $(foreach lib, $(PKGS), -I $(LIBDIR)/$(lib)/$(INCLUDEDIR))

LIBS :=  -lz -lm -L $(LIBDIR) -lamr
LIBS += $(foreach lib, $(PKGS), -L $(LIBDIR)/$(lib)/$(LIBDIR) -l$(lib))

GLOBAL_OBJS := $(OBJDIR)/date_time.o

OBJS = $(GLOBAL_OBJS) $(OBJDIR)/main.o

VPATH := $(SRCDIR)

.PHONY: default
default: | $(PKGS) $(BINDIR)/$(EXEC)


$(BINDIR)/$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) $(OPENMP) $(LIBS) $(INCLUDES) $(OBJS) -o $@


.PHONY: $(PKGS)
$(PKGS):
	@for lib in $(PKGS); do \
		make libstatic -C $(LIBDIR)/$$lib; \
	done;


$(OBJDIR)/%.o: %.f90
	$(FC) -c $(FFLAGS) $(LIBS) $(OPENMP) $(INCLUDES) -o $@ $< -J $(INCLUDEDIR)


.PHONY: watch
watch:
	$(eval FILES := `find . -regextype sed -regex ".*[?\.git][a-zA-Z0-9].f90"`)
	@printf "Watching following file(s)...\n$(FILES)\n"
	@[ command -v inotifywait >/dev/null 2>&1 ] && exit || true;
	@inotifywait -q -m -e modify $(FILES) | \
		while read -r filename event; do \
			make && printf "\n\nWrtching following file(s)...\n$(FILES);\n"; \
		done;


clean:
	$(RM) $(OBJDIR)/*.o
	$(RM) $(LIBDIR)/*.a
	$(RM) $(INCLUDEDIR)/*.mod
	@for lib in $(PKGS); do \
		make clean -C $(LIBDIR)/$$lib; \
	done;
