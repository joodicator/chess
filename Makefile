SRCDIR := .
LIBDIR := lib
OBJDIR := obj
BINDIR := bin

_GHCFLAGS := $(GHCFLAGS) -O2 -hidir $(OBJDIR) -odir $(OBJDIR) -i$(LIBDIR)

$(BINDIR)/Main: $(SRCDIR)/Main.hs $(LIBDIR)/*
	ghc --make $(_GHCFLAGS) -o $@ $<

$(BINDIR)/Server: $(SRCDIR)/Server.hs $(LIBDIR)/*
	ghc --make $(_GHCFLAGS) -o $@ $<

.phony: server
server: $(BINDIR)/Server

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
