SRCDIR := .
LIBDIR := lib
OBJDIR := obj
BINDIR := bin

_GHCFLAGS := $(GHCFLAGS) -O2 -hidir $(OBJDIR) -odir $(OBJDIR) -i$(LIBDIR)

$(BINDIR)/Main: $(SRCDIR)/Main.hs $(LIBDIR)/*
	ghc --make $(_GHCFLAGS) -o $@ $<

$(BINDIR)/ChessMinMaxAITest: $(SRCDIR)/ChessMinMaxAITest.hs $(LIBDIR)/*
	ghc --make $(_GHCFLAGS) -o $@ $<

.phony: aitest
aitest: $(BINDIR)/ChessMinMaxAITest

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
