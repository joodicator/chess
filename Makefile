SRCDIR := .
LIBDIR := lib
OBJDIR := obj
BINDIR := bin

GHCFLAGS := -O2 -hidir $(OBJDIR) -odir $(OBJDIR) -i$(LIBDIR)

$(BINDIR)/Main: $(SRCDIR)/Main.hs $(LIBDIR)/*
	ghc --make $(GHCFLAGS) -o $@ $<

$(BINDIR)/ChessMinMaxAITest: $(SRCDIR)/ChessMinMaxAITest.hs $(LIBDIR)/*
	ghc --make $(GHCFLAGS) -o $@ $<

.phony: aitest
aitest: $(BINDIR)/ChessMinMaxAITest

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
