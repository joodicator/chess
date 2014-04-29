SRCDIR := .
OBJDIR := obj
BINDIR := bin

$(BINDIR)/Main: $(SRCDIR)/Main.hs
	ghc -hidir $(OBJDIR) -odir $(OBJDIR) \
	    -i$(SRCDIR) -o $(BINDIR)/Main $(SRCDIR)/Main.hs

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
