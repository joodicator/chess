SRCDIR := .
OBJDIR := obj
BINDIR := bin

$(BINDIR)/Main: $(SRCDIR)/Main.hs
	ghc --make -O -hidir $(OBJDIR) -odir $(OBJDIR) \
	    -i$(SRCDIR) -o $(BINDIR)/Main $(SRCDIR)/*.hs

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
