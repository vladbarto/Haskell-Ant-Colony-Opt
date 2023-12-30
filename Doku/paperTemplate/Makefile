DOCUMENT=v1
PDFVIEWER=acroread

#no modifications below this line
all: zap
	latex $(DOCUMENT).tex
	bibtex $(DOCUMENT) 
	latex $(DOCUMENT).tex
	latex $(DOCUMENT).tex
	dvips -t a4 -Ppdf -f $(DOCUMENT).dvi > $(DOCUMENT).ps
	ps2pdf $(DOCUMENT).ps > $(DOCUMENT).pdf
	make clean
	$(PDFVIEWER) $(DOCUMENT).pdf&

clean:
	rm -f *.ps *.dvi *.log *.toc *.aux *.bbl *.blg core *~ *.log *.backup

zap: clean
	rm -f *.pdf 
