all:	pdf clean


pdf:	twoChannels.tex cellhts2.tex twoWay.tex
	pdflatex cellhts2
	bibtex cellhts2
	pdflatex cellhts2
	pdflatex cellhts2
	pdflatex twoChannels
	bibtex twoChannels
	pdflatex twoChannels
	pdflatex twoChannels
	pdflatex twoWay
	bibtex twoWay
	pdflatex twoWay
	pdflatex twoWay

clean:
	rm -f *.out *.bbl *.log *.aux *.blg *.brf *.toc *.tex
	rm -f *.dvi *.ps cellhts2-* twoChannels-* twoWay-* tmp_*
	rm -rf raw logRatio 2Wraw 2Wnormalized KcViab.rda  DualChannelScreen.rda TwoWayAssay.rda


