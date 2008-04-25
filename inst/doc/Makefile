all:	pdf cellhts2Complete clean


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

cellhts2Complete: cellhts2Complete.tex
	unzip -o ../scripts/cellhts2Complete.zip

clean:
	rm -f *.out *.bbl *.log *.aux *.blg *.brf *.toc *.tex
	rm -f *.dvi *.ps Rplots.pdf cellhts2-* twoChannels-* twoWay-* tmp_* template-Description.txt
	rm -rf report-raw report-normalized raw logRatio 2Wraw 2Wnormalized *.rda


