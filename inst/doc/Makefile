all:	pdf cellhts2Complete


pdf:	twoChannels.tex cellhts2.tex twoWay.tex
	texi2dvi --pdf --clean cellhts2.tex
	texi2dvi --pdf --clean twoChannels.tex
	texi2dvi --pdf --clean twoWay.tex
	make clean

cellhts2Complete: cellhts2Complete.tex
	unzip -o ../scripts/cellhts2Complete.zip

clean:
	rm -f *.dvi *.ps Rplots.pdf cellhts2-* twoChannels-* twoWay-* tmp_* template-Description.txt
	rm -f *.tex
	rm -rf report-raw report-normalized raw logRatio 2Wraw 2Wnormalized *.rda


