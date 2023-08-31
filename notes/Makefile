# -*- makefile -*-
info ::
	@echo "Do: make pdf"
	@echo "to format the book"

onepass :
	TEXINPUTS=${TEXINPUTS}:graphics:mpireference:snippets:graphics:hwloc \
	    pdflatex TAP
pdf :
	make onepass
	make onepass
	make onepass

clean ::
	@rm -f TAP.pdf acronyms.tex chapternames.tex

info ::
	@echo "Do: make clean"
	@echo "to clean up temporary files"
clean ::
	@/bin/rm -f *.{aux,blg,idx,ilg,log,out,toc} *~ comment.cut \
	 exercises/*.tex
