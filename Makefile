
default: project.pdf
	echo "Done"

%.md:	%.Rmd
	r -e "library(knitr); knit(\"$<\",encoding=\"UTF-8\")"

%.html: %.Rmd
	r -e "library(knitr); knit2html(\"$<\",encoding=\"UTF-8\")"

%.pdf:	%.md
	/usr/lib/rstudio/bin/pandoc/pandoc $< --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output $@ --template /home/jdamon/Tools/R/3.1/lib/R/library/rmarkdown/rmd/latex/default.tex --highlight-style tango --latex-engine pdflatex --variable geometry:margin=1in



