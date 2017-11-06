.PHONY: pdf

pdf : output/example.pdf

output/example.md : example.Rmd
	@mkdir -p $(@D)
	Rscript \
		-e "require(knitr)" \
		-e "knitr::render_markdown()"\
		-e "knitr::knit('$<', '$@')"

output/example.pdf : output/example.md
	@echo --Generating PDF from Markdown--
	pandoc $< -V geometry:margin=1.0in --from=markdown -t latex -s -o $@ -V fontsize=12pt

