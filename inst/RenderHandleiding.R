#Dit korte script laat toe om de handleiding als rapport (pdf) te renderen

library(dhcurve)
library(rprojroot)
setwd(find_root_file("vignettes", criterion = is_git_root))
bookdown::render_book("Handleiding.Rmd", output_format = "bookdown::pdf_book")

#om de handleiding als gitbook-pagina te renderen, de laatste lijn code vervangen door:
bookdown::render_book("Handleiding.Rmd", output_format = "bookdown::gitbook")
