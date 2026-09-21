args <- commandArgs(trailingOnly = TRUE)
manifest <- yaml::yaml.load(paste(readLines("inst/materialy/manifest.yml", encoding = "UTF-8"), collapse = "\n"), eval.expr = FALSE)$jednostki
wybrane <- if (length(args)) toupper(args) else vapply(manifest, `[[`, character(1), "id")
stopifnot(all(wybrane %in% vapply(manifest, `[[`, character(1), "id")))
css <- normalizePath("inst/materialy/wspolne/styl.css", winslash = "/", mustWork = TRUE)
preambula <- normalizePath("inst/materialy/wspolne/preambula.tex", winslash = "/", mustWork = TRUE)
matematyka <- if (rmarkdown::pandoc_version() >= "3.11") "--math-method=mathml" else "--mathml"
for (id in wybrane) {
  folder <- file.path("inst", "materialy", tolower(id))
  nazwy <- if (startsWith(id, "W")) c("pelne", "handout") else "pelne"
  for (nazwa in nazwy) {
    input <- file.path(folder, paste0(nazwa, ".Rmd"))
    stopifnot(file.exists(input))
    knitr::opts_chunk$set(dev = "png", fig.width = 6, fig.height = 3.8,
                          out.width = "100%", dpi = 150)
    rmarkdown::render(input, output_format = rmarkdown::html_document(
      self_contained = TRUE, toc = TRUE, toc_depth = 2, theme = "readable",
      mathjax = NULL, pandoc_args = matematyka, css = css),
      output_file = paste0(nazwa, ".html"), envir = new.env(parent = globalenv()), quiet = TRUE)
    knitr::opts_chunk$set(dev = "cairo_pdf", fig.width = 6, fig.height = 3.8,
                          out.width = "100%")
    rmarkdown::render(input, output_format = rmarkdown::pdf_document(
      latex_engine = "xelatex", dev = "cairo_pdf", toc = nazwa == "pelne", number_sections = FALSE,
      includes = rmarkdown::includes(in_header = preambula),
      keep_tex = nazwa == "handout"), output_file = paste0(nazwa, ".pdf"),
      envir = new.env(parent = globalenv()), quiet = TRUE)
    # Jednolity zapis artefaktów tekstowych także po renderowaniu na Windows.
    tekstowe <- file.path(folder,paste0(nazwa,c(".html",
      if(nazwa=="handout") ".tex")))
    for(plik in tekstowe) {
      tresc <- paste0(paste(sub("[ \t]+$","",
        readLines(plik,encoding="UTF-8",warn=FALSE)),
        collapse="\n"),"\n")
      writeBin(charToRaw(enc2utf8(tresc)),plik)
    }
    cat(id, nazwa, "HTML i PDF: OK\n")
  }
  if (startsWith(id, "C")) stopifnot(file.exists(file.path(folder, "analiza.R")))
}
