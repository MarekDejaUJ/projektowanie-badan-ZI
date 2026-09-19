sprawdz_render <- function() {
manifest <- yaml::read_yaml("inst/materialy/manifest.yml")$jednostki
zakres <- Sys.getenv("RENDER_SCOPE", "smoke")
pdf <- identical(tolower(Sys.getenv("RENDER_PDF", "false")), "true")
if (!zakres %in% c("smoke", "full")) stop("RENDER_SCOPE musi mieć wartość smoke albo full.")

ids <- if (zakres == "full") vapply(manifest, `[[`, character(1), "id") else c("C05", "W03")
out <- tempfile("render-badaniazi-")
dir.create(out, recursive = TRUE)
on.exit(unlink(out, recursive = TRUE), add = TRUE)
css <- normalizePath("inst/materialy/wspolne/styl.css", winslash = "/", mustWork = TRUE)
preambula <- normalizePath("inst/materialy/wspolne/preambula.tex", winslash = "/", mustWork = TRUE)
matematyka <- if (rmarkdown::pandoc_version() >= "3.11") "--math-method=mathml" else "--mathml"

for (id in ids) {
  typ <- manifest[[match(id, vapply(manifest, `[[`, character(1), "id"))]]$typ
  nazwy <- if (typ == "wyklad") c("pelne", "handout") else "pelne"
  katalog_jednostki <- file.path(out, tolower(id))
  dir.create(katalog_jednostki, recursive = TRUE)
  zrodlo_jednostki <- file.path("inst", "materialy", tolower(id))
  pliki_zrodlowe <- list.files(zrodlo_jednostki, pattern = "[.](Rmd|R)$", full.names = TRUE)
  stopifnot(all(file.copy(pliki_zrodlowe, katalog_jednostki, overwrite = TRUE)))
  for (nazwa in nazwy) {
    input <- normalizePath(file.path(katalog_jednostki, paste0(nazwa, ".Rmd")),
                           winslash = "/", mustWork = TRUE)
    html <- file.path(katalog_jednostki, paste0(nazwa, ".html"))
    knitr::opts_chunk$set(dev = "png", fig.width = 6, fig.height = 3.8,
                          out.width = "100%", dpi = 120)
    rmarkdown::render(input, output_format = rmarkdown::html_document(
      self_contained = TRUE, toc = TRUE, toc_depth = 2, theme = "readable",
      mathjax = NULL, pandoc_args = matematyka, css = css),
      output_file = paste0(nazwa, ".html"), envir = new.env(parent = globalenv()), quiet = TRUE)
    stopifnot(file.exists(html), file.info(html)$size > 1000)
    if (pdf) {
      plik_pdf <- file.path(katalog_jednostki, paste0(nazwa, ".pdf"))
      knitr::opts_chunk$set(dev = "cairo_pdf", fig.width = 6, fig.height = 3.8,
                            out.width = "100%")
      rmarkdown::render(input, output_format = rmarkdown::pdf_document(
        latex_engine = "xelatex", dev = "cairo_pdf", fig_crop = FALSE,
        toc = nazwa == "pelne",
        includes = rmarkdown::includes(in_header = preambula)),
        output_file = paste0(nazwa, ".pdf"), envir = new.env(parent = globalenv()), quiet = TRUE)
      stopifnot(file.exists(plik_pdf), file.info(plik_pdf)$size > 1000)
    }
    cat(id, nazwa, if (pdf) "HTML/PDF" else "HTML", "OK\n")
  }
}
}

sprawdz_render()
