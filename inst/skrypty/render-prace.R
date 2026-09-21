# Wywołuje go pakiet dopiero po statycznej kontroli całego wejścia.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[1L]))
wejscie <- normalizePath(args[1L], winslash = "/", mustWork = TRUE)
options(tinytex.install_packages = FALSE)
tex <- rmarkdown::render(wejscie,
  output_format = rmarkdown::latex_document(latex_engine = "xelatex", dev = "cairo_pdf",
    fig_crop = FALSE, md_extensions = "-raw_attribute", pandoc_args = "--sandbox"),
  runtime = "static", knit_root_dir = dirname(wejscie),
  envir = new.env(parent = globalenv()), quiet = TRUE)
silnik <- unname(Sys.which("xelatex"))
stopifnot(nzchar(silnik), file.exists(tex))
wersja <- processx::run(silnik, "--version", timeout = 10, windows_hide_window = TRUE)$stdout
opcje <- if (grepl("MiKTeX", wersja, fixed = TRUE)) c("--disable-installer", "--disable-write18") else "-no-shell-escape"
for (i in seq_len(3L)) {
  wynik <- processx::run(silnik,
    c(opcje, "-halt-on-error", "-interaction=batchmode", basename(tex)),
    wd = dirname(tex), timeout = 60, cleanup_tree = TRUE,
    windows_hide_window = TRUE, error_on_status = FALSE)
  if (!identical(wynik$status, 0L)) stop("Błąd składu PDF. Sprawdź narzędzia stanowiska i zapis akapitów.")
}
stopifnot(file.exists(sub("[.]tex$", ".pdf", tex)))
