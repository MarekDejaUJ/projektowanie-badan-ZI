# Pakiety LaTeX używane przez PDF pracy studenta. Render nie doinstalowuje ich
# w trakcie oddania, dlatego przygotuj_pdf() instaluje cały zestaw z góry.
pakiety_latex <- function() {
  c("xetex", "fontspec", "unicode-math", "lm", "lm-math", "amsmath", "amsfonts", "babel",
    "babel-polish", "hyphen-polish", "latex", "tools", "graphics", "graphics-cfg",
    "graphics-def", "geometry", "hyperref", "bookmark", "booktabs", "etoolbox", "fancyvrb",
    "float", "footnotehyper", "framed", "iftex", "l3kernel", "l3packages", "microtype",
    "parskip", "upquote", "url", "xcolor", "xurl", "bigintcalc", "bitset", "gettitlestring",
    "hycolor", "infwarerr", "intcalc", "kvdefinekeys", "kvoptions", "kvsetkeys", "ltxcmds",
    "pdfescape", "pdftexcmds", "refcount", "rerunfilecheck", "stringenc", "uniquecounter")
}

# TinyTeX instalowany w trakcie sesji (np. w Posit Cloud) nie zawsze jest w PATH.
dodaj_tinytex_do_path <- function() {
  if (nzchar(Sys.which("xelatex"))) return(invisible(FALSE))
  root <- tryCatch(tinytex::tinytex_root(error = FALSE), error = function(e) "")
  if (!is.character(root) || length(root) != 1L || !nzchar(root) || !dir.exists(root)) return(invisible(FALSE))
  bin <- list.dirs(file.path(root, "bin"), recursive = FALSE)
  bin <- bin[vapply(bin, function(b) any(file.exists(file.path(b, c("xelatex", "xelatex.exe")))), logical(1))]
  if (!length(bin)) return(invisible(FALSE))
  Sys.setenv(PATH = paste(normalizePath(bin[1L], winslash = "/"), Sys.getenv("PATH"), sep = .Platform$path.sep))
  invisible(TRUE)
}

narzedzia_pdf_gotowe <- function() {
  dodaj_tinytex_do_path()
  isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)) && nzchar(Sys.which("xelatex"))
}

zapewnij_narzedzia_pdf <- function(pytaj = interactive()) {
  if (narzedzia_pdf_gotowe()) return(invisible(TRUE))
  if (pytaj && !nzchar(Sys.which("xelatex"))) {
    odp <- zapytaj("Brakuje XeLaTeX do utworzenia PDF. Zainstalowa\u0107 teraz TinyTeX (jednorazowo, kilka minut)? [t/n]: ")
    if (tolower(trimws(odp)) %in% c("t", "tak")) return(invisible(przygotuj_pdf()))
  }
  invisible(FALSE)
}

#' Jednorazowe przygotowanie narzędzi PDF
#'
#' Instaluje TinyTeX, jeśli brakuje XeLaTeX (np. w nowym projekcie Posit Cloud),
#' doinstalowuje pakiety LaTeX używane przez PDF pracy i składa próbny dokument.
#' Ponowne wywołanie tylko sprawdza gotowe narzędzia. Wymaga internetu przy
#' pierwszej instalacji; trwa kilka minut.
#' @param test Czy złożyć próbny dokument XeLaTeX po instalacji.
#' @return TRUE, niewidocznie, gdy XeLaTeX z kompletem pakietów działa.
#' @export
#' @examples
#' \dontrun{ przygotuj_pdf() }
przygotuj_pdf <- function(test = TRUE) {
  dodaj_tinytex_do_path()
  if (!nzchar(Sys.which("xelatex"))) {
    message("Instaluj\u0119 TinyTeX (jednorazowo, kilka minut). Nie zamykaj sesji R.")
    tinytex::install_tinytex()
    dodaj_tinytex_do_path()
  }
  if (!nzchar(Sys.which("xelatex")))
    stop("Nie znaleziono XeLaTeX po instalacji TinyTeX. Uruchom ponownie sesj\u0119 R i powt\u00f3rz przygotuj_pdf().", call. = FALSE)
  if (isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))) {
    brak <- setdiff(pakiety_latex(), tinytex::tl_pkgs())
    if (length(brak)) {
      message("Doinstalowuj\u0119 pakiety LaTeX: ", paste(brak, collapse = ", "))
      tinytex::tlmgr_install(brak)
    }
  }
  if (test) sprawdz_sklad_latex()
  if (!isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)))
    stop("XeLaTeX dzia\u0142a, ale brakuje Pandoc. Uruchom polecenie w RStudio albo Positronie.", call. = FALSE)
  message("Narz\u0119dzia PDF s\u0105 gotowe.")
  invisible(TRUE)
}

# Próbny skład z pakietami szablonu PDF pracy, bez pobierania czegokolwiek.
sprawdz_sklad_latex <- function() {
  tmp <- tempfile("test-latex-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  tex <- c("\\documentclass[11pt,a4paper]{article}",
    "\\usepackage{amsmath,amssymb}", "\\usepackage{fontspec}", "\\usepackage{unicode-math}",
    "\\usepackage[polish]{babel}", "\\usepackage{geometry}", "\\usepackage{booktabs}",
    "\\usepackage{framed}", "\\usepackage{xcolor}", "\\usepackage{fancyvrb}", "\\usepackage{float}",
    "\\usepackage{microtype}", "\\usepackage{parskip}", "\\usepackage{upquote}", "\\usepackage{xurl}",
    "\\usepackage{footnotehyper}", "\\usepackage{bookmark}", "\\usepackage{hyperref}",
    "\\begin{document}", "Za\u017c\u00f3\u0142\u0107 g\u0119\u015bl\u0105 ja\u017a\u0144: $\\bar{x} = 2{,}5$, $\\alpha = 0{,}05$.",
    "\\end{document}")
  writeLines(enc2utf8(tex), file.path(tmp, "test.tex"), useBytes = TRUE)
  wynik <- processx::run(unname(Sys.which("xelatex")), c("-halt-on-error", "-interaction=batchmode", "test.tex"),
    wd = tmp, timeout = 180, error_on_status = FALSE, windows_hide_window = TRUE)
  if (!identical(wynik$status, 0L) || !czy_pdf(file.path(tmp, "test.pdf"))) {
    log <- file.path(tmp, "test.log")
    bledy <- if (file.exists(log)) grep("^!", readLines(log, warn = FALSE), value = TRUE) else character()
    stop("Pr\u00f3bny sk\u0142ad XeLaTeX si\u0119 nie uda\u0142.",
         if (length(bledy)) paste0("\n", paste(utils::head(bledy, 5L), collapse = "\n")),
         "\nUruchom przygotuj_pdf() ponownie; je\u015bli b\u0142\u0105d si\u0119 powtarza, zg\u0142o\u015b go prowadz\u0105cemu.", call. = FALSE)
  }
  invisible(TRUE)
}
