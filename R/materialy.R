#' Syntetyczny przykład do wspólnej analizy
#' @return Surowa tabela ankietowa S02 dla demonstracyjnego ID demo001.
#' @export
#' @examples
#' head(dane_przykladowe())
dane_przykladowe <- function() {
  utils::read.csv(zasob("extdata", "przyklad", "surowe.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
}

#' Plik przykładowych danych
#' @param format CSV albo XLSX.
#' @return Ścieżka do pliku z pakietu.
#' @export
#' @examples
#' plik_przykladu("csv")
plik_przykladu <- function(format = c("csv", "xlsx")) {
  format <- match.arg(format)
  zasob("extdata", "przyklad", if (format == "csv") "surowe.csv" else "ankieta.xlsx")
}

#' Katalog materiałów rocznika
#' @return Tabela jednostek, tytułów i lokalnych ścieżek źródeł.
#' @export
#' @examples
#' materialy()
materialy <- function() {
  x <- czytaj_yaml(zasob("materialy", "manifest.yml"))$jednostki
  data.frame(id = vapply(x, `[[`, character(1), "id"),
             tytul = vapply(x, `[[`, character(1), "tytul"),
             typ = vapply(x, `[[`, character(1), "typ"))
}

#' Dostęp do materiału lokalnego lub Pages
#' @param id Identyfikator jednostki, np. C01 lub W01.
#' @param format html, pdf, Rmd, R albo tex.
#' @param handout Czy otworzyć handout.
#' @param zrodlo lokalne albo pages.
#' @param otworz Czy uruchomić przeglądarkę lub otworzyć plik.
#' @return Istniejąca ścieżka lub adres Pages, niewidocznie.
#' @export
#' @examples
#' \dontrun{ otworz_material("C01") }
otworz_material <- function(id, format = c("html", "pdf", "Rmd", "R", "tex"),
                            handout = FALSE, zrodlo = c("lokalne", "pages"), otworz = TRUE) {
  format <- match.arg(format)
  zrodlo <- match.arg(zrodlo)
  id <- toupper(id)
  if (!id %in% materialy()$id) stop("Nie ma takiej jednostki w katalogu materia\u0142\u00f3w.", call. = FALSE)
  nazwa <- if (handout) "handout" else if (format == "R") "analiza" else "pelne"
  rel <- file.path("materialy", tolower(id), paste0(nazwa, ".", format))
  if (zrodlo == "lokalne") {
    sciezka <- zasob(rel)
  } else sciezka <- paste0("https://MarekDejaUJ.github.io/projektowanie-badan-ZI/", gsub("\\\\", "/", rel))
  if (otworz) utils::browseURL(sciezka)
  invisible(sciezka)
}

#' Kontrola środowiska pracy
#'
#' Nie instaluje pakietów ani nie zmienia konfiguracji. LaTeX jest potrzebny
#' do budowania PDF, lecz nie do wykonywania zadań lub korzystania z gotowych materiałów.
#' @return Tabela narzędzi, stanów i znaczenia braków.
#' @export
#' @examples
#' sprawdz_srodowisko()
sprawdz_srodowisko <- function() {
  narzedzie <- c("R >= 4.3", "badaniaZI", "Git/libgit2", "GitHub CLI", "R Markdown", "XeLaTeX", "katalog roboczy", "IDE")
  pakiet <- function(x) requireNamespace(x, quietly = TRUE)
  ide <- Sys.getenv("RSTUDIO") == "1" || nzchar(Sys.getenv("POSITRON_VERSION"))
  ok <- c(getRversion() >= "4.3.0", TRUE, pakiet("gert"), nzchar(Sys.which("gh")),
          pakiet("rmarkdown"), nzchar(Sys.which("xelatex")), file.access(getwd(), 2) == 0L, ide)
  data.frame(narzedzie = narzedzie, dostepne = ok,
             znaczenie = c("Wymagane", "Zainstalowany pakiet kursu", "Wymagane do pobierania i oddawania",
               "Wymagane dla logowania gh; alternatywa device z client ID", "Opcjonalne: budowanie w\u0142asnego HTML/PDF",
               "Opcjonalne: budowanie PDF", "Wymagane do zapisania pracy", "RStudio zalecane; konsola R r\u00f3wnie\u017c dzia\u0142a"))
}
