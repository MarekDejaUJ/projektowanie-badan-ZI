#' Slownik zmiennych przykladu S02
#'
#' Odczytuje slownik danych dolaczony do pakietu: nazwe kolumny, opis
#' (dla pozycji ankiety pelne brzmienie stwierdzenia), skale pomiaru, kodowanie,
#' zapis brakow i informacje o pozycji odwroconej.
#' @param zmienne Opcjonalny wektor nazw kolumn; domyslnie wszystkie.
#' @return Ramka danych z kolumnami zmienna, opis, skala, kodowanie, braki,
#'   odwrocona.
#' @export
#' @examples
#' slownik_zmiennych(c("czas_wyszukiwania", "powodzenie"))
slownik_zmiennych <- function(zmienne = NULL) {
  x <- utils::read.csv(zasob("extdata", "przyklad", "slownik.csv"), encoding = "UTF-8",
                       stringsAsFactors = FALSE)
  if (!is.null(zmienne)) {
    brak <- setdiff(zmienne, x$zmienna)
    if (length(brak)) stop("S\u0142ownik nie zawiera kolumn: ", paste(brak, collapse = ", "), call. = FALSE)
    x <- x[match(zmienne, x$zmienna), , drop = FALSE]
  }
  rownames(x) <- NULL
  x
}

#' Mapa jednostek kursu
#'
#' Tabela wykladow i cwiczen z manifestu materialow: kod jednostki, rodzaj,
#' tytul i jedno zdanie o tresci. Kody zaczynajace sie od W oznaczaja wyklady,
#' od C cwiczenia.
#' @return Ramka danych z kolumnami kod, rodzaj, tytul, tresc.
#' @export
#' @examples
#' mapa_kursu()
mapa_kursu <- function() {
  x <- czytaj_yaml(zasob("materialy", "manifest.yml"))$jednostki
  d <- data.frame(kod = vapply(x, `[[`, character(1), "id"),
                  rodzaj = ifelse(vapply(x, `[[`, character(1), "typ") == "wyklad",
                                  "wyk\u0142ad", "\u0107wiczenie"),
                  tytul = vapply(x, `[[`, character(1), "tytul"),
                  tresc = vapply(x, function(j) if (is.null(j$opis)) "" else j$opis, character(1)))
  d <- d[order(substring(d$kod, 1, 1) != "W", d$kod), , drop = FALSE]
  rownames(d) <- NULL
  d
}
