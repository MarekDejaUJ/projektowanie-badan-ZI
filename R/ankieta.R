#' Rekodacja pozycji odwróconej
#' @param x Numeryczne odpowiedzi od min do max, z dopuszczalnym NA.
#' @param min,max Granice skali.
#' @return Wektor odwróconych odpowiedzi; braki pozostają brakami.
#' @export
#' @examples
#' odwroc_pozycje(c(1, 3, 5, NA))
odwroc_pozycje <- function(x, min = 1, max = 5) {
  if (!is.numeric(min) || !is.numeric(max) || length(min) != 1L || length(max) != 1L ||
      !is.finite(min) || !is.finite(max) || min >= max)
    stop("Podaj sko\u0144czone granice skali: min mniejsze od max.", call. = FALSE)
  if (!is.numeric(x) || any(!is.finite(x[!is.na(x)])) || any(x < min | x > max, na.rm = TRUE))
    stop("Odpowiedzi musz\u0105 mie\u015bci\u0107 si\u0119 w granicach skali; najpierw usu\u0144 kody brak\u00f3w.", call. = FALSE)
  max + min - x
}

#' Średni indeks z pozycji ankiety
#' @param pozycje Ramka lub macierz numerycznych, już zgodnie ukierunkowanych pozycji.
#' @param minimum Minimalna liczba ważnych odpowiedzi.
#' @return Wektor średnich; NA dla zbyt małej liczby odpowiedzi.
#' @export
#' @examples
#' indeks_ankiety(data.frame(p1 = c(1, NA), p2 = c(3, 5)), minimum = 2)
indeks_ankiety <- function(pozycje, minimum = 5L) {
  if (!(is.data.frame(pozycje) || is.matrix(pozycje)) || !ncol(pozycje) ||
      !all(vapply(as.data.frame(pozycje), is.numeric, logical(1))))
    stop("Podaj tabel\u0119 numerycznych pozycji, po rekodacji i oczyszczeniu.", call. = FALSE)
  if (!is.numeric(minimum) || length(minimum) != 1L || !is.finite(minimum) ||
      minimum < 1 || minimum > ncol(pozycje) || minimum != as.integer(minimum))
    stop("Niepoprawne minimum odpowiedzi.", call. = FALSE)
  m <- as.matrix(pozycje)
  if (any(!is.finite(m[!is.na(m)])) || any(m < 1 | m > 5, na.rm = TRUE))
    stop("Indeks kursowy wymaga odpowiedzi od 1 do 5; kod 99 jest brakiem.", call. = FALSE)
  wynik <- rowMeans(m, na.rm = TRUE)
  wynik[rowSums(!is.na(m)) < minimum] <- NA_real_
  wynik
}

#' Odsetki odpowiedzi wielokrotnych
#' @param dane Tabela kolumn 0/1, z dopuszczalnym NA.
#' @return Liczebności, mianowniki i procent respondentów. Suma procentów może przekraczać 100.
#' @export
#' @examples
#' odpowiedzi_wielokrotne(data.frame(www = c(1, 0, NA), email = c(1, 1, NA)))
odpowiedzi_wielokrotne <- function(dane) {
  if (!is.data.frame(dane) || !ncol(dane) ||
      !all(vapply(dane, function(x) is.numeric(x) && all(is.na(x) | x %in% 0:1), logical(1))))
    stop("Podaj tabel\u0119 numerycznych odpowiedzi 0/1/NA.", call. = FALSE)
  # Częściowo pusta odpowiedź nie jest traktowana jako kompletna odpowiedź 0.
  wazna <- stats::complete.cases(dane)
  mianownik <- sum(wazna)
  liczba <- colSums(dane[wazna, , drop = FALSE])
  data.frame(opcja = names(dane), liczba = as.integer(liczba),
             mianownik = mianownik,
             procent_respondentow = if (mianownik) 100 * liczba / mianownik else NA_real_,
             pominiete_niepelne = sum(!wazna), row.names = NULL)
}

#' Jawne przygotowanie danych z generatora
#'
#' Funkcja pokazuje referencyjne reguły czyszczenia: identyczne duplikaty,
#' kod 99 w pozycjach ankiety i czas spoza 0--120 minut. Nie oblicza indeksu.
#' @param dane Surowa tabela z generuj_dane().
#' @return Lista: dane i dziennik zastosowanych reguł.
#' @export
#' @examples
#' x <- przygotuj_ankiete(generuj_dane("s017")$dane)
#' x$dziennik
przygotuj_ankiete <- function(dane) {
  wymagane <- c("id_odpowiedzi", "grupa", "czestosc_korzystania", "czas_wyszukiwania",
               paste0("pozycja_", 1:6), "powodzenie", paste0("kanal_", 1:4))
  if (!is.data.frame(dane) || !all(wymagane %in% names(dane))) stop("Brakuje kolumn ankiety.", call. = FALSE)
  n0 <- nrow(dane)
  dane <- unique(dane)
  if (anyDuplicated(dane$id_odpowiedzi)) stop("To samo ID ma r\u00f3\u017cne odpowiedzi; sprawd\u017a konflikt r\u0119cznie.", call. = FALSE)
  num <- setdiff(wymagane, c("id_odpowiedzi", "grupa"))
  if (!all(vapply(dane[num], is.numeric, logical(1)))) stop("Najpierw popraw numeryczne typy kolumn.", call. = FALSE)
  kody <- 0L
  for (j in paste0("pozycja_", 1:6)) {
    w <- which(dane[[j]] == 99)
    kody <- kody + length(w)
    dane[[j]][w] <- NA_real_
    if (any(!is.na(dane[[j]]) & !(dane[[j]] %in% 1:5))) stop("Nieznana warto\u015b\u0107 pozycji: ", j, call. = FALSE)
  }
  w <- which(dane$czas_wyszukiwania < 0 | dane$czas_wyszukiwania > 120)
  dane$czas_wyszukiwania[w] <- NA_real_
  rownames(dane) <- NULL
  list(dane = dane, dziennik = data.frame(
    regula = c("N przed czyszczeniem", "Usuni\u0119te identyczne duplikaty", "Kod 99 zamieniony na NA",
                "Czas poza 0--120 minut zamieniony na NA", "N po czyszczeniu"),
    liczba = c(n0, n0 - nrow(dane), kody, length(w), nrow(dane))))
}
