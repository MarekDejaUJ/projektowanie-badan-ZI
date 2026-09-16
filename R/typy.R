#' Klasy kolumn w R
#' @param dane Ramka danych.
#' @return Tabela nazw kolumn, klas R i typów wewnętrznych.
#' @expor
#' @examples
#' tabela_klas_r(data.frame(czas = c(2, 4), grupa = c("A", "B")))
tabela_klas_r <- function(dane) {
    data.frame(zmienna = names(dane), klasa_R = vapply(dane,
        function(x) paste(class(x), collapse = " / "), character(1)),
        typ_wewnetrzny = vapply(dane, typeof, character(1)),
        stringsAsFactors = FALSE)
}
