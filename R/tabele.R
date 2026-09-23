#' Tabela Markdown z zawijaniem tekstu
#'
#' Tworzy tabele w skladni pipe z podpisem. Liczba kresek w linii naglowka
#' wyznacza wzgledne szerokosci kolumn, wiec dlugi tekst zawija sie w PDF
#' i w HTML. Wynik drukuje sie w dokumencie knitr jako gotowy Markdown.
#' @param x Ramka danych.
#' @param szerokosci Wzgledne szerokosci kolumn, np. c(1, 3, 2).
#' @param podpis Podpis tabeli.
#' @return Obiekt knit_asis z tekstem tabeli.
#' @export
#' @examples
#' tabela_markdown(data.frame(Kod = "W01", Opis = "Wyklad"), c(1, 4), "Przyklad")
tabela_markdown <- function(x, szerokosci, podpis) {
  stopifnot(is.data.frame(x), ncol(x) >= 1L, length(szerokosci) == ncol(x),
            all(szerokosci > 0), is.character(podpis), length(podpis) == 1L)
  komorka <- function(v) gsub("|", "\\|", gsub("\n", " ", as.character(v)), fixed = TRUE)
  wiersz <- function(v) paste0("| ", paste(komorka(v), collapse = " | "), " |")
  kreski <- pmax(3L, round(120 * szerokosci / sum(szerokosci)))
  tekst <- c(wiersz(names(x)), paste0("|", paste(strrep("-", kreski), collapse = "|"), "|"),
             vapply(seq_len(nrow(x)), function(i) wiersz(unlist(x[i, , drop = TRUE])), character(1)),
             "", paste0("Table: ", podpis))
  knitr::asis_output(paste(c("", tekst, ""), collapse = "\n"))
}
