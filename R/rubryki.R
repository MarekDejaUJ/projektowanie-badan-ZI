#' Rubryka zadania lub projektu
#' @param id Z01--Z10 albo PROJEKT; wielkość liter nie ma znaczenia.
#' @return Lista kryteriów i obserwowalnych poziomów z punktacją.
#' @expor
#' @examples
#' rubryka("Z07")$max_points
rubryka <- function(id = "PROJEKT") {
  id <- toupper(id)
  sprawdz_id(id, "rubryka", "^(Z(0[1-9]|10)|PROJEKT)$")
  czytaj_yaml(zasob("rubryki", paste0(id, ".yml")))
}

#' Pusty formularz oceny według rubryki
#' @param id Z01--Z10 albo PROJEKT.
#' @return Tabela kryteriów z pustymi punktami i komentarzami. Nie zawiera ocen studentów.
#' @expor
#' @examples
#' formularz_oceny("Z01")
formularz_oceny <- function(id = "PROJEKT") {
  x <- rubryka(id)$criteria
  data.frame(kryterium = vapply(x, `[[`, character(1), "id"),
             dowod = vapply(x, `[[`, character(1), "evidence"),
             maksimum = vapply(x, `[[`, numeric(1), "max_points"),
             punkty = NA_real_, komentarz = "", stringsAsFactors = FALSE)
}

#' Walidacja punktów formularza
#' @param formularz Wypełniony formularz_oceny().
#' @param id Identyfikator rubryki.
#' @return Łączna punktacja; błąd przy niekompletnej lub niedozwolonej ocenie.
#' @expor
#' @examples
#' x <- formularz_oceny("Z01")
#' x$punkty <- x$maksimum
#' sprawdz_ocene(x, "Z01")
sprawdz_ocene <- function(formularz, id = "PROJEKT") {
  r <- rubryka(id)
  if (!is.data.frame(formularz) || !all(c("kryterium", "punkty", "komentarz") %in% names(formularz)) ||
      !is.numeric(formularz$punkty) || anyNA(formularz$punkty) || anyDuplicated(formularz$kryterium) ||
      !setequal(formularz$kryterium, vapply(r$criteria, `[[`, character(1), "id")))
    stop("Uzupe\u0142nij ka\u017cde kryterium rubryki dok\u0142adnie raz.", call. = FALSE)
  for (k in r$criteria) {
    p <- formularz$punkty[match(k$id, formularz$kryterium)]
    poziomy <- vapply(k$levels, `[[`, numeric(1), "points")
    if (!(p %in% poziomy)) stop("Wybierz punktacj\u0119 jednego z poziom\u00f3w: ", k$id, call. = FALSE)
  }
  sum(formularz$punkty)
}
