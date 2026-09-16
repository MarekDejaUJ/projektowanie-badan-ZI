#' Wynik tej realizacji kursu
#'
#' Dziesięć zadań ma jednakową wagę w części zadaniowej. Za spóźniony projek
#' maksymalna ocena końcowa wynosi 4,5. Brak punktacji nie oznacza zera.
#' @param punkty_zadan Dziesięć ocen Z01--Z10 od 0 do 10; NA oznacza brak oceny.
#' @param punkty_projektu Ocena projektu od 0 do 100 lub NA.
#' @param spozniony Czy pierwsze kwalifikujące się oddanie projektu było po terminie.
#' @param konfiguracja Konfiguracja rocznika z wagami i progami.
#' @return Lista: stan, wynik procentowy i ocena; przy braku ocen wynik i ocena są NA.
#' @expor
#' @examples
#' oblicz_ocene(rep(8, 10), 90)
#' oblicz_ocene(rep(10, 10), 100, spozniony = TRUE)
oblicz_ocene <- function(punkty_zadan, punkty_projektu, spozniony = FALSE,
                        konfiguracja = konfiguracja_kursu()) {
  if (!is.numeric(punkty_zadan) || length(punkty_zadan) != 10L ||
      any(punkty_zadan < 0 | punkty_zadan > 10, na.rm = TRUE) ||
      !is.numeric(punkty_projektu) || length(punkty_projektu) != 1L ||
      any(punkty_projektu < 0 | punkty_projektu > 100, na.rm = TRUE))
    stop("Podaj 10 punktacji zada\u0144 0--10 i jedn\u0105 punktacj\u0119 projektu 0--100.", call. = FALSE)
  if (!is.logical(spozniony) || length(spozniony) != 1L || is.na(spozniony))
    stop("Ustal sp\u00f3\u017anienie na podstawie serwerowego pokwitowania.", call. = FALSE)
  problemy <- sprawdz_konfiguracje(konfiguracja)
  if (any(problemy$pole %in% c("wagi", "progi_ocen", "maksymalna_ocena_spozniona"))) stop("Nie ustalono poprawnych wag, prog\u00f3w lub limitu sp\u00f3\u017anienia.", call. = FALSE)
  if (anyNA(punkty_zadan) || is.na(punkty_projektu)) return(list(stan = "brak_oceny", procent = NA_real_, ocena = NA_real_))
  w <- unlist(konfiguracja$wagi)
  procent <- mean(punkty_zadan) * 10 * w[["zadania"]] + punkty_projektu * w[["projekt"]]
  progi <- unlist(konfiguracja$progi_ocen)
  kwalifikuje <- which(procent >= progi)
  ocena <- if (length(kwalifikuje)) as.numeric(names(progi)[max(kwalifikuje)]) else 2
  if (spozniony) ocena <- min(ocena, konfiguracja$maksymalna_ocena_spozniona)
  list(stan = "oceniono", procent = procent, ocena = ocena)
}
