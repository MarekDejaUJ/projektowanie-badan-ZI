#' Termin zadania wynikający z kalendarza rocznika
#'
#' Z01--Z09 są oddawane do rozpoczęcia kolejnego ćwiczenia. Z10 wymaga osobnej
#' daty. Nieznane daty są zwracane jako do_ustalenia, bez wymyślania kalendarza.
#' @param id Z01--Z10.
#' @param konfiguracja Konfiguracja rocznika.
#' @return Lista: stan, zasada, data ISO 8601 lub NULL i identyfikator kolejnego ćwiczenia.
#' @export
#' @examples
#' termin_zadania("Z01")
termin_zadania <- function(id, konfiguracja = konfiguracja_kursu()) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  nr <- as.integer(sub("^Z", "", id))
  kolejny <- if (nr < 10L) sprintf("C%02d", nr + 1L) else NULL
  data <- konfiguracja$terminy_zadan[[id]]
  if (is.null(data) && nr < 10L && identical(konfiguracja$zasada_terminow_zadan, "nastepne_cwiczenie"))
    data <- konfiguracja$daty_zajec[[kolejny]]
  if (!is.null(data) && is.na(czas_iso(data))) stop("Niepoprawna data terminu zadania.", call. = FALSE)
  list(stan = if (is.null(data)) "do_ustalenia" else "ustalony",
       zasada = if (nr < 10L && identical(konfiguracja$zasada_terminow_zadan, "nastepne_cwiczenie"))
         "Do rozpocz\u0119cia kolejnych \u0107wicze\u0144" else "Osobny termin z konfiguracji",
       data = data, kolejne_cwiczenie = kolejny)
}
