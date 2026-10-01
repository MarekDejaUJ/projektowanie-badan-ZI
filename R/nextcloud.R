# Folder „File drop” w Nextcloud przyjmuje PUT bez logowania. Nie pozwala
# przeglądać ani nadpisywać plików: ponowne oddanie trafia obok poprzedniego.
wzorzec_folderu <- "^https://[A-Za-z0-9.-]+/(index[.]php/)?s/[A-Za-z0-9]+$"

folder_oddania <- function(id, konfiguracja) {
  link <- konfiguracja$foldery_oddania[[id]]
  if (!is.character(link) || length(link) != 1L || is.na(link) || !grepl(wzorzec_folderu, link))
    stop("Konfiguracja rocznika nie ma folderu oddania dla ", id, ". Zg\u0142o\u015b to prowadz\u0105cemu.", call. = FALSE)
  link
}

adres_wysylki <- function(link, nazwa) {
  paste0(sub("^(https://[^/]+)/.*$", "\\1", link), "/public.php/dav/files/",
         sub("^.*/s/", "", link), "/", utils::URLencode(nazwa, reserved = TRUE))
}

# Jedyne miejsce, które łączy się z serwerem; testy podstawiają tu atrapę.
nc_put <- function(plik, url, naglowki) {
  curl::curl_upload(plik, url, verbose = FALSE, httpheader = naglowki,
                    connecttimeout = 30, timeout = 300)
}

pauza_ponowienia <- function() 5

# NULL oznacza przyjęcie pliku; tekst opisuje przyczynę odmowy.
wyslij_do_folderu <- function(plik, nazwa, link, podpis) {
  naglowki <- c("X-Requested-With: XMLHttpRequest",
                paste0("X-NC-Nickname: ", utils::URLencode(enc2utf8(podpis), reserved = TRUE)))
  # Po błędzie sieci jedna ponowna próba; odmowa serwera (kod HTTP) nie jest powtarzana.
  for (proba in 1:2) {
    odp <- tryCatch(nc_put(plik, adres_wysylki(link, nazwa), naglowki), error = function(e) e)
    if (!inherits(odp, "error") || proba == 2L) break
    Sys.sleep(pauza_ponowienia())
  }
  if (inherits(odp, "error"))
    return(paste0("brak po\u0142\u0105czenia z serwerem (", conditionMessage(odp), ")"))
  kod <- odp$status_code
  # Nextcloud zgłasza usunięty lub wygasły udział jako 503 z NotFound w treści.
  if (identical(kod, 503L) && grepl("NotFound", rawToChar(odp$content[odp$content != as.raw(0)]), fixed = TRUE))
    kod <- 404L
  switch(as.character(kod),
         "201" = , "204" = NULL,
         "404" = "folder nie przyjmuje ju\u017c plik\u00f3w (min\u0105\u0142 termin albo link wygas\u0142)",
         "401" = , "403" = "folder nie przyjmuje plik\u00f3w bez has\u0142a albo zosta\u0142 zamkni\u0119ty",
         "413" = "plik jest za du\u017cy",
         "507" = "w folderze prowadz\u0105cego zabrak\u0142o miejsca",
         paste0("serwer odrzuci\u0142 plik (HTTP ", kod, ")"))
}
