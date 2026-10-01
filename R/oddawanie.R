#' Oddanie zadania z konsoli R
#'
#' Sprawdza zapisany Rmd (odpowiedzi, gotowy kod, ID i dane), tworzy od
#' początku aktualny PDF w świeżej sesji R i wysyła go do folderu zadania
#' prowadzącego na nc.uj.edu.pl. Nazwa pliku zawiera ID oraz nazwisko i imię,
#' np. s017_kowalska_anna_Z03.pdf. W sesji interaktywnej najpierw otwiera PDF
#' i pyta o potwierdzenie. Ponowne oddanie dodaje nową wersję obok poprzedniej.
#' Funkcja nie przyznaje punktów.
#' @param id Z01--Z10.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @param potwierdz Czy pokazać PDF i zapytać przed wysłaniem.
#' @return Lista: stan, zadanie, nazwy wysłanych plików, czas i sumy SHA-256,
#'   niewidocznie.
#' @export
#' @examples
#' \dontrun{ oddaj_zadanie("Z01") }
oddaj_zadanie <- function(id, katalog = NULL, potwierdz = interactive()) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  oddaj_prace(id, katalog, potwierdz)
}

#' Oddanie indywidualnego projektu ilościowego
#'
#' Tak jak oddaj_zadanie() tworzy i sprawdza PDF raportu, a do folderu
#' projektu wysyła dwa pliki: PDF oraz archiwum ZIP ze źródłami (raport.Rmd,
#' analiza.R, dane wariantu, metadane i PDF), np. s017_kowalska_anna_PROJEKT.pdf
#' i s017_kowalska_anna_PROJEKT_zrodla.zip.
#' @inheritParams oddaj_zadanie
#' @return Lista jak w oddaj_zadanie(), niewidocznie.
#' @export
#' @examples
#' \dontrun{ oddaj_projekt() }
oddaj_projekt <- function(katalog = NULL, potwierdz = interactive()) {
  oddaj_prace("PROJEKT", katalog, potwierdz)
}

oddaj_prace <- function(id, katalog, potwierdz) {
  if (isTRUE(getOption("knitr.in.progress")))
    stop("Uruchom polecenie oddania w konsoli, nie w bloku dokumentu.", call. = FALSE)
  if (!is.logical(potwierdz) || length(potwierdz) != 1L || is.na(potwierdz))
    stop("potwierdz musi mie\u0107 warto\u015b\u0107 TRUE albo FALSE.", call. = FALSE)
  katalog <- katalog_kursu(katalog)
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  if (is.null(cfg$student))
    stop("Brakuje imienia i nazwiska do nazwy pliku oddania. Dopisz je, np.\n",
         "  rozpocznij_zajecia(\"C01\", student = \"Anna Kowalska\")", call. = FALSE)
  nazwa <- nazwa_oddania(cfg$id, cfg$student, id)
  link <- folder_oddania(id, konfiguracja_kursu(cfg$rocznik))
  zapewnij_narzedzia_pdf(potwierdz)
  kontrola <- sprawdz_zadanie(id, katalog)
  if (!kontrola$ok)
    stop(paste(kontrola$kontrole$opis[!kontrola$kontrole$ok], collapse = "\n"),
         "\nNiczego nie wys\u0142ano.", call. = FALSE)
  pdf <- sciezka_pracy(katalog, pliki_pdf_pracy(id)[1L])
  pliki <- stats::setNames(pdf, paste0(nazwa, ".pdf"))
  if (id == "PROJEKT") {
    zip <- spakuj_oddanie(katalog, kontrola$pliki)
    on.exit(unlink(zip), add = TRUE)
    pliki <- c(pliki, stats::setNames(zip, paste0(nazwa, "_zrodla.zip")))
  }
  folder <- if (id == "PROJEKT") "projekt" else paste0("z", as.integer(sub("^Z", "", id)))
  message("\nGotowe do oddania ", id, ":\n",
          paste0("  ", names(pliki), " (", format_rozmiar(file.size(pliki)), ")", collapse = "\n"), "\n",
          "  autor:  ", cfg$student, ", ID ", cfg$id, "\n",
          "  folder: ", folder, " na ", sub("^https://([^/]+)/.*$", "\\1", link))
  ostrzez_po_terminie(id, konfiguracja_kursu(cfg$rocznik))
  if (potwierdz) {
    pokaz_plik(pdf)
    odp <- zapytaj("Obejrzyj PDF. Wys\u0142a\u0107 go prowadz\u0105cemu? [t/n]: ")
    if (!tolower(trimws(odp)) %in% c("t", "tak")) {
      message("Nie wys\u0142ano. PDF zosta\u0142 w ", pdf, ". Gdy b\u0119dziesz gotowy, powt\u00f3rz polecenie oddania.")
      return(invisible(list(stan = "niewyslane", zadanie = id, pliki = character())))
    }
  }
  # Plik mógł zmienić się podczas oglądania PDF: wysyłamy tylko wersję zgodną z Rmd.
  sprawdz_aktualnosc_pdf(kontroluj_wejscia_rmd(id, katalog))
  sumy <- vapply(pliki, function(p) digest::digest(file = p, algo = "sha256"), character(1))
  wyslane <- character()
  for (n in names(pliki)) {
    problem <- wyslij_do_folderu(pliki[[n]], n, link, paste(cfg$student, cfg$id))
    if (!is.null(problem))
      stop("Nie wys\u0142ano pliku ", n, ": ", problem, ".",
           if (length(wyslane)) paste0("\nWys\u0142ano wcze\u015bniej: ", paste(wyslane, collapse = ", "), "."),
           "\nTwoja praca i PDF s\u0105 zapisane: ", pdf,
           "\nSpr\u00f3buj ponownie za chwil\u0119. Je\u015bli b\u0142\u0105d si\u0119 powtarza, otw\u00f3rz ", link,
           " i przeci\u0105gnij plik do okna przegl\u0105darki albo zg\u0142o\u015b problem prowadz\u0105cemu.", call. = FALSE)
    wyslane <- c(wyslane, n)
  }
  czas <- Sys.time()
  zapisz_rejestr_oddan(katalog, id, names(pliki), sumy, file.size(pliki), czas)
  message("Oddano ", id, ": ", paste(names(pliki), collapse = ", "), " do folderu ", folder,
          " (", format(czas, "%d.%m.%Y %H:%M"), ").\n",
          "Je\u015bli poprawisz prac\u0119 przed terminem, oddaj j\u0105 ponownie tym samym poleceniem; nowa wersja trafi obok poprzedniej.")
  invisible(list(stan = "wyslano", zadanie = id, pliki = names(pliki),
                 czas = format(czas, "%Y-%m-%dT%H:%M:%S%z"), sha256 = unname(sumy)))
}

# Odpowiedź i podgląd w osobnych funkcjach, aby dało się je sprawdzić bez konsoli.
zapytaj <- function(tekst) readline(tekst)
pokaz_plik <- function(plik) invisible(tryCatch(utils::browseURL(plik), error = function(e) NULL))

# Archiwum zawiera jawną listę plików oddania ze ścieżkami względnymi.
spakuj_oddanie <- function(katalog, pliki) {
  zip <- tempfile("oddanie-", fileext = ".zip")
  zip::zip(zip, files = pliki, root = katalog, mode = "mirror")
  if (!file.exists(zip) || !setequal(zip::zip_list(zip)$filename, pliki))
    stop("Nie mo\u017cna przygotowa\u0107 archiwum \u017ar\u00f3de\u0142 projektu. Niczego nie wys\u0142ano.", call. = FALSE)
  zip
}

format_rozmiar <- function(bajty) {
  vapply(bajty, function(b) format(structure(b, class = "object_size"), units = "auto", standard = "SI"), character(1))
}

ostrzez_po_terminie <- function(id, konfiguracja) {
  data <- if (id == "PROJEKT") konfiguracja$termin_projektu else termin_zadania(id, konfiguracja)$data
  if (is.null(data)) return(invisible(FALSE))
  termin <- czas_iso(data)
  if (!is.na(termin) && Sys.time() > termin) {
    message("Uwaga: termin oddania ", id, " min\u0105\u0142 ", format(termin, "%d.%m.%Y %H:%M", tz = konfiguracja$timezone),
            ". Prowadz\u0105cy widzi czas przyj\u0119cia pliku; zasady sp\u00f3\u017anie\u0144 opisuje strona kursu.")
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

plik_rejestru <- function(katalog) file.path(katalog, "oddania.csv")

zapisz_rejestr_oddan <- function(katalog, id, nazwy, sumy, rozmiary, czas) {
  wiersze <- data.frame(czas = format(czas, "%Y-%m-%d %H:%M:%S"), zadanie = id, plik = nazwy,
                        bajty = as.numeric(rozmiary), sha256 = unname(sumy))
  plik <- plik_rejestru(katalog)
  utils::write.table(wiersze, plik, sep = ",", row.names = FALSE, col.names = !file.exists(plik),
                     append = file.exists(plik), fileEncoding = "UTF-8", qmethod = "double")
  invisible(plik)
}

#' Lokalny rejestr oddanych prac
#'
#' Każde udane oddanie dopisuje do pliku oddania.csv w przestrzeni pracy
#' czas, nazwę wysłanego pliku, rozmiar i sumę SHA-256. Rejestr pokazuje, co
#' wysłano z tej przestrzeni; prowadzący widzi pliki w swoim folderze.
#' @param id Opcjonalnie Z01--Z10 albo PROJEKT; NULL zwraca wszystkie oddania.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @return Ramka danych oddań, od najstarszego.
#' @export
#' @examples
#' k <- tempfile("rejestr-")
#' utworz_projekt("s017", katalog = k, student = "Anna Kowalska")
#' status_oddania(katalog = k)
#' unlink(k, recursive = TRUE)
status_oddania <- function(id = NULL, katalog = NULL) {
  katalog <- katalog_kursu(katalog)
  if (!is.null(id)) {
    id <- toupper(id)
    sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  }
  plik <- plik_rejestru(katalog)
  pusty <- data.frame(czas = character(), zadanie = character(), plik = character(),
                      bajty = numeric(), sha256 = character())
  x <- if (file.exists(plik)) utils::read.csv(plik, encoding = "UTF-8", stringsAsFactors = FALSE,
                                             colClasses = c("character", "character", "character", "numeric", "character")) else pusty
  if (!is.null(id)) x <- x[x$zadanie == id, , drop = FALSE]
  if (!nrow(x)) message("Brak zapisu oddania", if (!is.null(id)) paste0(" ", id), " w tej przestrzeni pracy.")
  rownames(x) <- NULL
  x
}
