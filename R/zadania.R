#' Przygotowanie pełnego roboczego Rmd
#'
#' Kopiuje ćwiczenie LEARN i CHALLENGE do jednego pliku zadanie.Rmd wraz
#' z gotowym analiza.R. ID jest pobierane z kurs.yml; nie trzeba wpisywać
#' go ponownie w nagłówku. Ponowne wywołanie nie nadpisuje odpowiedzi ani
#' przypiętego skryptu. Stare pliki Markdown wymagają osobnej migracji.
#' Z10 wymaga zapisanego wariantu Z09 i zachowuje jego kopię bez losowania.
#' @param id Z01--Z10.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @return Bezwzględna ścieżka zadanie.Rmd, niewidocznie.
#' @export
#' @examples
#' k <- tempfile("zadania-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z01", k)
#' unlink(k, recursive = TRUE)
przygotuj_zadanie <- function(id, katalog = NULL) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  katalog <- katalog_kursu(katalog)
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  sprawdz_id(cfg$id, "id")
  konfiguracja_kursu(cfg$rocznik)
  folder <- sciezka_pracy(katalog, file.path("zadania", tolower(id)))
  if (file.exists(folder)) {
    sprawdz_zapisana_prace(folder, id, cfg)
    if (id == "Z10") odczytaj_wariant_zadania(folder, cfg)
    return(invisible(file.path(folder, "zadanie.Rmd")))
  }
  dir.create(dirname(folder), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(paste0(tolower(id), "-"), tmpdir = dirname(folder))
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 katalogu zadania.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  jednostka <- sub("^Z", "c", id)
  wzorzec <- zasob("materialy", jednostka, "pelne.Rmd")
  helper <- zasob("materialy", jednostka, "analiza.R")
  if (id == "Z10") {
    poprzedni <- sciezka_pracy(katalog, "zadania/z09")
    sprawdz_zapisana_prace(poprzedni, "Z09", cfg)
    kopiuj_wariant_zadania(poprzedni, tmp, cfg)
  }
  tekst <- readLines(wzorzec, encoding = "UTF-8", warn = FALSE)
  tekst <- lokalne_odnosniki(rmd_z_id(tekst, cfg$id))
  pisz_linie(tekst, file.path(tmp, "zadanie.Rmd"))
  if (!file.copy(helper, file.path(tmp, "analiza.R")))
    stop("Nie mo\u017cna skopiowa\u0107 gotowego skryptu. Zachowano istniej\u0105c\u0105 prac\u0119.", call. = FALSE)
  zapisz_literature_pracy(tmp)
  meta <- list(format_pracy = "rmd-1", zadanie = id, id_studenta = cfg$id,
    rocznik = cfg$rocznik, wersja_pakietu = as.character(utils::packageVersion("badaniaZI")),
    plik = "zadanie.Rmd", pomocniczy_R = "analiza.R",
    sha256_wzorca = digest::digest(file = wzorzec, algo = "sha256"),
    sha256_analiza = digest::digest(file = helper, algo = "sha256"))
  pisz_linie(yaml::as.yaml(meta), file.path(tmp, "zadanie.yml"))
  sprawdz_zapisana_prace(tmp, id, cfg)
  if (!file.rename(tmp, folder))
    stop("Nie mo\u017cna zapisa\u0107 nowego zadania. Nie nadpisuj\u0119 istniej\u0105cego katalogu.", call. = FALSE)
  invisible(file.path(folder, "zadanie.Rmd"))
}

#' Lokalna kontrola Rmd i utworzenie aktualnego PDF
#'
#' Sprawdza własne pola odpowiedzi oraz zgodność gotowego kodu, ID i danych.
#' Nie ocenia sensu interpretacji ani długości akapitów. Domyślnie tworzy PDF
#' od początku w świeżym procesie R, bez logowania i bez dostępu do tokenu
#' sesji. Wymaga narzędzi PDF przygotowanych przez informatyka.
#' Kontrola nie wysyła pracy. Brak sieci nie blokuje tworzenia PDF.
#' @param id Z01--Z10 albo PROJEKT.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @param uruchom Czy utworzyć aktualny PDF. FALSE tylko sprawdza zapisany
#'   PDF i jego zgodność z bieżącymi wejściami, bez wykonywania kodu.
#' @return Lista: ok, tabela kontroli i jawna lista plików oddania.
#' @export
#' @examples
#' k <- tempfile("kontrola-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z01", k)
#' sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok
#' unlink(k, recursive = TRUE)
sprawdz_zadanie <- function(id, katalog = NULL, uruchom = TRUE) {
  katalog <- katalog_kursu(katalog)
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  if (!is.logical(uruchom) || length(uruchom) != 1L || is.na(uruchom))
    stop("uruchom musi mie\u0107 warto\u015b\u0107 TRUE albo FALSE.", call. = FALSE)
  kontrole <- data.frame(element = character(), ok = logical(), opis = character())
  etap <- function(nazwa, kod) {
    blad <- NULL
    wynik <- tryCatch(kod(), error = function(e) {
      blad <<- conditionMessage(e)
      NULL
    })
    kontrole <<- rbind(kontrole, data.frame(element = nazwa, ok = is.null(blad),
      opis = if (is.null(blad)) "Poprawnie." else blad))
    wynik
  }
  wejscia <- etap("Rmd, odpowiedzi, gotowy kod i wej\u015bcia", function() kontroluj_wejscia_rmd(id, katalog))
  pliki <- character()
  if (!is.null(wejscia)) {
    etap("Zapis edytora", function() sprawdz_zapis_edytora(file.path(wejscia$folder, wejscia$rmd)))
    if (all(kontrole$ok)) {
      etap(if (uruchom) "Odtworzenie PDF" else "Aktualno\u015b\u0107 PDF", function() {
        if (uruchom) renderuj_prace(wejscia)
        sprawdz_aktualnosc_pdf(wejscia)
      })
    }
    if (all(kontrole$ok)) pliki <- etap("Jawna lista plik\u00f3w", function() pliki_oddania(id, katalog))
  }
  list(ok = all(kontrole$ok), kontrole = kontrole, pliki = pliki)
}


rscript_bin <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

pliki_oddania <- function(id, katalog) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  root <- katalog_kursu(katalog)
  pliki <- c(pliki_wejscia_pracy(id), pliki_pdf_pracy(id))
  for (p in pliki) {
    f <- sciezka_pracy(root, p)
    if (!file.exists(f) || dir.exists(f)) stop("Brakuje pliku oddania: ", p, ".", call. = FALSE)
  }
  pliki
}
