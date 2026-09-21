#' Przygotowanie raportu z zapisanych odpowiedzi Z09 i Z10
#'
#' Nowy raport.Rmd otrzymuje dziesięć własnych odpowiedzi według mapy kursu,
#' gotowy analiza.R i kopię tego samego wariantu danych. P11 pozostaje miejscem
#' na bibliografię i opis faktycznie wykorzystanego wsparcia. Nie trzeba
#' przepisywać akapitów; nieuzupełnione odpowiedzi pozostają pustymi polami.
#' Ponowne wywołanie zwraca istniejący raport bez kopiowania tekstu na nowo.
#' @param katalog Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.
#' @return Bezwzględna ścieżka raport.Rmd, niewidocznie.
#' @export
#' @examples
#' k <- tempfile("raport-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z09", k)
#' wariant_projektu("Z09", "S02", katalog = k)
#' przygotuj_zadanie("Z10", k)
#' przygotuj_raport(k)
#' unlink(k, recursive = TRUE)
przygotuj_raport <- function(katalog = NULL) {
  root <- katalog_kursu(katalog)
  cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
  sprawdz_id(cfg$id, "ID pracy")
  folder <- sciezka_pracy(root, "projekty/ilosciowy")
  if (file.exists(folder)) {
    sprawdz_zapisany_raport(folder, cfg)
    return(invisible(file.path(folder, "raport.Rmd")))
  }
  poprzednie <- stats::setNames(lapply(c("Z09", "Z10"), function(z) {
    f <- sciezka_pracy(root, file.path("zadania", tolower(z)))
    sprawdz_zapisana_prace(f, z, cfg)
    odczytaj_wariant_zadania(f, cfg)
    f
  }), c("Z09", "Z10"))
  x9 <- odczytaj_wariant_zadania(poprzednie$Z09, cfg)
  x10 <- odczytaj_wariant_zadania(poprzednie$Z10, cfg)
  if (!identical(x9$manifest, x10$manifest))
    stop("Z09 i Z10 opisuj\u0105 r\u00f3\u017cne warianty. Zachowano wszystkie odpowiedzi; najpierw wyja\u015bnij rozbie\u017cno\u015b\u0107.", call. = FALSE)
  mapa <- czytaj_yaml(zasob("szablony", "projekt", "przeniesienie.yml"))
  teksty <- lapply(poprzednie, function(f)
    readLines(file.path(f, "zadanie.Rmd"), encoding = "UTF-8", warn = FALSE))
  odpowiedzi <- lapply(teksty, odczytaj_odpowiedzi, pola = sprintf("S%02d", 1:5))
  tekst <- readLines(zasob("szablony", "projekt", "raport.Rmd"), encoding = "UTF-8", warn = FALSE)
  m <- x10$manifest
  wartosci <- c(ID = cfg$id, SCENARIUSZ = m$scenariusz, ROCZNIK = m$rocznik,
                GENERATOR = m$wersja_generatora, HASH_DANYCH = m$hash_danych)
  for (p in names(wartosci)) tekst <- gsub(paste0("{{", p, "}}"), wartosci[[p]], tekst, fixed = TRUE)
  tekst <- lokalne_odnosniki(tekst)
  przeniesione <- character()
  for (w in mapa$odpowiedzi) {
    akapit <- odpowiedzi[[w$zadanie]][[w$odpowiedz]]
    if (any(nzchar(trimws(akapit))) && !any(grepl("UZUPELNIJ", akapit, fixed = TRUE))) {
      tekst <- wstaw_odpowiedz(tekst, w$pole, akapit)
      przeniesione <- c(przeniesione, w$pole)
    }
  }
  dir.create(dirname(folder), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile("raport-", tmpdir = dirname(folder))
  if (!dir.create(tmp)) stop("Nie mo\u017cna przygotowa\u0107 katalogu raportu.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  kopiuj_wariant_zadania(poprzednie$Z10, tmp, cfg)
  pisz_linie(tekst, file.path(tmp, "raport.Rmd"))
  helper <- zasob("szablony", "projekt", "analiza.R")
  if (!file.copy(helper, file.path(tmp, "analiza.R"))) stop("Nie mo\u017cna skopiowa\u0107 silnika raportu.", call. = FALSE)
  zapisz_literature_pracy(tmp)
  meta <- list(format_pracy = "rmd-1", zadanie = "PROJEKT", id_studenta = cfg$id,
    rocznik = cfg$rocznik, wersja_mapy = mapa$version, przeniesione_pola = przeniesione,
    sha256_analiza = digest::digest(file = helper, algo = "sha256"),
    sha256_zrodel = lapply(poprzednie, function(f)
      digest::digest(file = file.path(f, "zadanie.Rmd"), algo = "sha256")))
  pisz_linie(yaml::as.yaml(meta), file.path(tmp, "raport.yml"))
  sprawdz_zapisany_raport(tmp, cfg)
  if (!file.rename(tmp, folder)) stop("Nie mo\u017cna zapisa\u0107 raportu; istniej\u0105ce pliki pozostaj\u0105 bez zmian.", call. = FALSE)
  invisible(file.path(folder, "raport.Rmd"))
}

sprawdz_zapisany_raport <- function(folder, cfg) {
  pliki <- c("raport.Rmd", "analiza.R", "raport.yml", "wariant.yml")
  if (!dir.exists(folder) || !all(file.exists(file.path(folder, pliki))))
    stop("Istniej\u0105cy raport jest niekompletny lub ma starszy format. Zachowano wszystkie pliki; wymaga odtworzenia albo migracji.", call. = FALSE)
  for (p in pliki) sciezka_pracy(folder, p)
  meta <- czytaj_yaml(file.path(folder, "raport.yml"))
  yaml <- naglowek_rmd(readLines(file.path(folder, "raport.Rmd"), encoding = "UTF-8", warn = FALSE))
  m <- odczytaj_wariant_zadania(folder, cfg)$manifest
  parametry <- list(id_studenta = cfg$id, scenariusz = m$scenariusz, rocznik = m$rocznik,
                    wersja_generatora = m$wersja_generatora, hash_danych = m$hash_danych)
  if (!identical(meta$format_pracy, "rmd-1") || !identical(meta$zadanie, "PROJEKT") ||
      !identical(meta$id_studenta, cfg$id) || !identical(meta$rocznik, cfg$rocznik) ||
      !identical(yaml$author, paste0("ID: ", cfg$id)) ||
      !all(vapply(names(parametry), function(p) identical(yaml$params[[p]], parametry[[p]]), logical(1))))
    stop("Raport i dane nie odpowiadaj\u0105 ID, scenariuszowi lub wersji pracy. Nie nadpisuj\u0119 raportu.", call. = FALSE)
  if (!identical(meta$sha256_analiza, digest::digest(file = file.path(folder, "analiza.R"), algo = "sha256")))
    stop("Silnik analiza.R raportu r\u00f3\u017cni si\u0119 od zachowanego zapisu. Popro\u015b o pomoc w odtworzeniu skryptu.", call. = FALSE)
  invisible(TRUE)
}
