# Operacje na zapisie pracy nie wykonują kodu Rmd ani nie uruchamiają sieci.
sciezka_pracy <- function(katalog, wzgledna) {
  if (length(wzgledna) != 1L || is.na(wzgledna) ||
      grepl("(^[/\\\\]|:|(^|[/\\\\])\\.\\.([/\\\\]|$))", wzgledna))
    stop("Podaj \u015bcie\u017ck\u0119 wewn\u0105trz w\u0142asnego katalogu pracy.", call. = FALSE)
  root <- normalizePath(katalog, winslash = "/", mustWork = TRUE)
  if (!dir.exists(root)) stop("Brak katalogu pracy.", call. = FALSE)
  czesci <- strsplit(gsub("\\\\", "/", wzgledna), "/", fixed = TRUE)[[1L]]
  cel <- root
  for (czesc in czesci) {
    cel <- file.path(cel, czesc)
    link <- Sys.readlink(cel)
    if (!is.na(link) && nzchar(link))
      stop("Praca nie mo\u017ce prowadzi\u0107 przez dowi\u0105zanie do innego katalogu.", call. = FALSE)
    if (file.exists(cel)) {
      pelna <- normalizePath(cel, winslash = "/", mustWork = TRUE)
      if (!startsWith(tolower(pelna), paste0(tolower(root), "/")))
        stop("Plik znajduje si\u0119 poza katalogiem pracy.", call. = FALSE)
    }
  }
  cel
}

naglowek_rmd <- function(tekst) {
  granice <- which(trimws(tekst) == "---")
  if (length(granice) < 2L || granice[1L] != 1L || granice[2L] < 3L)
    stop("Rmd nie ma poprawnego nag\u0142\u00f3wka YAML.", call. = FALSE)
  yaml::yaml.load(paste(tekst[2L:(granice[2L] - 1L)], collapse = "\n"), eval.expr = FALSE)
}

rmd_z_id <- function(tekst, id) {
  sprawdz_id(id, "id_studenta")
  naglowek_rmd(tekst)
  koniec <- which(trimws(tekst) == "---")[2L]
  autor <- which(grepl("^author:", tekst[seq_len(koniec)]))
  if (length(autor) != 1L) stop("Wzorzec nie ma jednego pola author.", call. = FALSE)
  tekst[autor] <- paste0("author: 'ID: ", id, "'")
  parametry <- which(grepl("^params:", tekst[seq_len(koniec)]))
  if (!length(parametry)) {
    tekst <- append(tekst, c("params:", paste0("  id_studenta: '", id, "'")), after = koniec - 1L)
  } else {
    pole <- which(grepl("^  id_studenta:", tekst[seq_len(koniec)]))
    if (length(pole) != 1L) stop("Wzorzec nie ma jednoznacznego parametru ID.", call. = FALSE)
    tekst[pole] <- paste0("  id_studenta: '", id, "'")
  }
  tekst
}

sprawdz_zapisana_prace <- function(folder, id, cfg) {
  pliki <- c("zadanie.Rmd", "analiza.R", "zadanie.yml")
  if (!dir.exists(folder) || !all(file.exists(file.path(folder, pliki))))
    stop("Istniej\u0105cy katalog nie jest kompletnym zadaniem Rmd. Zachowano wszystkie pliki; popro\u015b o pomoc w odtworzeniu lub migracji pracy.", call. = FALSE)
  for (p in pliki) sciezka_pracy(folder, p)
  meta <- czytaj_yaml(file.path(folder, "zadanie.yml"))
  tekst <- readLines(file.path(folder, "zadanie.Rmd"), encoding = "UTF-8", warn = FALSE)
  yaml <- naglowek_rmd(tekst)
  if (!identical(meta$format_pracy, "rmd-1") || !identical(meta$zadanie, id) ||
      !identical(meta$id_studenta, cfg$id) || !identical(meta$rocznik, cfg$rocznik) ||
      !identical(yaml$author, paste0("ID: ", cfg$id)) ||
      !identical(yaml$params$id_studenta, cfg$id))
    stop("ID, rocznik lub numer istniej\u0105cej pracy nie odpowiada kurs.yml. Pliki pozostaj\u0105 bez zmian.", call. = FALSE)
  if (!identical(meta$sha256_analiza,
                 digest::digest(file = file.path(folder, "analiza.R"), algo = "sha256")))
    stop("Pomocniczy analiza.R r\u00f3\u017cni si\u0119 od zapisanej wersji. Nie nadpisuj\u0119 go; popro\u015b o pomoc w odtworzeniu skryptu.", call. = FALSE)
  invisible(TRUE)
}

# Rozpoznawanie pól nie uruchamia chunków ani wyrażeń YAML.
granice_odpowiedzi <- function(tekst, pola) {
  kod <- rep(FALSE, length(tekst))
  ogrodzenie <- NULL
  for (i in seq_along(tekst)) {
    linia <- trimws(tekst[i])
    if (is.null(ogrodzenie)) {
      trafienie <- regmatches(linia, regexpr("^(`{3,}|~{3,})", linia))
      if (length(trafienie) && nzchar(trafienie)) ogrodzenie <- trafienie
      kod[i] <- !is.null(ogrodzenie)
    } else {
      kod[i] <- TRUE
      znak <- substr(ogrodzenie, 1L, 1L)
      if (grepl(paste0("^", znak, "{", nchar(ogrodzenie), ",}\\s*$"), linia))
        ogrodzenie <- NULL
    }
  }
  if (!is.null(ogrodzenie)) stop("W Rmd pozosta\u0142 niezamkni\u0119ty blok kodu.", call. = FALSE)
  wynik <- lapply(pola, function(pole) {
    a <- which(!kod & trimws(tekst) == paste0("<!-- odpowiedz:", pole, " -->"))
    b <- which(!kod & trimws(tekst) == paste0("<!-- /odpowiedz:", pole, " -->"))
    if (length(a) != 1L || length(b) != 1L || b <= a)
      stop(paste0("Pole ", pole, " musi mie\u0107 jedn\u0105 par\u0119 znacznik\u00f3w poza kodem. Zachowano prac\u0119 bez zmian."), call. = FALSE)
    c(a, b)
  })
  names(wynik) <- pola
  przedzialy <- do.call(rbind, wynik)
  przedzialy <- przedzialy[order(przedzialy[, 1L]), , drop = FALSE]
  if (nrow(przedzialy) > 1L && any(przedzialy[-1L, 1L] <= przedzialy[-nrow(przedzialy), 2L]))
    stop("Pola odpowiedzi nak\u0142adaj\u0105 si\u0119 na siebie. Przywr\u00f3\u0107 ich znaczniki.", call. = FALSE)
  wynik
}

odczytaj_odpowiedzi <- function(tekst, pola) {
  granice <- granice_odpowiedzi(tekst, pola)
  lapply(granice, function(g) if (g[2] == g[1] + 1L) character() else tekst[(g[1] + 1L):(g[2] - 1L)])
}

wstaw_odpowiedz <- function(tekst, pole, odpowiedz) {
  g <- granice_odpowiedzi(tekst, pole)[[1L]]
  c(tekst[seq_len(g[1])], odpowiedz, tekst[g[2]:length(tekst)])
}
