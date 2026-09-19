#' Przygotowanie lokalnego zadania
#' @param id Z01--Z10.
#' @param katalog Katalog własnego projektu.
#' @return Ścieżka odpowiedzi Markdown, niewidocznie. Istniejąca praca pozostaje zachowana.
#' @export
#' @examples
#' k <- tempfile("zadania-")
#' utworz_projekt("s017", katalog = k)
#' przygotuj_zadanie("Z01", k)
#' unlink(k, recursive = TRUE)
przygotuj_zadanie <- function(id, katalog = ".") {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^Z(0[1-9]|10)$")
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  sprawdz_id(cfg$id, "id")
  folder <- file.path(katalog, "zadania", tolower(id))
  if (dir.exists(folder)) return(invisible(file.path(folder, "odpowiedzi.md")))
  dir.create(file.path(folder, "wyniki"), recursive = TRUE)
  r <- rubryka(id)
  tekst <- c(paste0("# ", id, " \u2014 odpowiedzi"), "", paste("ID:", cfg$id), paste("Rocznik:", cfg$rocznik), "",
             "Wykonaj polecenie w \u0107wiczeniu. Zast\u0105p ka\u017cde `[UZUPELNIJ]` w\u0142asnym opisem i liczbami.",
             "Odpowied\u017a: 250\u2013450 s\u0142\u00f3w bez kodu i tabel. Wyniki zapisuj w podkatalogu `wyniki`.")
  for (k in r$criteria) tekst <- c(tekst, "", paste0("## ", k$id, " \u2014 ", k$max_points, " pkt"), "", k$evidence, "", "[UZUPELNIJ]")
  writeLines(enc2utf8(tekst), file.path(folder, "odpowiedzi.md"), useBytes = TRUE)
  skrypt <- c(paste0("# ", id, " \u2014 ", cfg$id),
    "# Otw\u00f3rz g\u0142\u00f3wny projekt RStudio. Zapisz tu kod zadania wykonywany od pocz\u0105tku.",
    paste0("dir.create(\"zadania/", tolower(id), "/wyniki\", recursive = TRUE, showWarnings = FALSE)"),
    "stop(\"Uzupe\u0142nij analiz\u0119 zgodnie z poleceniem \u0107wiczenia.\")")
  writeLines(skrypt, file.path(folder, "analiza.R"))
  invisible(file.path(folder, "odpowiedzi.md"))
}

#' Lokalna kontrola zadania lub projektu
#'
#' Kontrola nie przyznaje punktów za interpretację. Opcjonalne wykonanie skryptu
#' odbywa się w kopii pracy i świeżym procesie R; wyniki w oryginale muszą być zapisane.
#' Wykonuj w ten sposób własny kod. Kontrolę prac oddanych wykonuje CI bez poświadczeń kursu.
#' @param id Z01--Z10 albo PROJEKT.
#' @param katalog Katalog projektu.
#' @param uruchom Czy wykonać zapisany skrypt (domyślnie tak).
#' @return Lista: ok, tabela kontroli i jawna lista plików.
#' @export
#' @examples
#' k <- tempfile("kontrola-")
#' utworz_projekt("s017", katalog = k)
#' sprawdz_zadanie("PROJEKT", k, uruchom = FALSE)$ok
#' unlink(k, recursive = TRUE)
sprawdz_zadanie <- function(id, katalog = ".", uruchom = TRUE) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  kontrole <- data.frame(element = character(), ok = logical(), opis = character())
  dodaj <- function(element, ok, opis) {
    kontrole <<- rbind(kontrole, data.frame(element = element, ok = isTRUE(ok), opis = opis))
  }
  cfg <- tryCatch(czytaj_yaml(file.path(katalog, "kurs.yml")), error = function(e) NULL)
  dodaj("konfiguracja", !is.null(cfg$id) && !is.null(cfg$scenariusz), "kurs.yml musi zawiera\u0107 w\u0142asne ID i scenariusz")
  folder <- if (id == "PROJEKT") "projekty/ilosciowy" else paste0("zadania/", tolower(id))
  md <- if (id == "PROJEKT") c("raport.md", "kwestionariusz.md") else "odpowiedzi.md"
  katalog_zadan <- czytaj_yaml(zasob("zadania", "katalog.yml"))$zadania
  artefakty <- unlist(katalog_zadan[[id]]$wyniki, use.names = FALSE)
  wymagane <- c("kurs.yml", paste0(folder, "/", c(md, "analiza.R")),
               "dane/surowe.csv", "dane/slownik.csv", "dane/manifest.json",
               if (length(artefakty)) paste0(folder, "/wyniki/", artefakty))
  for (p in wymagane) dodaj(p, file.exists(file.path(katalog, p)), "Wymagany zapisany plik")
  for (p in paste0(folder, "/", md)) if (file.exists(file.path(katalog, p))) {
    tekst <- readLines(file.path(katalog, p), encoding = "UTF-8", warn = FALSE)
    dodaj(paste0(p, ": uzupe\u0142nienie"), !any(grepl("UZUPELNIJ|\\{\\{[A-Z]+\\}\\}", tekst)) && sum(nchar(tekst)) > 250,
          "Zast\u0105p wszystkie znaczniki w\u0142asnymi odpowiedziami")
    naglowki <- if (id == "PROJEKT" && basename(p) == "raport.md") paste0("## ", 1:9, ".") else
      if (id != "PROJEKT") paste0("## ", id, ".", 1:4) else character()
    dodaj(paste0(p, ": sekcje"), all(vapply(naglowki, function(h) any(startsWith(tekst, h)), logical(1))), "Zachowaj sekcje wymagane w szablonie")
    dodaj(paste0(p, ": ID"), !is.null(cfg$id) && any(grepl(paste0("ID: ", cfg$id), tekst, fixed = TRUE)), "Wpisz w\u0142asne ID")
  }
  manifest <- tryCatch(jsonlite::read_json(file.path(katalog, "dane", "manifest.json")), error = function(e) NULL)
  if (!is.null(cfg) && !is.null(manifest)) {
    dodaj("manifest ID", identical(manifest$id, cfg$id) && identical(manifest$scenariusz, cfg$scenariusz) && identical(manifest$rocznik, cfg$rocznik), "Dane i konfiguracja musz\u0105 nale\u017ce\u0107 do tego samego wariantu")
    oryginal <- tryCatch(generuj_dane(cfg$id, cfg$scenariusz, cfg$rocznik, manifest$n), error = function(e) NULL)
    dodaj("wariant generatora", !is.null(oryginal) && identical(oryginal$manifest$hash_danych, manifest$hash_danych) &&
            identical(oryginal$manifest$wersja_generatora, cfg$wersja_generatora), "U\u017cyj przypi\u0119tej wersji generatora i w\u0142asnego ID")
    hash <- if (file.exists(file.path(katalog, "dane", "surowe.csv"))) digest::digest(file = file.path(katalog, "dane", "surowe.csv"), algo = "sha256") else NULL
    dodaj("surowe CSV", identical(hash, manifest$sha256_csv), "Surowych danych nie zmienia si\u0119 po wygenerowaniu")
    tabela <- tryCatch(utils::read.csv(file.path(katalog, "dane", "surowe.csv"), encoding = "UTF-8", stringsAsFactors = FALSE), error = function(e) NULL)
    dodaj("warto\u015bci danych", !is.null(tabela) && identical(hash_tabeli(tabela), manifest$hash_danych), "Manifest musi odpowiada\u0107 rzeczywistym warto\u015bciom surowego pliku")
  }
  pliki <- tryCatch(pliki_oddania(id, katalog), error = function(e) character())
  dodaj("lista plik\u00f3w", length(pliki) >= length(wymagane), "Dopuszczalne pliki i \u015bcie\u017cki wewn\u0105trz projektu")
  for (p in pliki[grepl("[.](R|md|csv|json|yml)$", pliki)]) {
    tekst <- readLines(file.path(katalog, p), encoding = "UTF-8", warn = FALSE)
    dodaj(paste0(p, ": po\u015bwiadczenia"), !any(grepl("(gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,})", tekst)), "Pliki nie mog\u0105 zawiera\u0107 token\u00f3w GitHub")
  }
  skrypt <- file.path(katalog, folder, "analiza.R")
  if (file.exists(skrypt)) {
    syntax <- tryCatch({ parse(skrypt, encoding = "UTF-8"); TRUE }, error = function(e) FALSE)
    dodaj("sk\u0142adnia R", syntax, "Popraw sk\u0142adni\u0119 zapisanego skryptu")
    if (uruchom && syntax && all(kontrole$ok)) {
      tmp <- tempfile("kontrola-pracy-")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      # Wyniki muszą powstać ponownie, więc nie trafiają do wejścia świeżej sesji.
      for (p in pliki[!startsWith(pliki, paste0(folder, "/wyniki/"))]) {
        dir.create(dirname(file.path(tmp, p)), recursive = TRUE, showWarnings = FALSE)
        file.copy(file.path(katalog, p), file.path(tmp, p))
      }
      r <- tryCatch(processx::run(rscript_bin(),
        c("--vanilla", paste0(folder, "/analiza.R")), wd = tmp,
        env = c("current", GH_TOKEN = "", GITHUB_TOKEN = "", GITHUB_PAT = "",
                GH_ENTERPRISE_TOKEN = "", GITHUB_ENTERPRISE_TOKEN = "",
                GH_CONFIG_DIR = file.path(tmp, ".logowanie"), R_TESTS = ""),
        timeout = 120000, error_on_status = FALSE), error = function(e) NULL)
      dodaj("wykonanie R", !is.null(r) && r$status == 0L, "Skrypt musi wykona\u0107 si\u0119 od pocz\u0105tku w \u015bwie\u017cej sesji; przy b\u0142\u0119dzie uruchom go w RStudio i popraw komunikat")
      for (p in pliki[startsWith(pliki, paste0(folder, "/wyniki/"))]) {
        nowy <- file.path(tmp, p)
        istnieje <- file.exists(nowy) && !is.na(file.info(nowy)$size) && file.info(nowy)$size > 0
        dodaj(paste0(p, ": odtworzenie"), istnieje, "Skrypt musi ponownie utworzy\u0107 zapisany wynik z surowych danych")
        if (istnieje && grepl("[.]csv$", p)) {
          czytaj <- function(f) utils::read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
          zgodne <- tryCatch(isTRUE(all.equal(czytaj(file.path(katalog, p)), czytaj(nowy),
                                              tolerance = 1e-10)), error = function(e) FALSE)
          dodaj(paste0(p, ": zgodno\u015b\u0107 CSV"), zgodne, "Odtworzona tabela musi zgadza\u0107 si\u0119 z zapisan\u0105; zapisz aktualne wyniki przed oddaniem")
        }
      }
    }
  }
  list(ok = all(kontrole$ok), kontrole = kontrole, pliki = pliki)
}

rscript_bin <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

pliki_oddania <- function(id, katalog) {
  folder <- if (id == "PROJEKT") "projekty/ilosciowy" else paste0("zadania/", tolower(id))
  wzorzec <- "[.](R|md|csv|png|json|rds|pdf)$"
  lokalne <- list.files(file.path(katalog, folder), recursive = TRUE, all.files = TRUE)
  if (any(!grepl(wzorzec, lokalne)) || any(grepl("(^|/)\\.", lokalne))) stop("Nieobs\u0142ugiwany plik oddania.", call. = FALSE)
  pliki <- c("kurs.yml", paste0(folder, "/", lokalne),
             paste0("dane/", c("surowe.csv", "slownik.csv", "manifest.json")))
  root <- normalizePath(katalog, winslash = "/", mustWork = TRUE)
  for (p in pliki) {
    full <- file.path(katalog, p)
    if (!file.exists(full)) next
    resolved <- normalizePath(full, winslash = "/", mustWork = TRUE)
    if (!startsWith(tolower(resolved), paste0(tolower(root), "/"))) stop("Plik poza projektem.", call. = FALSE)
    link <- Sys.readlink(full)
    if (!is.na(link) && nzchar(link)) stop("Dowi\u0105zania nie s\u0105 plikami oddania.", call. = FALSE)
  }
  unique(pliki[file.exists(file.path(katalog, pliki))])
}
