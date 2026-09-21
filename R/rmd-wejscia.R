folder_pracy <- function(id) {
  if (id == "PROJEKT") "projekty/ilosciowy" else file.path("zadania", tolower(id))
}

nazwa_rmd <- function(id) if (id == "PROJEKT") "raport.Rmd" else "zadanie.Rmd"

lokalne_odnosniki <- function(tekst) {
  gsub("../wspolne/literatura.md", "literatura.md", tekst, fixed = TRUE)
}

zapisz_literature_pracy <- function(folder) {
  if (!file.copy(zasob("materialy", "wspolne", "literatura.md"), file.path(folder, "literatura.md")))
    stop("Nie mo\u017cna zachowa\u0107 bibliografii obok zadania.", call. = FALSE)
  invisible(TRUE)
}

# Lista wejść jest stała, nie zależy od przypadkowych plików w katalogu.
pliki_wejscia_pracy <- function(id) {
  folder <- folder_pracy(id)
  meta <- if (id == "PROJEKT") "raport.yml" else "zadanie.yml"
  p <- c(nazwa_rmd(id), "analiza.R", meta, "literatura.md")
  if (id %in% c("Z09", "Z10", "PROJEKT"))
    p <- c(p, "wariant.yml", file.path("dane", pliki_wariantu()))
  c("kurs.yml", file.path(folder, p))
}

wzorzec_pracy <- function(id, cfg, manifest = NULL) {
  src <- if (id == "PROJEKT") zasob("szablony", "projekt", "raport.Rmd") else
    zasob("materialy", sub("^Z", "c", id), "pelne.Rmd")
  tekst <- readLines(src, encoding = "UTF-8", warn = FALSE)
  if (id == "PROJEKT") {
    if (is.null(manifest)) stop("Brak metadanych zapisanego wariantu raportu.", call. = FALSE)
    wartosci <- c(ID = cfg$id, SCENARIUSZ = manifest$scenariusz, ROCZNIK = manifest$rocznik,
                  GENERATOR = manifest$wersja_generatora, HASH_DANYCH = manifest$hash_danych)
    for (p in names(wartosci)) tekst <- gsub(paste0("{{", p, "}}"), wartosci[[p]], tekst, fixed = TRUE)
  } else tekst <- rmd_z_id(tekst, cfg$id)
  lokalne_odnosniki(tekst)
}

kontroluj_wejscia_rmd <- function(id, katalog) {
  id <- toupper(id)
  sprawdz_id(id, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  root <- katalog_kursu(katalog)
  wejscia <- pliki_wejscia_pracy(id)
  sciezki <- vapply(wejscia, function(p) sciezka_pracy(root, p), character(1))
  brak <- wejscia[!file.exists(sciezki) | dir.exists(sciezki)]
  if (length(brak)) stop("Brakuje plik\u00f3w w\u0142asnej pracy: ", paste(brak, collapse = ", "),
                         ". Odtw\u00f3rz komplet, nie tw\u00f3rz pustych zamiennik\u00f3w.", call. = FALSE)
  tekstowe <- sciezki[grepl("[.](Rmd|R|md|yml|json|csv)$", sciezki)]
  for (p in tekstowe) {
    if (file.info(p)$size > 2e6) stop("Plik jest nietypowo du\u017cy dla tego zestawu: ", basename(p), ". Sprawd\u017a, czy nie wklejono danych zamiast akapitu.", call. = FALSE)
    t <- readLines(p, encoding = "UTF-8", warn = FALSE)
    if (any(grepl("(gh[pousr]_[A-Za-z0-9_]{20,}|github_pat_[A-Za-z0-9_]{20,})", t)))
      stop("W plikach pracy znaleziono zapis przypominaj\u0105cy token GitHub. Usu\u0144 go i uniewa\u017cnij ujawniony token; niczego nie wysy\u0142aj.", call. = FALSE)
  }
  cfg <- czytaj_yaml(file.path(root, "kurs.yml"))
  sprawdz_id(cfg$id, "ID pracy")
  if (cfg$id == "demo001" || grepl("^DEMO_C", cfg$id))
    stop("Wariant demonstracyjny nie jest w\u0142asn\u0105 prac\u0105. Odtw\u00f3rz zadanie ze swoim ID.", call. = FALSE)
  konfiguracja_kursu(cfg$rocznik)
  folder <- sciezka_pracy(root, folder_pracy(id))
  if (id == "PROJEKT") sprawdz_zapisany_raport(folder, cfg) else sprawdz_zapisana_prace(folder, id, cfg)
  meta <- czytaj_yaml(file.path(folder, if (id == "PROJEKT") "raport.yml" else "zadanie.yml"))
  helper <- if (id == "PROJEKT") zasob("szablony", "projekt", "analiza.R") else
    zasob("materialy", sub("^Z", "c", id), "analiza.R")
  if (!identical(digest::digest(file = helper, algo = "sha256"), meta$sha256_analiza))
    stop("Praca korzysta z innej wersji gotowego skryptu ni\u017c zainstalowany pakiet. Zachowaj pliki; popro\u015b o przywr\u00f3cenie zgodnej wersji pakietu.", call. = FALSE)
  if (!identical(digest::digest(file = file.path(folder, "literatura.md"), algo = "sha256"),
                 digest::digest(file = zasob("materialy", "wspolne", "literatura.md"), algo = "sha256")))
    stop("Pomocnicza bibliografia r\u00f3\u017cni si\u0119 od wersji pakietu. W\u0142asne \u017ar\u00f3d\u0142a zapisuj w odpowiedzi lub P11 raportu.", call. = FALSE)
  wariant <- if (id %in% c("Z09", "Z10", "PROJEKT")) odczytaj_wariant_zadania(folder, cfg) else NULL
  tekst <- readLines(file.path(folder, nazwa_rmd(id)), encoding = "UTF-8", warn = FALSE)
  baza <- wzorzec_pracy(id, cfg, wariant$manifest)
  sprawdz_kontrakt_rmd(tekst, baza, id)
  if (id == "Z09") {
    wybrane <- podziel_rmd(tekst, pola_pracy(id))$chunki[["challenge-1"]]$kod[[1L]][[3L]][["scenariusz"]]
    if (!identical(wybrane, wariant$manifest$scenariusz))
      stop("Scenariusz w chunku C09 r\u00f3\u017cni si\u0119 od zachowanego wariantu. Przywr\u00f3\u0107 kod zgodny z zapisanymi danymi.", call. = FALSE)
  }
  list(id = id, katalog = root, folder = folder, rmd = nazwa_rmd(id), cfg = cfg,
       pliki = wejscia, hashe = stats::setNames(vapply(sciezki, function(p)
         digest::digest(file = p, algo = "sha256"), character(1)), wejscia))
}
