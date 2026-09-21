# Render odbywa się w kopii jawnych wejść, w świeżym R bez poświadczeń sesji.
# To nie jest piaskownica dla dowolnego obcego kodu: dopuszczamy tylko sprawdzony
# tok kursu. Kontrole serwerowe wymagają oddzielnego wykonawcy bez sekretów.
pliki_pdf_pracy <- function(id) {
  file.path(folder_pracy(id), c(sub("[.]Rmd$", ".pdf", nazwa_rmd(id)), "pdf.json"))
}

hashe_plikow <- function(katalog, pliki) {
  stats::setNames(vapply(pliki, function(p) {
    f <- sciezka_pracy(katalog, p)
    if (!file.exists(f) || dir.exists(f)) return(NA_character_)
    digest::digest(file = f, algo = "sha256")
  }, character(1)), pliki)
}

sprawdz_zapis_edytora <- function(plik) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable() ||
      !rstudioapi::hasFun("getSourceEditorContext")) return(invisible(TRUE))
  edytor <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(edytor$path) || !nzchar(edytor$path) || !file.exists(edytor$path)) return(invisible(TRUE))
  sciezka <- function(p) normalizePath(p, winslash = "/", mustWork = TRUE)
  if (identical(sciezka(edytor$path), sciezka(plik))) {
    # IDE może dodawać końcową pustą linię do reprezentacji bufora.
    bez_konca <- function(x) sub("\n+$", "", paste(x, collapse = "\n"))
    if (!identical(bez_konca(edytor$contents),
                   bez_konca(readLines(plik, encoding = "UTF-8", warn = FALSE))))
      stop("Zapisz odpowiedzi w Rmd (Ctrl+S, na macOS Cmd+S), a nast\u0119pnie powt\u00f3rz polecenie. Otwarty edytor ma niezapisane zmiany.", call. = FALSE)
  }
  invisible(TRUE)
}

srodowisko_renderu <- function() {
  nazwy <- c("PATH", "SystemRoot", "WINDIR", "COMSPEC", "TEMP", "TMP", "TMPDIR",
             "APPDATA", "LOCALAPPDATA", "PROGRAMDATA", "LANG", "LC_ALL", "LC_CTYPE",
             "RSTUDIO_PANDOC", "FONTCONFIG_PATH", "FONTCONFIG_FILE",
             "OS", "NUMBER_OF_PROCESSORS", "PROCESSOR_ARCHITECTURE")
  env <- Sys.getenv(nazwy, unset = NA_character_)
  env <- env[!is.na(env)]
  # Bez znacznika 'current': nie dziedziczymy GH_TOKEN, kluczy API ani R_TESTS.
  # Stała techniczna data metadanych PDF, nie data wykonania ani oddania pracy.
  # Dzięki niej ponowny skład tych samych wejść nie zmienia PDF tylko przez zegar.
  c(env, R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
    SOURCE_DATE_EPOCH = "946684800", FORCE_SOURCE_DATE = "1", TZ = "UTC")
}

czy_pdf <- function(plik) {
  if (!file.exists(plik) || dir.exists(plik) || is.na(file.info(plik)$size) ||
      file.info(plik)$size < 100) return(FALSE)
  identical(readBin(plik, "raw", n = 5L), charToRaw("%PDF-"))
}

sprawdz_aktualnosc_pdf <- function(kontrola) {
  p <- vapply(pliki_pdf_pracy(kontrola$id), function(x)
    sciezka_pracy(kontrola$katalog, x), character(1))
  if (!czy_pdf(p[1L]) || !file.exists(p[2L]))
    stop("Brak aktualnego PDF. U\u017cyj sprawdz_zadanie() bez uruchom = FALSE albo oddaj_zadanie(); PDF powstanie automatycznie.", call. = FALSE)
  meta <- tryCatch(jsonlite::read_json(p[2L], simplifyVector = FALSE), error = function(e) NULL)
  zgodne <- !is.null(meta) && identical(meta$format, "rmd-pdf-1") &&
    identical(meta$zadanie, kontrola$id) && identical(meta$id_studenta, kontrola$cfg$id) &&
    identical(meta$wejscia, as.list(kontrola$hashe)) &&
    identical(meta$sha256_pdf, digest::digest(file = p[1L], algo = "sha256")) &&
    identical(hashe_plikow(kontrola$katalog, kontrola$pliki), kontrola$hashe)
  if (!zgodne) stop("PDF jest nieaktualny lub zmieniony. Zapisz Rmd i ponownie wykonaj sprawdz_zadanie() albo polecenie oddania.", call. = FALSE)
  invisible(TRUE)
}

uruchom_render_pracy <- function(wejscie, katalog, timeout) {
  katalog <- normalizePath(katalog, winslash = "/", mustWork = TRUE)
  # Starszy rmarkdown wymaga HOME na Linux. Nie przekazujemy domu użytkownika:
  # wyłącznie pusty katalog tego procesu, usuwany po zakończeniu renderowania.
  render_home <- tempfile("dom-renderu-", tmpdir = katalog)
  stopifnot(dir.create(render_home, mode = "0700"))
  render_home <- normalizePath(render_home, winslash = "/", mustWork = TRUE)
  on.exit(unlink(render_home, recursive = TRUE, force = TRUE), add = TRUE)
  processx::run(rscript_bin(), c("--vanilla", zasob("skrypty", "render-prace.R"), wejscie),
    wd = katalog, env = c(srodowisko_renderu(), HOME = render_home), timeout = timeout, error_on_status = FALSE,
    cleanup_tree = TRUE, windows_hide_window = TRUE)
}

zapisz_wynik_pdf <- function(kontrola, roboczy) {
  pliki <- pliki_pdf_pracy(kontrola$id)
  cele <- vapply(pliki, function(p) sciezka_pracy(kontrola$katalog, p), character(1))
  if (any(dir.exists(cele))) stop("Nazwa pliku PDF lub pdf.json jest zaj\u0119ta przez katalog. Zachowaj go i popro\u015b o pomoc.", call. = FALSE)
  kopie <- file.path(roboczy, c("poprzedni.pdf", "poprzedni.json"))
  istnieja <- file.exists(cele)
  for (i in which(istnieja)) if (!file.copy(cele[i], kopie[i]))
    stop("Nie mo\u017cna zabezpieczy\u0107 poprzedniego PDF. Zamknij podgl\u0105d pliku i powt\u00f3rz polecenie.", call. = FALSE)
  gotowe <- FALSE
  on.exit(if (!gotowe) {
    for (i in seq_along(cele)) {
      if (istnieja[i]) file.copy(kopie[i], cele[i], overwrite = TRUE) else
        if (file.exists(cele[i])) unlink(cele[i])
    }
  }, add = TRUE)
  for (i in seq_along(cele)) if (!file.copy(file.path(roboczy, pliki[i]), cele[i], overwrite = TRUE))
    stop("Nie mo\u017cna zapisa\u0107 PDF. Zamknij jego podgl\u0105d i powt\u00f3rz polecenie; zachowano wcze\u015bniejsze pliki.", call. = FALSE)
  sprawdz_aktualnosc_pdf(kontrola)
  gotowe <- TRUE
  invisible(cele[1L])
}

sprawdz_narzedzia_pdf <- function() {
  if (!requireNamespace("knitr", quietly = TRUE) || !requireNamespace("rmarkdown", quietly = TRUE) ||
      !rmarkdown::pandoc_available() || !nzchar(Sys.which("xelatex")))
    stop("Stanowisko nie ma kompletu narz\u0119dzi PDF (knitr, rmarkdown, Pandoc, XeLaTeX). Zachowaj Rmd i zg\u0142o\u015b to prowadz\u0105cemu lub informatykowi; nie instaluj ich podczas \u0107wiczenia.", call. = FALSE)
  invisible(TRUE)
}

renderuj_prace <- function(kontrola, timeout = 240) {
  sprawdz_narzedzia_pdf()
  sprawdz_zapis_edytora(file.path(kontrola$folder, kontrola$rmd))
  tmp <- tempfile("pdf-pracy-")
  if (!dir.create(tmp)) stop("Nie mo\u017cna utworzy\u0107 roboczej kopii do PDF.", call. = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  for (p in kontrola$pliki) {
    cel <- file.path(tmp, p)
    dir.create(dirname(cel), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(sciezka_pracy(kontrola$katalog, p), cel))
      stop("Nie mo\u017cna skopiowa\u0107 wej\u015bcia do PDF. Zachowano oryginalne pliki.", call. = FALSE)
  }
  if (!identical(hashe_plikow(tmp, kontrola$pliki), kontrola$hashe))
    stop("Pliki zmieni\u0142y si\u0119 podczas przygotowania PDF. Zapisz prac\u0119 i powt\u00f3rz polecenie.", call. = FALSE)
  message("Tworz\u0119 aktualny PDF z zapisanego Rmd. Mo\u017ce to potrwa\u0107 chwil\u0119.")
  wynik <- tryCatch(uruchom_render_pracy(file.path(folder_pracy(kontrola$id), kontrola$rmd), tmp, timeout),
                    error = function(e) NULL)
  if (is.null(wynik) || !identical(wynik$status, 0L))
    stop("Nie uda\u0142o si\u0119 utworzy\u0107 PDF w \u015bwie\u017cej sesji R. Zachowano odpowiedzi i poprzedni PDF. Uruchom chunki od pocz\u0105tku i sprawd\u017a zapis akapit\u00f3w; je\u015bli b\u0142\u0105d pozostaje, popro\u015b prowadz\u0105cego o sprawdzenie narz\u0119dzi PDF. Niczego nie wys\u0142ano.", call. = FALSE)
  pdf <- file.path(tmp, pliki_pdf_pracy(kontrola$id)[1L])
  if (!czy_pdf(pdf)) stop("Proces nie utworzy\u0142 poprawnego pliku PDF. Niczego nie wys\u0142ano.", call. = FALSE)
  if (!identical(hashe_plikow(tmp, kontrola$pliki), kontrola$hashe) ||
      !identical(hashe_plikow(kontrola$katalog, kontrola$pliki), kontrola$hashe))
    stop("Wej\u015bcia zmieni\u0142y si\u0119 podczas renderowania. Zachowano poprzedni PDF; zapisz prac\u0119 i powt\u00f3rz polecenie.", call. = FALSE)
  meta <- list(format = "rmd-pdf-1", zadanie = kontrola$id, id_studenta = kontrola$cfg$id,
    wejscia = as.list(kontrola$hashe), sha256_pdf = digest::digest(file = pdf, algo = "sha256"),
    wersje = list(pakiet = as.character(utils::packageVersion("badaniaZI")), R = as.character(getRversion()),
      knitr = as.character(utils::packageVersion("knitr")), rmarkdown = as.character(utils::packageVersion("rmarkdown"))),
    utworzono_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  poprzedni <- file.path(kontrola$katalog, pliki_pdf_pracy(kontrola$id)[2L])
  zapis <- if (file.exists(poprzedni))
    tryCatch(jsonlite::read_json(poprzedni, simplifyVector = FALSE), error = function(e) NULL) else NULL
  pola <- setdiff(names(meta), "utworzono_utc")
  if (identical(zapis[pola], meta[pola]) &&
      isTRUE(tryCatch(sprawdz_aktualnosc_pdf(kontrola), error = function(e) FALSE))) {
    # Odtworzono PDF od początku i porównano bajty. Zachowujemy datę pierwszego
    # identycznego wyniku; sam edytowalny pdf.json nie zastępuje świeżego renderu.
    message("\u015awie\u017co odtworzony PDF jest identyczny. Zachowano dotychczasowe pliki wyniku.")
    return(invisible(file.path(kontrola$folder, sub("[.]Rmd$", ".pdf", kontrola$rmd))))
  }
  pisz_linie(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
    file.path(tmp, pliki_pdf_pracy(kontrola$id)[2L]))
  zapisz_wynik_pdf(kontrola, tmp)
}
