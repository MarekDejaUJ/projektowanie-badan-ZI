#' Niezależne odtworzenie pobranego oddania
#'
#' Narzędzie prowadzącego, nie polecenie studenta. Wymaga wcześniej zbudowanego
#' i zaufanego obrazu Linux z pakietem kursu oraz Docker. Uruchamia kontrolę
#' bez sieci, tokenów, uprawnień root i zapisu do systemu hosta. Do kontenera
#' trafia tylko odrębna kopia pobranego oddania, zamontowana tylko do odczytu.
#' Nie instaluje Docker, nie pobiera obrazów i nie zastępuje oceny interpretacji.
#' Wynik i odtworzony PDF zapisuje w nowym prywatnym podkatalogu; nie wysyła
#' oceny ani nie modyfikuje oryginalnego oddania na GitHub.
#' @param katalog Katalog utworzony przez pobierz_oddanie().
#' @param obraz Pełny lokalny identyfikator obrazu `sha256:...`, wskazany
#'   przez informatyka. Nie nazwa zmiennego tagu.
#' @param timeout Limit kontroli w sekundach, 30--600.
#' @return Lista: ok, katalog wyniku i raport kontroli. PDF kontrolny może
#'   różnić się bajtami od PDF studenta z innego systemu; nie zastępuje oryginału.
#' @export
#' @examples
#' \dontrun{
#' sprawdz_oddanie("prywatna-kontrola-z01", obraz = "sha256:IDENTYFIKATOR_OBRAZU")
#' }
sprawdz_oddanie <- function(katalog, obraz, timeout = 360) {
  sprawdz_id(obraz, "obraz", "^sha256:[0-9a-f]{64}$")
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout < 30 || timeout > 600)
    stop("Limit kontroli musi wynosi\u0107 od 30 do 600 sekund.", call. = FALSE)
  if (!nzchar(Sys.which("docker")))
    stop("Kontrola prowadz\u0105cego wymaga Docker i zaufanego obrazu przygotowanego przez informatyka. Nie wykonano kodu oddania.", call. = FALSE)
  katalog <- normalizePath(katalog, winslash = "/", mustWork = TRUE)
  d <- czytaj_odbior_kontrolny(katalog)
  tmp <- tempfile("wejscie-kontroli-")
  stopifnot(dir.create(tmp, mode = "0700"))
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  for (p in c("odbior.json", file.path("wejscie", names(d$pliki)))) {
    src <- sciezka_pracy(katalog, p)
    cel <- file.path(tmp, p)
    dir.create(dirname(cel), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(src, cel)) stop("Nie mo\u017cna przygotowa\u0107 odr\u0119bnej kopii kontroli.", call. = FALSE)
  }
  czytaj_odbior_kontrolny(tmp)
  Sys.chmod(list.files(tmp, recursive = TRUE, full.names = TRUE), "0444")
  Sys.chmod(list.dirs(tmp, recursive = TRUE, full.names = TRUE), "0555")
  wynik <- uruchom_kontrole_kontener(tmp, obraz, timeout)
  odpowiedz <- tryCatch(jsonlite::fromJSON(wynik$stdout, simplifyVector = FALSE), error = function(e) NULL)
  if (wynik$status != 0L || is.null(odpowiedz) ||
      !identical(odpowiedz$format, "kontrola-oddania-1") ||
      !identical(odpowiedz$sha, d$sha) || !identical(odpowiedz$zadanie, d$zadanie) ||
      !identical(odpowiedz$id_studenta, d$id_studenta) ||
      !is.logical(odpowiedz$ok) || length(odpowiedz$ok) != 1L || is.na(odpowiedz$ok))
    stop("Kontener nie zwr\u00f3ci\u0142 poprawnego wyniku. Zachowano pobrane oddanie; sprawd\u017a wersj\u0119 obrazu i narz\u0119dzia kontroli.", call. = FALSE)
  pdf <- NULL
  if (isTRUE(odpowiedz$ok)) {
    pdf <- tryCatch(jsonlite::base64_dec(odpowiedz$pdf_base64), error = function(e) NULL)
    if (is.null(pdf) || length(pdf) < 100L || length(pdf) > 10e6 ||
        !identical(utils::head(pdf, 5L), charToRaw("%PDF-")) ||
        !identical(digest::digest(pdf, algo = "sha256", serialize = FALSE), odpowiedz$sha256_pdf))
      stop("Niepoprawny PDF kontrolny. Nie zapisano wyniku.", call. = FALSE)
  }
  odpowiedz$pdf_base64 <- NULL
  odpowiedz$obraz <- obraz
  out <- tempfile("kontrola-", tmpdir = katalog)
  stopifnot(dir.create(out, mode = "0700"))
  if (!is.null(pdf)) writeBin(pdf, file.path(out, "kontrola.pdf"))
  pisz_linie(jsonlite::toJSON(odpowiedz, auto_unbox = TRUE, pretty = TRUE), file.path(out, "kontrola.json"))
  list(ok = odpowiedz$ok, katalog = normalizePath(out, winslash = "/"), raport = odpowiedz)
}

czytaj_odbior_kontrolny <- function(katalog) {
  p <- sciezka_pracy(katalog, "odbior.json")
  if (!file.exists(p) || file.info(p)$size > 100000)
    stop("Brak poprawnej metryki pobrania oddania.", call. = FALSE)
  d <- jsonlite::read_json(p, simplifyVector = FALSE)
  if (!identical(d$format, "odbior-kontrolny-1")) stop("Nieznany format odbioru.", call. = FALSE)
  sprawdz_id(d$sha, "SHA oddania", "^[0-9a-f]{40}$")
  sprawdz_id(d$zadanie, "zadanie", "^(Z(0[1-9]|10)|PROJEKT)$")
  sprawdz_id(d$id_studenta, "ID pracy")
  sprawdz_id(d$repo, "repo", "^[A-Za-z0-9][A-Za-z0-9-]*/[A-Za-z0-9_.-]+$")
  pliki <- c(pliki_wejscia_pracy(d$zadanie), pliki_pdf_pracy(d$zadanie))
  if (!identical(names(d$pliki), pliki) ||
      !identical(d$pliki, as.list(hashe_plikow(file.path(katalog, "wejscie"), pliki))))
    stop("Pobrane pliki zmieni\u0142y si\u0119 albo nie stanowi\u0105 jawnego kompletu oddania.", call. = FALSE)
  d
}

argumenty_kontenera <- function(wejscie, obraz) {
  wejscie <- normalizePath(wejscie, winslash = "/", mustWork = TRUE)
  if (grepl("[,\r\n]", wejscie)) stop("Katalog kontroli nie mo\u017ce zawiera\u0107 przecinka ani nowej linii.", call. = FALSE)
  c("create", "--pull=never", "--network=none", "--read-only", "--cap-drop=ALL",
    "--security-opt=no-new-privileges", "--user=65534:65534",
    "--pids-limit=128", "--memory=2g", "--cpus=2", "--log-driver=none",
    "--tmpfs=/tmp:rw,nosuid,nodev,size=512m,mode=1777",
    "--tmpfs=/work:rw,nosuid,nodev,size=512m,mode=1777", "--workdir=/work",
    "--mount", paste0("type=bind,source=", wejscie, ",target=/input,readonly"),
    "--entrypoint=/usr/bin/Rscript", obraz, "--vanilla", "/opt/badaniazi/kontrola.R")
}

uruchom_kontrole_kontener <- function(wejscie, obraz, timeout) {
  x <- processx::run("docker", argumenty_kontenera(wejscie, obraz),
    timeout = 30, error_on_status = FALSE, windows_hide_window = TRUE)
  cid <- trimws(x$stdout)
  if (x$status != 0L || !grepl("^[0-9a-f]{64}$", cid))
    stop("Nie utworzono kontenera. Sprawd\u017a lokalny obraz Linux i dost\u0119p do Docker; nie u\u017cywam trybu bez izolacji.", call. = FALSE)
  # Usuwamy wyłącznie identyfikator zwrócony przez własne udane create.
  on.exit(try(processx::run("docker", c("rm", "--force", cid), timeout = 30,
    error_on_status = FALSE, windows_hide_window = TRUE), silent = TRUE), add = TRUE)
  p <- processx::process$new("docker", c("start", "--attach", cid),
    stdout = "|", stderr = "|", cleanup_tree = TRUE, windows_hide_window = TRUE)
  on.exit(if (p$is_alive()) p$kill(), add = TRUE)
  start <- proc.time()[["elapsed"]]
  chunks <- character()
  size <- 0
  repeat {
    p$poll_io(100)
    out <- p$read_output(65536L)
    err <- p$read_error(65536L)
    size <- size + nchar(out, type = "bytes") + nchar(err, type = "bytes")
    if (size > 20e6 || proc.time()[["elapsed"]] - start > timeout)
      stop("Kontrola przekroczy\u0142a limit czasu lub rozmiaru odpowiedzi; zatrzymano w\u0142asny kontener.", call. = FALSE)
    if (nzchar(out)) chunks <- c(chunks, out)
    if (!p$is_alive() && !nzchar(out) && !nzchar(err)) break
  }
  list(status = p$get_exit_status(), stdout = paste0(chunks, collapse = ""))
}
