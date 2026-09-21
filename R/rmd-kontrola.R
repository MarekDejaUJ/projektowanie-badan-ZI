# Kontrola źródła odbywa się przed uruchomieniem knitr, bez eval kodu/YAML.
pola_pracy <- function(id) {
  if (id == "PROJEKT") sprintf("P%02d", 1:11) else sprintf("S%02d", 1:5)
}

czy_odpowiedz <- function(tekst) {
  x <- paste(tekst, collapse = "\n")
  x <- gsub("(?s)<!--.*?-->", "", x, perl = TRUE)
  !grepl("UZUPELNIJ|\\{\\{[A-Z_]+\\}\\}", x) &&
    grepl("[[:alnum:]]", x) && nzchar(trimws(x))
}

sprawdz_tekst_odpowiedzi <- function(tekst, pole) {
  x <- paste(tekst, collapse = "\n")
  if (!czy_odpowiedz(tekst)) stop("Uzupe\u0142nij w\u0142asn\u0105 odpowied\u017a w polu ", pole, ".", call. = FALSE)
  # Akapity mogą zawierać Markdown i zwykły zapis matematyczny, nie programy
  # ani polecenia wczytujące zasoby podczas składu dokumentu.
  if (grepl("`+[rR]([[:space:]#]|\\{)|^[[:space:]]*(`{3,}|~{3,})|!\\[|\\^\\^", x, perl = TRUE) ||
      any(grepl("^[[:space:]]*(`{3,}|~{3,})", tekst)) ||
      any(grepl("^\\s*(---|[.]{3})\\s*$", tekst, perl = TRUE)))
    stop("Pole ", pole, " ma zawiera\u0107 w\u0142asny tekst poza blokami kodu, bez kodu wykonywalnego i obraz\u00f3w z zewn\u0105trz.", call. = FALSE)
  bez_komentarzy <- gsub("(?s)<!--.*?-->", "", x, perl = TRUE)
  if (grepl("<!--|-->", bez_komentarzy))
    stop("W polu ", pole, " jest niezamkni\u0119ty komentarz. Wpisz odpowied\u017a poza komentarzem.", call. = FALSE)
  bez_linkow <- gsub("<https?://[^>]+>", "", bez_komentarzy)
  if (grepl("<[!/?A-Za-z][^>]*>", bez_linkow))
    stop("W polu ", pole, " u\u017cyj zwyk\u0142ego tekstu i Markdown zamiast znacznik\u00f3w HTML.", call. = FALSE)
  polecenia <- regmatches(x, gregexpr("\\\\[A-Za-z@]+", x, perl = TRUE))[[1L]]
  matematyka <- c("alpha", "beta", "gamma", "delta", "Delta", "theta", "mu", "sigma", "Sigma",
    "rho", "chi", "pi", "lambda", "tau", "varepsilon", "epsilon", "eta", "nu", "phi",
    "bar", "hat", "widehat", "overline", "sqrt", "frac", "dfrac", "tfrac", "sum", "prod",
    "log", "ln", "exp", "min", "max", "left", "right", "text", "mathrm", "mathbf",
    "operatorname", "cdot", "times", "pm", "mp", "le", "leq", "ge", "geq", "neq",
    "approx", "sim", "propto", "in", "infty", "ldots", "dots", "quad", "qquad")
  if (length(setdiff(substring(polecenia, 2L), matematyka)))
    stop("W polu ", pole, " s\u0105 polecenia sk\u0142adu spoza prostego zapisu matematycznego. Zapisz akapit zwyk\u0142ym tekstem lub Markdown.", call. = FALSE)
  invisible(TRUE)
}

podziel_rmd <- function(tekst, pola) {
  granice <- granice_odpowiedzi(tekst, pola)
  # Zastąpienie odpowiedzi zachowuje wszystkie granice dokumentu i kodu.
  bez <- tekst
  for (g in rev(granice[order(vapply(granice, `[`, numeric(1), 1L))])) {
    if (g[2L] > g[1L] + 1L) bez <- bez[-seq.int(g[1L] + 1L, g[2L] - 1L)]
    bez <- append(bez, "ODPOWIEDZ", after = g[1L])
  }
  chunki <- list()
  tresc <- character()
  i <- 1L
  while (i <= length(bez)) {
    linia <- bez[i]
    if (grepl("^```\\{r[ ,}]", linia)) {
      etykieta <- sub("^```\\{r[[:space:]]+([A-Za-z0-9_-]+).*", "\\1", linia)
      if (!grepl("^[A-Za-z0-9_-]+$", etykieta) || !is.null(chunki[[etykieta]]))
        stop("Zachowaj unikalne nazwy gotowych chunk\u00f3w.", call. = FALSE)
      koniec <- which(seq_along(bez) > i & trimws(bez) == "```")[1L]
      if (is.na(koniec)) stop("Niezamkni\u0119ty chunk R.", call. = FALSE)
      kod <- if (koniec == i + 1L) character() else bez[seq.int(i + 1L, koniec - 1L)]
      if (any(grepl("^[[:space:]]*#\\|", kod)))
        stop("Nie dodawaj nowych opcji chunku w komentarzach #|.", call. = FALSE)
      expr <- tryCatch(parse(text = kod, keep.source = FALSE), error = function(e)
        stop("W chunku ", etykieta, " jest b\u0142\u0105d sk\u0142adni. Przywr\u00f3\u0107 gotowy kod i wskazany parametr.", call. = FALSE))
      chunki[[etykieta]] <- list(naglowek = trimws(linia), kod = as.list(expr))
      tresc <- c(tresc, paste0("CHUNK:", etykieta))
      i <- koniec + 1L
    } else {
      tresc <- c(tresc, trimws(linia))
      i <- i + 1L
    }
  }
  list(tresc = tresc, chunki = chunki)
}

# Dopuszczalne wybory występują w konkretnych chunkach. Pozostały AST,
# w tym source(), nazwy funkcji i opcje chunków, musi być zgodny z wzorcem.
reguly_parametrow <- function(id, chunk) {
  klucz <- paste(id, chunk, sep = "/")
  switch(klucz,
    "Z01/odczyt-rekordu" = list(numer = c(3, 4)),
    "Z08/challenge-2" = list(miara = c("Pearson", "Spearman")),
    "Z09/challenge-1" = list(scenariusz = sprintf("S%02d", 1:20)),
    list())
}

zgodny_ast <- function(x, wzorzec, reguly, argument = NULL) {
  if (!is.null(argument) && argument %in% names(reguly) && is.atomic(x) && length(x) == 1L &&
      !is.na(x) && is.atomic(wzorzec) && length(wzorzec) == 1L) {
    return((is.numeric(x) && is.numeric(wzorzec) || typeof(x) == typeof(wzorzec)) && x %in% reguly[[argument]])
  }
  if (is.call(x) && is.call(wzorzec)) {
    if (length(x) != length(wzorzec) || !identical(x[[1L]], wzorzec[[1L]]) ||
        !identical(names(x), names(wzorzec))) return(FALSE)
    if (length(x) == 1L) return(TRUE)
    return(all(vapply(seq.int(2L, length(x)), function(i) {
      nazwa <- names(x)[i]
      if (is.null(nazwa) || is.na(nazwa) || !nzchar(nazwa)) nazwa <- NULL
      zgodny_ast(x[[i]], wzorzec[[i]], reguly, nazwa)
    }, logical(1))))
  }
  if (is.list(x) && is.list(wzorzec)) {
    if (length(x) != length(wzorzec)) return(FALSE)
    return(all(vapply(seq_along(x), function(i) zgodny_ast(x[[i]], wzorzec[[i]], reguly), logical(1))))
  }
  identical(x, wzorzec)
}

sprawdz_kontrakt_rmd <- function(tekst, wzorzec, id) {
  pola <- pola_pracy(id)
  odpowiedzi <- odczytaj_odpowiedzi(tekst, pola)
  for (p in pola) sprawdz_tekst_odpowiedzi(odpowiedzi[[p]], p)
  kandydat <- podziel_rmd(tekst, pola)
  baza <- podziel_rmd(wzorzec, pola)
  if (!identical(kandydat$tresc, baza$tresc))
    stop("Zachowaj nag\u0142\u00f3wek, tre\u015b\u0107 zestawu i znaczniki. W\u0142asne akapity wpisuj wy\u0142\u0105cznie w polach odpowiedzi; nie dopisuj kodu wykonywalnego.", call. = FALSE)
  if (!identical(names(kandydat$chunki), names(baza$chunki)))
    stop("Zachowaj wszystkie gotowe chunki w ich kolejno\u015bci.", call. = FALSE)
  for (nazwa in names(baza$chunki)) {
    k <- kandydat$chunki[[nazwa]]
    b <- baza$chunki[[nazwa]]
    if (!identical(k$naglowek, b$naglowek) || !zgodny_ast(k$kod, b$kod, reguly_parametrow(id, nazwa)))
      stop("Chunk ", nazwa, " r\u00f3\u017cni si\u0119 od gotowego toku analizy. Przywr\u00f3\u0107 kod i opcje; zmieniaj tylko wskazane parametry.", call. = FALSE)
  }
  invisible(TRUE)
}
