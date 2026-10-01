#' Początek zajęć
#'
#' Przy pierwszym starcie tworzy w bieżącym folderze roboczym przestrzeń
#' moje-badania z ID i imieniem oraz nazwiskiem studenta. Przy kolejnych
#' odnajduje tę przestrzeń, dopisuje plik zadanie.Rmd bieżącego ćwiczenia
#' i udostępnia materiały do czytania w podfolderze materialy. Nie zmienia
#' katalogu roboczego, nie łączy się z siecią i nie nadpisuje odpowiedzi.
#' @param cwiczenie C01--C10.
#' @param id_studenta Pseudonim studenta; wymagany przy pierwszym starcie,
#'   później sprawdzany z kurs.yml.
#' @param student Imię i nazwisko do nazw plików oddania, np. "Anna Kowalska";
#'   wymagane przy pierwszym starcie, później można je poprawić.
#' @param katalog Katalog pracy. NULL używa zapamiętanej lub bieżącej
#'   przestrzeni kursu, a przy pierwszym starcie podkatalogu moje-badania.
#' @param otworz Czy otworzyć roboczy Rmd w bieżącej sesji IDE.
#' @return Lista: plik, cwiczenie, id, student, katalog, materialy i stan
#'   (utworzono albo otwarto), niewidocznie.
#' @export
#' @examples
#' k <- tempfile("start-")
#' rozpocznij_zajecia("C01", id_studenta = "s017", student = "Anna Kowalska",
#'                    katalog = k, otworz = FALSE)
#' unlink(k, recursive = TRUE)
rozpocznij_zajecia <- function(cwiczenie, id_studenta = NULL, student = NULL,
                              katalog = NULL, otworz = TRUE) {
  cwiczenie <- toupper(cwiczenie)
  sprawdz_id(cwiczenie, "cwiczenie", "^C(0[1-9]|10)$")
  if (!is.null(id_studenta)) sprawdz_id(id_studenta, "id_studenta")
  if (!is.null(student)) student <- sprawdz_studenta(student)
  if (is.null(katalog)) {
    katalog <- if (!is.null(sesja_pracy$katalog)) katalog_kursu() else znajdz_katalog_kursu(getwd())
    if (is.null(katalog)) katalog <- file.path(getwd(), "moje-badania")
  }
  if (file.exists(katalog) && !dir.exists(katalog))
    stop("Pod wskazan\u0105 \u015bcie\u017ck\u0105 istnieje plik, nie katalog pracy.", call. = FALSE)
  if (!dir.exists(katalog)) {
    if (is.null(id_studenta) || is.null(student))
      stop("Przy pierwszym starcie podaj swoje ID oraz imi\u0119 i nazwisko, np.\n",
           "  rozpocznij_zajecia(\"", cwiczenie, "\", id_studenta = \"s017\", student = \"Anna Kowalska\")", call. = FALSE)
    utworz_projekt(id_studenta, katalog = katalog, student = student)
    stan <- "utworzono"
  } else {
    katalog <- katalog_kursu(katalog)
    cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
    if (!is.null(id_studenta) && !identical(cfg$id, id_studenta))
      stop("Ta przestrze\u0144 pracy nale\u017cy do ID ", cfg$id, ", a podano ", id_studenta,
           ". Sprawd\u017a swoje ID; istniej\u0105ca praca pozostaje bez zmian.", call. = FALSE)
    if (!is.null(student) && !identical(cfg$student, student)) {
      cfg$student <- student
      pisz_linie(yaml::as.yaml(cfg), file.path(katalog, "kurs.yml"))
      message("Zapisano imi\u0119 i nazwisko do nazw plik\u00f3w oddania: ", student, ".")
    }
    stan <- "otwarto"
  }
  zadanie <- sub("^C", "Z", cwiczenie)
  plik <- przygotuj_zadanie(zadanie, katalog)
  katalog <- zapamietaj_prace(katalog, cwiczenie)
  cfg <- czytaj_yaml(file.path(katalog, "kurs.yml"))
  materialy <- odslon_materialy(katalog, cwiczenie)
  message("Praca ", cwiczenie, ", ID: ", cfg$id,
          if (!is.null(cfg$student)) paste0(" (", cfg$student, ")"), ". Plik: ", plik)
  if (is.null(cfg$student))
    message("Brakuje imienia i nazwiska do nazwy pliku oddania. Dopisz je: rozpocznij_zajecia(\"",
            cwiczenie, "\", student = \"Imi\u0119 Nazwisko\").")
  if (otworz) otworz_plik_pracy(plik)
  message("Materia\u0142y do czytania (HTML, PDF i rubryka): ", materialy, "\n",
    "W RStudio uruchom pierwszy blok R (chunk) tr\u00f3jk\u0105tem przy tym bloku, potem kolejne bloki od g\u00f3ry.\n",
    "W CHALLENGE zast\u0105p znaczniki [UZUPELNIJ_S01]\u2013[UZUPELNIJ_S05] w\u0142asnymi akapitami poza blokami R.\n",
    "Zapisz Rmd (Ctrl+S; na macOS Cmd+S), a w konsoli wpisz: oddaj_zadanie(\"", zadanie,
    "\"). Funkcja przygotuje PDF i wy\u015ble go prowadz\u0105cemu. Poczekaj na potwierdzenie oddania.")
  if (!narzedzia_pdf_gotowe())
    message("Narz\u0119dzia PDF nie s\u0105 jeszcze gotowe. Przed pierwszym oddaniem uruchom raz przygotuj_pdf() (kilka minut).")
  invisible(list(plik = plik, cwiczenie = cwiczenie, id = cfg$id, student = cfg$student,
                 katalog = katalog, materialy = materialy, stan = stan))
}

# Kopie materiałów z pakietu do czytania w panelu Files; odpowiedzi zostają w zadanie.Rmd.
odslon_materialy <- function(katalog, cwiczenie) {
  jednostka <- tolower(cwiczenie)
  zadanie <- sub("^C", "Z", cwiczenie)
  cel <- sciezka_pracy(katalog, file.path("materialy", jednostka))
  dir.create(cel, recursive = TRUE, showWarnings = FALSE)
  zrodla <- c(zasob("materialy", jednostka, "pelne.html"), zasob("materialy", jednostka, "pelne.pdf"),
              zasob("rubryki", paste0(zadanie, ".html")), zasob("rubryki", paste0(zadanie, ".pdf")))
  nazwy <- c(paste0(cwiczenie, c(".html", ".pdf")), paste0("rubryka-", zadanie, c(".html", ".pdf")))
  if (!all(file.copy(zrodla, file.path(cel, nazwy), overwrite = TRUE)))
    stop("Nie mo\u017cna skopiowa\u0107 materia\u0142\u00f3w do czytania. Praca pozostaje bez zmian.", call. = FALSE)
  if (cwiczenie %in% c("C09", "C10")) {
    scen <- sciezka_pracy(katalog, file.path("materialy", "scenariusze"))
    dir.create(scen, recursive = TRUE, showWarnings = FALSE)
    karty <- list.files(zasob("scenariusze"), "^S[0-9]{2}[.](html|pdf)$", full.names = TRUE)
    if (!all(file.copy(karty, scen, overwrite = TRUE)))
      stop("Nie mo\u017cna skopiowa\u0107 kart scenariuszy do czytania.", call. = FALSE)
  }
  normalizePath(cel, winslash = "/", mustWork = TRUE)
}
