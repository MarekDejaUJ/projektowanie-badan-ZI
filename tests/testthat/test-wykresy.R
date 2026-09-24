zbuduj <- function(p) {
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
}

test_that("wykresy dydaktyczne budują się dla małych przykładów", {
  zbuduj(diagram_etapow(c("Problem", "Pomiar", "Wniosek"), c("Pytanie?", "Wskaźnik", "Zakres"),
                        c("plan", "plan", "analiza")))
  zbuduj(wykres_przeplyw_proby(c("Zaproszeni", "Ukończyli"), c(300, 150)))
  zbuduj(wykres_srednia_mediana(c(2, 3, 3, 20, NA), os = "Czas [min]"))
  zbuduj(wykres_srednia_mediana(c(2, 4, 6, 8, 10, 2, 4, 6, 8, 30), rep(c("A", "B"), each = 5)))
  zbuduj(wykres_przedzialy(c("a", "b"), c(2, 2), c(1.7, -3), c(2.3, 7), odniesienie = 0))
  zbuduj(wykres_roznica_zmian(c(10, 8), c(7, 7), c("A", "B"), "A przy trendzie B"))
  zbuduj(wykres_rowne_srednie(8, c(1, 4), c("SD 1", "SD 4")))
  zbuduj(diagram_piramida_dowodow(c("Opinia", "Opis", "Randomizacja")))
  zbuduj(wykres_punkty_os(c(1, 2, 3, 4, 10)))
  zbuduj(wykres_pudelkowy(c(4.5, 6.5, 8, 35)))
  zbuduj(wykres_histogram(c(1, 2, 3, 4, 10), 2.5, skala = "gestosc"))
  zbuduj(wykres_transformacje(c(2, 4, 4, 8), c("a", "b", "c", "d")))
  zbuduj(wykres_log(data.frame(id = c("u1", "u1"), minuta = c(0, 5), zdarzenie = c("start", "koniec"))))
  d <- data.frame(g = c("a", "a", "b"), x = c(1, NA, 2))
  zbuduj(wykres_braki(d, "x", "g", "Zmienna x"))
  zbuduj(wykres_selekcja(c("Nowi", "Doświadczeni"), c(700, 300), c(20, 80), c(3, 4)))
  zbuduj(wykres_precyzja(0.8, c(25, 50, 100, 200)))
})

test_that("wykresy odrzucają niespójne wejście", {
  expect_error(wykres_przedzialy("a", 5, 6, 7))
  expect_error(wykres_przeplyw_proby("a", c(1, 2)))
  expect_error(wykres_transformacje(c(2, 2), c("a", "b")))
  expect_error(wykres_selekcja("a", 1, 1, 3))
  expect_error(wykres_precyzja(-1, 10))
})

test_that("etykiety liczb używają przecinka i znaku minus", {
  expect_identical(badaniaZI:::liczba_pl(c(1.26, -3), 1L), c("1,3", "−3,0"))
  expect_identical(badaniaZI:::os_pl(c(0.5, 2, -1)), c("0,5", "2", "−1"))
})

test_that("słownik i mapa kursu odczytują zasoby pakietu", {
  s <- slownik_zmiennych()
  expect_equal(nrow(s), 15L)
  expect_identical(slownik_zmiennych("pozycja_3")$odwrocona, TRUE)
  expect_error(slownik_zmiennych("brak_kolumny"), "brak_kolumny")
  m <- mapa_kursu()
  expect_identical(m$kod, c(sprintf("W%02d", 1:5), sprintf("C%02d", 1:10)))
  expect_true(all(nzchar(m$tresc)))
})
