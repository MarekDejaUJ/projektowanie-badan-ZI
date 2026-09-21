# Sprawdzamy te same silniki, które student wczytuje w swoim Rmd.
wczytaj_silnik_testowy <- function(id) {
  e <- new.env(parent = globalenv())
  sys.source(system.file("materialy", id, "analiza.R", package = "badaniaZI"), e)
  e
}

test_that("stare eksporty CSV nie zmieniają istniejącej pracy", {
  k <- tempfile("stara praca ")
  dir.create(k)
  on.exit(unlink(k, recursive = TRUE), add = TRUE)
  plik <- file.path(k, "odpowiedzi.md")
  writeLines("Własna interpretacja.", plik, useBytes = TRUE)
  przed <- digest::digest(file = plik, algo = "sha256")
  expect_error(uruchom_analize("Z01", k), "zadanie.Rmd")
  expect_error(uruchom_projekt(k), "raport.Rmd")
  expect_identical(list.files(k), "odpowiedzi.md")
  expect_identical(digest::digest(file = plik, algo = "sha256"), przed)
  expect_identical(system.file("szablony", "zadania", "Z01", "analiza.R",
                              package = "badaniaZI"), "")
})

test_that("C05 rozdziela estymandę, test t, bootstrap i model zerowy", {
  withr::local_seed(731)
  e <- wczytaj_silnik_testowy("c05")
  a <- e$portal_wynik
  ref <- t.test(a$dane, mu = 3)
  expect_equal(a$opis$N, 24)
  expect_equal(a$opis$braki, 1)
  expect_equal(a$opis$roznica, mean(a$dane) - 3)
  expect_equal(a$klasyczny$t, unname(ref$statistic))
  expect_equal(a$klasyczny$p, ref$p.value)
  expect_equal(c(a$przedzialy$dol[1], a$przedzialy$gora[1]), as.numeric(ref$conf.int))
  expect_equal(a$przedzialy$dol[2], a$przedzialy$dol[1] - 3)
  expect_equal(a$przedzialy$gora[2], a$przedzialy$gora[1] - 3)
  expect_equal(c(a$przedzialy$dol[3], a$przedzialy$gora[3]),
               unname(quantile(a$bootstrap, c(.025, .975))))
  expect_equal(a$losowanie$skrajne, sum(abs(a$zerowy) >= abs(a$opis$roznica) - 1e-12))
  expect_equal(a$losowanie$p_MC, (a$losowanie$skrajne + 1) / (a$losowanie$B + 1))
  expect_error(e$analiza_jednej_sredniej(rep(3, 10)))
  expect_error(e$analiza_jednej_sredniej(c(NA_real_, 3)))
  out <- evalq(capture.output(print(test_sredniej(portal_wynik))), e)
  expect_true(any(grepl("Dwustronny test t", out)))
  expect_false(any(grepl("<table|begin\\{table", out)))
})

test_that("C06 zachowuje kierunek efektu, t i obu końców CI", {
  withr::local_seed(732)
  e <- wczytaj_silnik_testowy("c06")
  a <- e$repo_wynik
  x <- subset(e$repozytorium, grupa == "doświadczeni")$czas_min
  y <- subset(e$repozytorium, grupa == "nowi")$czas_min
  ref <- t.test(y, x)
  expect_equal(a$opis$N, c(12, 12))
  expect_equal(a$klasyczny$roznica, mean(y) - mean(x))
  expect_equal(a$klasyczny$t, unname(ref$statistic))
  expect_equal(a$klasyczny$p, ref$p.value)
  expect_equal(c(a$przedzialy$dol[1], a$przedzialy$gora[1]), as.numeric(ref$conf.int))
  expect_equal(a$efekt$Hedges_g, a$efekt$J * a$efekt$d)
  expect_equal(a$losowanie$p_perm, (a$losowanie$skrajne + 1) / (a$losowanie$B + 1))
  odwrocony <- e$porownaj_grupy(e$repozytorium, "czas_min", "nowi", "doświadczeni", B = 99L)
  expect_equal(odwrocony$klasyczny$t, -a$klasyczny$t)
  expect_equal(odwrocony$klasyczny$p, a$klasyczny$p)
  expect_equal(odwrocony$efekt$Hedges_g, -a$efekt$Hedges_g)
  expect_equal(c(odwrocony$przedzialy$dol[1], odwrocony$przedzialy$gora[1]),
               -rev(as.numeric(ref$conf.int)))
})

test_that("C07 rozróżnia liczebności, odsetki, efekt i niepewność", {
  withr::local_seed(733)
  e <- wczytaj_silnik_testowy("c07")
  a <- e$archiwum_wynik
  expect_equal(a$opis$N, c(40, 80))
  expect_equal(a$opis$procent, c(75, 60))
  expect_equal(unname(a$E), matrix(c(14, 26, 28, 52), 2, byrow = TRUE))
  ref <- chisq.test(a$tab, correct = FALSE)
  expect_equal(a$klasyczny$chi2, unname(ref$statistic))
  expect_equal(a$klasyczny$p_chi2, ref$p.value)
  expect_equal(a$efekty$roznica_pp, -15)
  expect_equal(a$efekty$V, sqrt(unname(ref$statistic) / sum(a$tab)))
  expect_equal(c(a$przedzialy$dol[1], a$przedzialy$gora[1]),
               as.numeric(prop.test(c(48, 30), c(80, 40), correct = FALSE)$conf.int))
  expect_equal(a$losowanie$p_MC, (a$losowanie$skrajne + 1) / (a$losowanie$B + 1))
  expect_equal(e$wynik_rzadki$fisher$p_Fisher, fisher.test(e$rzadka)$p.value)
  expect_error(e$analiza_tabeli(a$tab / 3))
  expect_error(e$analiza_tabeli(a$tab * 0))
})

test_that("C08 liczy kompletne pary i odróżnia CI od przedziału predykcji", {
  withr::local_seed(734)
  e <- wczytaj_silnik_testowy("c08")
  a <- e$portal_wynik
  expect_equal(c(a$N_wejscie, a$N_par, a$N_brak), c(30, 28, 2))
  rp <- cor.test(a$pary$x, a$pary$y, method = "pearson")
  rs <- cor.test(a$pary$x, a$pary$y, method = "spearman", exact = FALSE)
  expect_equal(a$klasyczny$wspolczynnik, unname(c(rp$estimate, rs$estimate)))
  expect_equal(a$klasyczny$p, c(rp$p.value, rs$p.value))
  expect_equal(a$klasyczny$df, c(26, NA_real_))
  expect_equal(c(a$przedzialy$dol[1], a$przedzialy$gora[1]), as.numeric(rp$conf.int))
  expect_equal(a$losowanie$p_perm, (a$losowanie$skrajne + 1) / (a$losowanie$B + 1))
  fit <- lm(y ~ x, data = a$pary)
  expect_equal(a$regresja$estymata, unname(coef(fit)))
  expect_equal(a$regresja$CI_dol, unname(confint(fit)[, 1]))
  expect_equal(a$dopasowanie$R2, unname(rp$estimate)^2)
  pred <- e$przewidywanie_czasu(a, 3)$tabele[[1]]
  expect_equal(as.numeric(pred[1, c("czas", "dol", "gora")]),
               as.numeric(predict(fit, data.frame(x = 3), interval = "confidence")))
  expect_equal(as.numeric(pred[2, c("czas", "dol", "gora")]),
               as.numeric(predict(fit, data.frame(x = 3), interval = "prediction")))
  expect_lt(pred$dol[2], pred$dol[1])
  expect_gt(pred$gora[2], pred$gora[1])
  expect_error(e$analizuj_zwiazek(rep(1, 10), 1:10))
  expect_equal(e$korelacja_do_odczytu(a, "Spearman")$tabele[[1]]$df, "—")
})
