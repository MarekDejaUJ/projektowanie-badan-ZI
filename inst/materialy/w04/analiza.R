# W04: małe syntetyczne demonstracje porównań, osobne względem zbioru S02
# (poza wykresem kwantylowym czasu S02). Kierunek różnic zapisuje każda tabela.

liczba <- function(x, cyfry = 2L) sub("^-", "−", formatC(x, format = "f", digits = cyfry, decimal.mark = ","))
wartosc_p <- function(p) if (p < 0.0001) "< 0,0001" else liczba(p, 4L)
przedzial <- function(a, b, cyfry = 2L) paste0("[", liczba(a, cyfry), "; ", liczba(b, cyfry), "]")

# Dwie niezależne grupy po pięć osób.
nowi <- c(8, 10, 12, 14, 16)
doswiadczeni <- c(4, 6, 8, 10, 12)
test_w <- t.test(nowi, doswiadczeni)
delta <- mean(nowi) - mean(doswiadczeni)
se_w <- sqrt(var(nowi) / 5 + var(doswiadczeni) / 5)
sp <- sqrt(((5 - 1) * var(nowi) + (5 - 1) * var(doswiadczeni)) / 8)
d_cohen <- delta / sp
g_hedges <- (1 - 3 / (4 * 10 - 9)) * d_cohen
opis_grup <- data.frame(Grupa = c("Nowi", "Doświadczeni"), N = c(5, 5),
                        `Czasy [min]` = c("8, 10, 12, 14, 16", "4, 6, 8, 10, 12"),
                        `Średnia [min]` = liczba(c(mean(nowi), mean(doswiadczeni))),
                        `SD [min]` = liczba(c(sd(nowi), sd(doswiadczeni))), check.names = FALSE)
welch <- data.frame(`Wielkość` = c("Różnica nowi − doświadczeni [min]", "SE różnicy [min]", "95% CI różnicy [min]",
                                   "Statystyka t Welcha", "Stopnie swobody ν", "Wartość p (dwustronna)",
                                   "Łączone SD s_p [min]", "d Cohena", "g Hedgesa"),
                    `Wartość` = c(liczba(delta), liczba(se_w), przedzial(test_w$conf.int[1], test_w$conf.int[2]),
                                  liczba(test_w$statistic), liczba(test_w$parameter), wartosc_p(test_w$p.value),
                                  liczba(sp), liczba(d_cohen), liczba(g_hedges)), check.names = FALSE)
wykres_grup <- badaniaZI::wykres_srednia_mediana(c(nowi, doswiadczeni), rep(c("Nowi", "Doświadczeni"), each = 5),
  os = "Czas [min]", tytul = "Czasy w dwóch niezależnych grupach")
wykres_ogonow_welcha <- badaniaZI::wykres_ogony(unname(test_w$statistic), "t", unname(test_w$parameter),
  os = "Statystyka t Welcha przy H0 (ν = 8)", tytul = "Dwustronne p testu Welcha")

# Pełne wyliczenie 252 podziałów dziesięciu czasów na dwie grupy po pięć.
wszystkie <- c(nowi, doswiadczeni)
podzialy <- combn(10, 5)
D_star <- apply(podzialy, 2, function(i) mean(wszystkie[i]) - mean(wszystkie[-i]))
k_perm <- sum(abs(D_star) >= abs(delta) - 1e-12)
wykres_permutacji <- badaniaZI::wykres_rozklad_zerowy(D_star, delta, 0.8, poczatek = min(D_star) - 0.4,
  dokladny = TRUE, os = "Różnica średnich D* [min]", tytul = "Wszystkie podziały etykiet grup")

# Miara theta: pary (nowy, doświadczony).
porownania <- outer(nowi, doswiadczeni, "-")
theta <- (sum(porownania > 0) + sum(porownania == 0) / 2) / 25
test_mw <- suppressWarnings(wilcox.test(nowi, doswiadczeni, exact = FALSE))
wykres_theta <- badaniaZI::wykres_theta(nowi, doswiadczeni, c("Czas nowego użytkownika [min]",
  "Czas doświadczonego użytkownika [min]"), tytul = "25 par nowy–doświadczony")

# Sześć osób przed i po instruktażu; kierunek: po minus przed.
pary <- data.frame(Osoba = letters[1:6], `Przed [min]` = c(6, 8, 10, 12, 14, 16),
                   `Po [min]` = c(5, 6, 9, 9, 12, 12), check.names = FALSE)
pary$`Różnica po − przed [min]` <- pary$`Po [min]` - pary$`Przed [min]`
roznice <- pary$`Różnica po − przed [min]`
test_par <- t.test(pary$`Po [min]`, pary$`Przed [min]`, paired = TRUE)
pary_opis <- list(r = cor(pary$`Przed [min]`, pary$`Po [min]`), s_przed = sd(pary$`Przed [min]`),
                  s_po = sd(pary$`Po [min]`), s_d = sd(roznice),
                  sd_kolumny = sd(c(pary$`Przed [min]`, pary$`Po [min]`)))
wynik_par <- data.frame(`Wielkość` = c("Liczba par", "Średnia zmiana po − przed [min]", "SD różnic [min]",
                                       "95% CI zmiany [min]", "Statystyka t", "Stopnie swobody", "Wartość p (dwustronna)"),
                        `Wartość` = c(nrow(pary), liczba(mean(roznice)), liczba(sd(roznice)),
                                      przedzial(test_par$conf.int[1], test_par$conf.int[2]), liczba(test_par$statistic),
                                      test_par$parameter, wartosc_p(test_par$p.value)), check.names = FALSE)
wykres_par <- badaniaZI::wykres_pary(pary$`Przed [min]`, pary$`Po [min]`, pary$Osoba, os = "Czas [min]",
  tytul = "Każda linia łączy dwa pomiary jednej osoby")
znaki <- as.matrix(expand.grid(rep(list(c(-1, 1)), 6)))
d_star <- as.vector(znaki %*% roznice) / 6
k_znaki <- sum(abs(d_star) >= abs(mean(roznice)) - 1e-12)
wykres_znakow <- badaniaZI::wykres_rozklad_zerowy(d_star, mean(roznice), 1 / 3, poczatek = min(d_star) - 1 / 6,
  dokladny = TRUE, os = "Średnia różnica po zmianie znaków [min]", tytul = "Wszystkie układy znaków sześciu różnic")

# Wykres kwantylowy: różnice par i czas S02 po przygotowaniu.
dane_s02 <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())$dane
czas_s02 <- dane_s02$czas_wyszukiwania[!is.na(dane_s02$czas_wyszukiwania)]
wykres_qq <- badaniaZI::wykres_kwantylowy(list(`Różnice sześciu par` = roznice, `Czas S02` = czas_s02),
  os = "Uporządkowane wartości [min]", tytul = "Wykres kwantylowy względem rozkładu normalnego")

# Trzy wersje katalogu po trzy osoby: jednoczynnikowa ANOVA.
trzy_grupy <- data.frame(wersja = rep(c("A", "B", "C"), each = 3), czas = c(4, 5, 6, 7, 8, 9, 4, 6, 8))
model_aov <- aov(czas ~ wersja, data = trzy_grupy)
aov_tab <- summary(model_aov)[[1]]
eta2 <- aov_tab$`Sum Sq`[1] / sum(aov_tab$`Sum Sq`)
anova_tabela <- data.frame(`Źródło` = c("Między grupami", "Wewnątrz grup", "Całkowita"),
                           SS = liczba(c(aov_tab$`Sum Sq`, sum(aov_tab$`Sum Sq`)), 0L),
                           df = c(aov_tab$Df, sum(aov_tab$Df)),
                           MS = c(liczba(aov_tab$`Mean Sq`, 0L), ""),
                           F = c(liczba(aov_tab$`F value`[1], 2L), "", ""),
                           p = c(wartosc_p(aov_tab$`Pr(>F)`[1]), "", ""), check.names = FALSE)
wykres_anovy <- badaniaZI::wykres_anova(trzy_grupy$czas, trzy_grupy$wersja, os = "Czas [min]",
  tytul = "Trzy wersje katalogu")
wykres_f <- badaniaZI::wykres_ogony(aov_tab$`F value`[1], "F", c(2, 6), os = "Statystyka F przy H0 (df = 2 i 6)",
  tytul = "Prawy ogon rozkładu F")

# Tabela 2×2: sukces zadania w dwóch grupach.
tab <- matrix(c(18, 12, 32, 8), nrow = 2, byrow = TRUE,
              dimnames = list(c("Nowi", "Doświadczeni"), c("Sukces", "Niepowodzenie")))
z_marginesami <- function(m) {
  t <- rbind(m, Suma = colSums(m))
  t <- cbind(t, Suma = rowSums(t))
  data.frame(Grupa = rownames(t), t, check.names = FALSE, row.names = NULL)
}
tab_do_druku <- z_marginesami(tab)
chi <- chisq.test(tab, correct = FALSE)
set.seed(202627)
mc <- chisq.test(tab, simulate.p.value = TRUE, B = 1999)
fisher <- fisher.test(tab)
oczekiwane <- chi$expected
oczekiwane_do_druku <- data.frame(Grupa = rownames(oczekiwane), Sukces = liczba(oczekiwane[, 1]),
                                  Niepowodzenie = liczba(oczekiwane[, 2]), check.names = FALSE)
wklady <- (tab - oczekiwane)^2 / oczekiwane
wklady_do_druku <- data.frame(Grupa = rownames(wklady), Sukces = liczba(wklady[, 1], 3L),
                              Niepowodzenie = liczba(wklady[, 2], 3L), check.names = FALSE)
wykres_proporcji <- badaniaZI::wykres_licznosci_odsetki(rownames(tab), tab[, 1], rowSums(tab),
  tytul = "Liczba i odsetek sukcesów w grupach 30 i 40 osób")
wykres_oe <- badaniaZI::wykres_obserwowane_oczekiwane(tab, tytul = "Liczebności obserwowane i oczekiwane")
wykres_chi2 <- badaniaZI::wykres_chi2(c(1, 2, 4), tytul = "Rozkłady χ² o 1, 2 i 4 stopniach swobody")
wykres_ogona_chi2 <- badaniaZI::wykres_ogony(unname(chi$statistic), "chi2", 1, os = "Statystyka χ² przy H0 (df = 1)",
  tytul = "Prawy ogon rozkładu χ² z 1 stopniem swobody")
p_n <- 18 / 30; p_d <- 32 / 40; p_bar <- 50 / 70
z_prop <- (p_n - p_d) / sqrt(p_bar * (1 - p_bar) * (1 / 30 + 1 / 40))
test_prop <- prop.test(tab[, 1], rowSums(tab), correct = FALSE)
testy_kategorii <- data.frame(
  Procedura = c("χ² bez korekty ciągłości", "Monte Carlo (B = 1999, marginesy stałe)", "Dokładny test Fishera",
                "Test z dwóch proporcji"),
  Statystyka = c(paste0("χ²(1) = ", liczba(chi$statistic)), paste0("χ² = ", liczba(chi$statistic)), "—",
                 paste0("z = ", liczba(z_prop, 3L), "; z² = ", liczba(z_prop^2))),
  `Wartość p` = c(wartosc_p(chi$p.value), liczba(mc$p.value, 3L), liczba(fisher$p.value, 3L),
                  wartosc_p(2 * pnorm(-abs(z_prop)))), check.names = FALSE)
efekty_kategorii <- data.frame(
  `Miara` = c("Różnica proporcji nowi − doświadczeni", "95% CI różnicy (Wald)", "Iloraz proporcji RR",
              "Iloraz szans OR", "V Craméra"),
  `Wartość` = c(liczba(p_n - p_d), przedzial(test_prop$conf.int[1], test_prop$conf.int[2], 3L),
                liczba(p_n / p_d), liczba((18 / 12) / (32 / 8), 3L), liczba(sqrt(unname(chi$statistic) / 70), 3L)),
  check.names = FALSE)

# Tabela rzadka: 1/10 wobec 7/10; rozkład hipergeometryczny przy stałych marginesach.
rzadka <- matrix(c(1, 9, 7, 3), 2, byrow = TRUE, dimnames = dimnames(tab))
rzadka_do_druku <- z_marginesami(rzadka)
fisher_rzadka <- fisher.test(rzadka)
hiper <- data.frame(k = 0:8, p = dhyper(0:8, 10, 10, 8))
ogony_hiper <- hiper$k[hiper$p <= hiper$p[hiper$k == 1] * (1 + 1e-7)]
wykres_hiper <- badaniaZI::wykres_rozklad_dyskretny(hiper$k, hiper$p, os = "Liczba sukcesów nowych n11",
  zaznacz = ogony_hiper, dystrybuanta = FALSE, tytul = "Rozkład hipergeometryczny tabeli rzadkiej")

# Iloraz proporcji i iloraz szans przy stałym ilorazie proporcji 0,75.
p_odniesienia <- seq(0.05, 0.95, by = 0.01)
rr_or <- rbind(data.frame(p = p_odniesienia, wartosc = 0.75, miara = "Iloraz proporcji RR"),
               data.frame(p = p_odniesienia, wartosc = (0.75 * p_odniesienia / (1 - 0.75 * p_odniesienia)) /
                            (p_odniesienia / (1 - p_odniesienia)), miara = "Iloraz szans OR"))
wykres_rr_or <- ggplot2::ggplot(rr_or, ggplot2::aes(x = 100 * p, y = wartosc, linetype = miara)) +
  ggplot2::geom_line(colour = badaniaZI::paleta_zi()[['dark']], linewidth = 0.9) +
  ggplot2::geom_vline(xintercept = 80, linetype = 'dotted') +
  ggplot2::scale_linetype_manual(values = c('Iloraz proporcji RR' = 'solid', 'Iloraz szans OR' = 'dashed'), name = NULL) +
  ggplot2::labs(x = 'Odsetek sukcesów w grupie odniesienia [%]', y = 'Wartość ilorazu',
                title = 'Iloraz proporcji 0,75 a iloraz szans',
                subtitle = 'Linia kropkowana: 80% sukcesów doświadczonych w przykładzie') +
  badaniaZI::theme_zi()
