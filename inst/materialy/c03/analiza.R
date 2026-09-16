## ----przygotowanie------------------------------------------------------------
przygotowane <- badaniaZI::przygotuj_ankiete(badaniaZI::dane_przykladowe())
dane <- przygotowane$dane
przygotowane$dziennik
c(n_osob = nrow(dane), braki_czasu = sum(is.na(dane$czas_wyszukiwania)))


## ----miary--------------------------------------------------------------------
czas <- dane$czas_wyszukiwania
opis <- data.frame(
  n_osob = nrow(dane), n_wazne = sum(!is.na(czas)), n_brak = sum(is.na(czas)),
  srednia_min = mean(czas, na.rm = TRUE), mediana_min = median(czas, na.rm = TRUE),
  sd_min = stats::sd(czas, na.rm = TRUE), iqr_min = stats::IQR(czas, na.rm = TRUE),
  minimum_min = min(czas, na.rm = TRUE), maksimum_min = max(czas, na.rm = TRUE))
round(opis, 2)


## ----grupy--------------------------------------------------------------------
liczebnosci <- table(dane$grupa, useNA = "no")
grupy <- data.frame(grupa = names(liczebnosci),
  n = as.integer(liczebnosci),
  procent = as.numeric(100 * prop.table(liczebnosci)))
grupy
sum(grupy$n)


## ----wejscie-wykresu----------------------------------------------------------
wykres_dane <- dane[!is.na(dane$czas_wyszukiwania), ]
kolory <- badaniaZI::paleta_zi()
nrow(wykres_dane)


## ----histogram-czas-----------------------------------------------------------
histogram <- ggplot2::ggplot(wykres_dane, ggplot2::aes(x = czas_wyszukiwania)) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    bins = 24,
    fill = kolory["secondary"],
    color = "white",
    alpha = 0.85
  ) +
  ggplot2::geom_density(color = kolory["accent"], linewidth = 1.1) +
  ggplot2::labs(
    title = "Rozkład czasu znalezienia informacji",
    x = "Czas znalezienia informacji (minuty)",
    y = "Gęstość"
  ) + badaniaZI::theme_zi()
histogram


## ----boxplot-czas-------------------------------------------------------------
pudelko <- ggplot2::ggplot(wykres_dane,
  ggplot2::aes(x = grupa, y = czas_wyszukiwania, fill = grupa)) +
  ggplot2::geom_boxplot(alpha = 0.75, show.legend = FALSE, outlier.alpha = 0.65) +
  ggplot2::scale_fill_manual(values = unname(kolory[c("primary", "accent")])) +
  ggplot2::labs(
    title = "Czas znalezienia informacji według grupy: boxplot",
    x = "Grupa użytkowników",
    y = "Czas znalezienia informacji (minuty)"
  ) +
  badaniaZI::theme_zi() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
pudelko
