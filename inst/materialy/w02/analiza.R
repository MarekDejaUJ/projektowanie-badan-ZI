# Dwie małe próby różnią się tylko ostatnim czasem.
serie <- list(A = c(2, 4, 6, 8, 10), B = c(2, 4, 6, 8, 30))
opis_serii <- do.call(rbind, lapply(names(serie), function(nazwa) {
  x <- serie[[nazwa]]
  data.frame(seria = nazwa, N = length(x), srednia = mean(x),
             mediana = median(x), wariancja = var(x), SD = sd(x),
             Q1 = unname(quantile(x, .25)), Q3 = unname(quantile(x, .75)), IQR = IQR(x))
}))
skladniki <- data.frame(czas = serie$A, odchylenie = serie$A - mean(serie$A),
                         kwadrat = (serie$A - mean(serie$A))^2)
punkty <- data.frame(seria = rep(names(serie), each = 5), czas = unlist(serie))
wykres_serii <- ggplot2::ggplot(punkty, ggplot2::aes(x = czas, y = seria)) +
  ggplot2::geom_point(size = 3, colour = '#0072B2') +
  ggplot2::geom_point(data = opis_serii,
                      ggplot2::aes(x = srednia, y = seria),
                      shape = 18, size = 4, colour = '#D55E00') +
  ggplot2::labs(x = 'Czas [min]', y = 'Seria',
                 title = 'Jedna długa próba zmienia średnią',
                 caption = 'Niebieskie koła: obserwacje; pomarańczowe romby: średnie. Dane syntetyczne.') +
  badaniaZI::theme_zi()
oceny <- data.frame(odpowiedz = 1:5, liczba = c(2, 3, 5, 7, 3))
oceny$procent <- 100 * oceny$liczba / sum(oceny$liczba)
oceny$procent_skumulowany <- cumsum(oceny$procent)
