\name{wnioski}
\alias{wnioski}
\alias{wniosek_one_sample}
\alias{wniosek_ind}
\alias{wniosek_paired}
\alias{wniosek_mw}
\alias{wniosek_wilcox}
\alias{wniosek_anova}
\alias{wniosek_kw}
\alias{wniosek_chi2}
\alias{wniosek_fisher}
\alias{wniosek_cor}
\alias{wniosek_reg}
\title{Zapis wyników i warunkowe wnioski statystyczne}
\usage{
wniosek_one_sample(
  test_result,
  alpha = 0.05,
  efekt = NULL,
  efekt_label = "*d*"
)

wniosek_ind(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*d*")

wniosek_paired(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*d*")

wniosek_mw(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*r*")

wniosek_wilcox(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*r*")

wniosek_anova(
  aov_result,
  alpha = 0.05,
  efekt = NULL,
  efekt_label = "$\\\\eta^2$"
)

wniosek_kw(
  kw_result,
  alpha = 0.05,
  efekt = NULL,
  efekt_label = "$\\\\epsilon^2$"
)

wniosek_chi2(
  chi_result,
  alpha = 0.05,
  efekt = NULL,
  efekt_label = "*V* Cramera"
)

wniosek_fisher(fisher_result, alpha = 0.05)

wniosek_cor(cor_result, alpha = 0.05)

wniosek_reg(lm_result, alpha = 0.05)
}
\arguments{
\item{test_result, kw_result, chi_result, fisher_result, cor_result}{Wynik właściwego testu z pakietu stats.}

\item{alpha}{Poziom istotności, większy od 0 i mniejszy od 1.}

\item{efekt}{Opcjonalna, samodzielnie obliczona wielkość efektu.}

\item{efekt_label}{Etykieta podanej wielkości efektu.}

\item{aov_result}{Wynik jednoczynnikowej analizy aov().}

\item{lm_result}{Dopasowany model lm().}
}
\value{
Tekst Markdown z oznaczeniami matematycznymi obsługiwanymi przez R Markdown.
}
\description{
Funkcje formatują wynik; samodzielnie opisz kierunek, znaczenie praktyczne,
założenia i ograniczenia. Brak istotności nie dowodzi braku efektu.
Przedział pochodzi z przekazanego testu, z jego poziomem ufności; alpha
zmienia tylko próg decyzji, nie przelicza przedziału. Kolejność różnicy
odpowiada wywołaniu testu (x minus y). Liczebności niewystępujące w obiekcie
testu, w szczególności N grup w teście Welcha, należy odczytać z danych.
Funkcje nie piszą za studenta odpowiedzi na pytanie badawcze.
}
\examples{
wniosek_ind(stats::t.test(c(2, 3, 5, 6), c(4, 6, 8, 9)))
wniosek_cor(stats::cor.test(1:8, c(2, 1, 5, 3, 7, 6, 4, 8), method = "spearman"))
}
