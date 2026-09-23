\name{diagram_piramida_dowodow}
\alias{diagram_piramida_dowodow}
\title{Uproszczona piramida dowodow}
\usage{
diagram_piramida_dowodow(poziomy, tytul = NULL)
}
\arguments{
\item{poziomy}{Nazwy poziomow od podstawy do wierzcholka.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Poziomy rysowane od podstawy do wierzcholka; kazdy wyzszy poziom jest wezszy.
}
\examples{
diagram_piramida_dowodow(c("Opinia", "Opis przypadku", "Badanie z randomizacja"))
}
