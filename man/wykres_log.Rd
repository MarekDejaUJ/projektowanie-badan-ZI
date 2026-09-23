\name{wykres_log}
\alias{wykres_log}
\title{Os czasu zdarzen w logu}
\usage{
wykres_log(log, os = "Minuta sesji", tytul = NULL)
}
\arguments{
\item{log}{Ramka z kolumnami id, minuta i zdarzenie.}

\item{os}{Podpis osi czasu.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazdy wiersz to jedna osoba lub sesja; odcinek biegnie od pierwszego do
ostatniego zdarzenia, a punkty z podpisami oznaczaja zdarzenia.
}
\examples{
log <- data.frame(id = c("u01", "u01"), minuta = c(0, 6.5), zdarzenie = c("start", "koniec"))
p <- wykres_log(log)
}
