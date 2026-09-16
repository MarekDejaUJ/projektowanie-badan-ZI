\name{konfiguracja_kursu}
\alias{konfiguracja_kursu}
\title{Konfiguracja rocznika}
\usage{
konfiguracja_kursu(rocznik = "2026-27", plik = NULL)
}
\arguments{
\item{rocznik}{Rocznik, np. "2026-27".}

\item{plik}{Własny plik YAML; domyślnie konfiguracja z pakietu.}
}
\value{
Lista parametrów rocznika.
}
\description{
Konfiguracja rocznika
}
\examples{
konfiguracja_kursu()$cwiczenia
}
