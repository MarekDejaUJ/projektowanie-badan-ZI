\name{status_oddania}
\alias{status_oddania}
\title{Lokalny rejestr oddanych prac}
\usage{
status_oddania(id = NULL, katalog = NULL)
}
\arguments{
\item{id}{Opcjonalnie Z01--Z10 albo PROJEKT; NULL zwraca wszystkie oddania.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}
}
\value{
Ramka danych oddań, od najstarszego.
}
\description{
Każde udane oddanie dopisuje do pliku oddania.csv w przestrzeni pracy
czas, nazwę wysłanego pliku, rozmiar i sumę SHA-256. Rejestr pokazuje, co
wysłano z tej przestrzeni; prowadzący widzi pliki w swoim folderze.
}
\examples{
k <- tempfile("rejestr-")
utworz_projekt("s017", katalog = k, student = "Anna Kowalska")
status_oddania(katalog = k)
unlink(k, recursive = TRUE)
}
