\name{odwroc_pozycje}
\alias{odwroc_pozycje}
\title{Rekodacja pozycji odwróconej}
\usage{
odwroc_pozycje(x, min = 1, max = 5)
}
\arguments{
\item{x}{Numeryczne odpowiedzi od min do max, z dopuszczalnym NA.}

\item{min, max}{Granice skali.}
}
\value{
Wektor odwróconych odpowiedzi; braki pozostają brakami.
}
\description{
Rekodacja pozycji odwróconej
}
\examples{
odwroc_pozycje(c(1, 3, 5, NA))
}
