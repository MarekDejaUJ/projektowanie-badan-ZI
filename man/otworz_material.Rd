\name{otworz_material}
\alias{otworz_material}
\title{Dostęp do materiału lokalnego lub Pages}
\usage{
otworz_material(
  id,
  format = c("html", "pdf", "Rmd", "R", "tex"),
  handout = FALSE,
  zrodlo = c("lokalne", "pages"),
  otworz = TRUE
)
}
\arguments{
\item{id}{Identyfikator jednostki, np. C01 lub W01.}

\item{format}{html, pdf, Rmd, R albo tex.}

\item{handout}{Czy otworzyć handout.}

\item{zrodlo}{lokalne albo pages.}

\item{otworz}{Czy uruchomić przeglądarkę lub otworzyć plik.}
}
\value{
Istniejąca ścieżka lub adres Pages, niewidocznie.
}
\description{
Dostęp do materiału lokalnego lub Pages
}
\examples{
\dontrun{ otworz_material("C01") }
}
