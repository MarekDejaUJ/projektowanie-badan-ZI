\name{otworz_material}
\alias{otworz_material}
\title{Dostep do materialu lokalnego lub Pages}
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

\item{handout}{Czy otworzyc handout wykladu. Cwiczenia nie maja handoutow.}

\item{zrodlo}{lokalne albo pages.}

\item{otworz}{Czy uruchomic przegladarke lub otworzyc plik.}
}
\value{
Istniejaca sciezka lub adres Pages, niewidocznie.
}
\description{
Dostep do materialu lokalnego lub Pages
}
\examples{
\dontrun{
otworz_material("C01")
otworz_material("W01", handout = TRUE)
}
}
