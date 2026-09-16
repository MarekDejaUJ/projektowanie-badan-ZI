\name{formularz_oceny}
\alias{formularz_oceny}
\title{Pusty formularz oceny według rubryki}
\usage{
formularz_oceny(id = "PROJEKT")
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}
}
\value{
Tabela kryteriów z pustymi punktami i komentarzami. Nie zawiera ocen studentów.
}
\description{
Pusty formularz oceny według rubryki
}
\examples{
formularz_oceny("Z01")
}
