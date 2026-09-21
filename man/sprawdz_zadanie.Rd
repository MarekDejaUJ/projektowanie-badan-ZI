\name{sprawdz_zadanie}
\alias{sprawdz_zadanie}
\title{Lokalna kontrola Rmd i utworzenie aktualnego PDF}
\usage{
sprawdz_zadanie(id, katalog = NULL, uruchom = TRUE)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

\item{uruchom}{Czy utworzyć aktualny PDF. FALSE tylko sprawdza zapisany
PDF i jego zgodność z bieżącymi wejściami, bez wykonywania kodu.}
}
\value{
Lista: ok, tabela kontroli i jawna lista plików oddania.
}
\description{
Sprawdza własne pola odpowiedzi oraz zgodność gotowego kodu, ID i danych.
Nie ocenia sensu interpretacji ani długości akapitów. Domyślnie tworzy PDF
od początku w świeżym procesie R, bez logowania i bez dostępu do tokenu
sesji. Wymaga narzędzi PDF przygotowanych przez informatyka.
Kontrola nie wysyła pracy. Brak sieci nie blokuje tworzenia PDF.
}
\examples{
k <- tempfile("kontrola-")
utworz_projekt("s017", katalog = k)
przygotuj_zadanie("Z01", k)
sprawdz_zadanie("Z01", k, uruchom = FALSE)$ok
unlink(k, recursive = TRUE)
}
