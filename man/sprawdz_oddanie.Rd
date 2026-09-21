\name{sprawdz_oddanie}
\alias{sprawdz_oddanie}
\title{Niezależne odtworzenie pobranego oddania}
\usage{
sprawdz_oddanie(katalog, obraz, timeout = 360)
}
\arguments{
\item{katalog}{Katalog utworzony przez pobierz_oddanie().}

\item{obraz}{Pełny lokalny identyfikator obrazu `sha256:...`, wskazany
przez informatyka. Nie nazwa zmiennego tagu.}

\item{timeout}{Limit kontroli w sekundach, 30--600.}
}
\value{
Lista: ok, katalog wyniku i raport kontroli. PDF kontrolny może
  różnić się bajtami od PDF studenta z innego systemu; nie zastępuje oryginału.
}
\description{
Narzędzie prowadzącego, nie polecenie studenta. Wymaga wcześniej zbudowanego
i zaufanego obrazu Linux z pakietem kursu oraz Docker. Uruchamia kontrolę
bez sieci, tokenów, uprawnień root i zapisu do systemu hosta. Do kontenera
trafia tylko odrębna kopia pobranego oddania, zamontowana tylko do odczytu.
Nie instaluje Docker, nie pobiera obrazów i nie zastępuje oceny interpretacji.
Wynik i odtworzony PDF zapisuje w nowym prywatnym podkatalogu; nie wysyła
oceny ani nie modyfikuje oryginalnego oddania na GitHub.
}
\examples{
\dontrun{
sprawdz_oddanie("prywatna-kontrola-z01", obraz = "sha256:IDENTYFIKATOR_OBRAZU")
}
}
