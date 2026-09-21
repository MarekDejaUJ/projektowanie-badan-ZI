FROM ubuntu:24.04
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    r-base-core r-cran-digest r-cran-gert r-cran-ggplot2 r-cran-httr2 \
    r-cran-jsonlite r-cran-knitr r-cran-processx r-cran-rmarkdown r-cran-yaml \
    r-cran-readxl pandoc texlive-xetex texlive-latex-extra texlive-fonts-recommended \
    texlive-lang-polish fonts-dejavu-core fonts-lmodern lmodern && apt-get clean
COPY badaniaZI.tar.gz /opt/badaniazi/badaniaZI.tar.gz
RUN Rscript --vanilla -e 'stopifnot(isTRUE(l10n_info()[["UTF-8"]]))'
RUN kpsewhich lmodern.sty
RUN R CMD INSTALL /opt/badaniazi/badaniaZI.tar.gz
RUN Rscript --vanilla -e 'stopifnot(file.copy(system.file("skrypty", "kontroluj-oddanie.R", package="badaniaZI"), "/opt/badaniazi/kontrola.R"))'
USER 65534:65534
WORKDIR /work
ENTRYPOINT ["/usr/bin/Rscript", "--vanilla", "/opt/badaniazi/kontrola.R"]
