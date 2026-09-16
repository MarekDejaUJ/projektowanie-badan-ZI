# Paleta Okabe–Ito i wspólny styl wykresów.
kolory_zi <- c(
  primary   = "#0072B2",
  secondary = "#56B4E9",
  accent    = "#D55E00",
  success   = "#009E73",
  warning   = "#E69F00",
  info      = "#CC79A7",
  light     = "#F0F3F5",
  dark      = "#222222"
)

#' Paleta Okabe--Ito dla materiałów kursu
#' @return Nazwany wektor kolorów.
#' @expor
#' @examples
#' paleta_zi()["primary"]
paleta_zi <- function() kolory_zi

#' Styl wykresów kursu
#' @param base_size Wielkość czcionki w punktach.
#' @return Obiekt theme z ggplot2.
#' @expor
#' @examples
#' theme_zi()
theme_zi <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2, color = kolory_zi["dark"]),
      plot.subtitle = ggplot2::element_text(color = "gray35"),
      axis.title = ggplot2::element_text(face = "bold", color = kolory_zi["dark"]),
      axis.text = ggplot2::element_text(color = "gray25"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "gray88", linewidth = 0.3),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", color = kolory_zi["dark"])
    )
}
