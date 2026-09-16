#' Zapis wyników i warunkowe wnioski statystyczne
#'
#' Funkcje formatują wynik; samodzielnie opisz kierunek, znaczenie praktyczne,
#' założenia i ograniczenia. Brak istotności nie dowodzi braku efektu.
#' @param test_result,kw_result,chi_result,fisher_result,cor_result Wynik właściwego testu z pakietu stats.
#' @param aov_result Wynik jednoczynnikowej analizy aov().
#' @param lm_result Dopasowany model lm().
#' @param alpha Poziom istotności, większy od 0 i mniejszy od 1.
#' @param efekt Opcjonalna, samodzielnie obliczona wielkość efektu.
#' @param efekt_label Etykieta podanej wielkości efektu.
#' @return Tekst Markdown z oznaczeniami matematycznymi obsługiwanymi przez R Markdown.
#' @name wnioski
#' @examples
#' wniosek_ind(stats::t.test(c(2, 3, 5, 6), c(4, 6, 8, 9)))
#' wniosek_cor(stats::cor.test(1:8, c(2, 1, 5, 3, 7, 6, 4, 8), method = "spearman"))
NULL

format_liczba_apa <- function(x, digits = 2, zero_wiodace = TRUE) {
    if (length(x) == 0 || is.null(x) || is.na(x[1]))
        return("NA")
    out <- formatC(as.numeric(x)[1], format = "f", digits = digits)
    if (!zero_wiodace) {
        out <- sub("^(-?)0\\.", "\\1.", out)
    }
    ou
}

format_df_apa <- function(df) {
    if (length(df) == 0 || is.null(df) || is.na(df[1]))
        return("NA")
    df <- as.numeric(df)[1]
    if (abs(df - round(df)) < 0.05) {
        as.character(as.integer(round(df)))
    }
    else {
        format_liczba_apa(df, digits = 1)
    }
}

format_p_apa <- function(p) {
    if (length(p) == 0 || is.null(p) || is.na(p[1]))
        return("= NA")
    p <- as.numeric(p)[1]
    if (p < 0.001) {
        "< .001"
    }
    else {
        paste0("= ", format_liczba_apa(p, digits = 3, zero_wiodace = FALSE))
    }
}

fragment_efekt <- function(efekt = NULL, etykieta = NULL, digits = 2, zero_wiodace = FALSE) {
    if (is.null(efekt) || length(efekt) == 0 || is.na(efekt[1]))
        return("")
    if (is.null(etykieta))
        etykieta <- "wielko\u015b\u0107 efektu"
    paste0(", ", etykieta, " = ", format_liczba_apa(efekt, digits = digits, zero_wiodace = zero_wiodace))
}

tekst_istotnosci <- function(p, alpha, pozytywny, negatywny) {
    if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1)
        stop("Poziom alpha musi mie\u015bci\u0107 si\u0119 mi\u0119dzy 0 a 1.", call. = FALSE)
    if (!length(p) || is.na(p[1]) || !is.finite(p[1]))
        return("nie pozwala rozstrzygn\u0105\u0107 istotno\u015bci: brak warto\u015bci p")
    if (p[1] < 0 || p[1] > 1) stop("Warto\u015b\u0107 p musi mie\u015bci\u0107 si\u0119 od 0 do 1.", call. = FALSE)
    if (p[1] < alpha)
        pozytywny
    else negatywny
}

#' @rdname wnioski
#' @expor
wniosek_one_sample <- function(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*d*") {
    stat <- format_liczba_apa(test_result$statistic, digits = 2)
    df <- format_df_apa(test_result$parameter)
    p <- as.numeric(test_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie wzgl\u0119dem warto\u015bci referencyjnej",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie wzgl\u0119dem warto\u015bci referencyjnej")
    paste0("Test *t* dla jednej pr\u00f3by ", decyzja, ", *t*(", df, ") = ", stat, ", *p* ", p_fmt,
        efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_ind <- function(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*d*") {
    stat <- format_liczba_apa(test_result$statistic, digits = 2)
    df <- format_df_apa(test_result$parameter)
    p <- as.numeric(test_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy grupami",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy grupami")
    paste0("Test *t* dla pr\u00f3b niezale\u017cnych ", decyzja, ", *t*(", df, ") = ", stat,
        ", *p* ", p_fmt, efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_paired <- function(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*d*") {
    stat <- format_liczba_apa(test_result$statistic, digits = 2)
    df <- format_df_apa(test_result$parameter)
    p <- as.numeric(test_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy pomiarami",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy pomiarami")
    paste0("Test *t* dla pr\u00f3b zale\u017cnych ", decyzja, ", *t*(", df, ") = ", stat, ", *p* ",
        p_fmt, efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_mw <- function(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*r*") {
    stat <- format_liczba_apa(test_result$statistic, digits = 2)
    p <- as.numeric(test_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy grupami",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy grupami")
    paste0("Test Manna-Whitneya ", decyzja, ", *W* = ", stat, ", *p* ", p_fmt, efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_wilcox <- function(test_result, alpha = 0.05, efekt = NULL, efekt_label = "*r*") {
    stat <- format_liczba_apa(test_result$statistic, digits = 2)
    p <- as.numeric(test_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy pomiarami",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy pomiarami")
    paste0("Test Wilcoxona ", decyzja, ", *W* = ", stat, ", *p* ", p_fmt, efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_anova <- function(aov_result, alpha = 0.05, efekt = NULL, efekt_label = "$\\eta^2$") {
    s <- summary(aov_result)[[1]]
    df1 <- format_df_apa(s$Df[1])
    df2 <- format_df_apa(s$Df[2])
    f_stat <- format_liczba_apa(s$`F value`[1], digits = 2)
    p <- as.numeric(s$`Pr(>F)`[1])
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142a r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy grupami",
        "nie wykaza\u0142a podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy grupami")
    paste0("Jednoczynnikowa ANOVA ", decyzja, ", *F*(", df1, ", ", df2, ") = ", f_stat, ", *p* ",
        p_fmt, efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_kw <- function(kw_result, alpha = 0.05, efekt = NULL, efekt_label = "$\\epsilon^2$") {
    stat <- format_liczba_apa(kw_result$statistic, digits = 2)
    df <- format_df_apa(kw_result$parameter)
    p <- as.numeric(kw_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 r\u00f3\u017cnic\u0119 istotn\u0105 statystycznie mi\u0119dzy grupami",
        "nie wykaza\u0142 podstaw do stwierdzenia r\u00f3\u017cnicy istotnej statystycznie mi\u0119dzy grupami")
    paste0("Test Kruskala-Wallisa ", decyzja, ", *H*(", df, ") = ", stat, ", *p* ", p_fmt, efekt_txt,
        ".")
}

#' @rdname wnioski
#' @expor
wniosek_chi2 <- function(chi_result, alpha = 0.05, efekt = NULL, efekt_label = "*V* Cramera") {
    stat <- format_liczba_apa(chi_result$statistic, digits = 2)
    df <- format_df_apa(chi_result$parameter)
    p <- as.numeric(chi_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    n_txt <- if (!is.null(chi_result$observed)) {
        paste0(", *N* = ", sum(chi_result$observed))
    }
    else {
        ""
    }
    efekt_txt <- fragment_efekt(efekt, efekt_label)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 zale\u017cno\u015b\u0107 istotn\u0105 statystycznie mi\u0119dzy zmiennymi",
        "nie wykaza\u0142 podstaw do stwierdzenia zale\u017cno\u015bci istotnej statystycznie mi\u0119dzy zmiennymi")
    paste0("Test chi-kwadrat ", decyzja, ", $\\chi^2$(", df, n_txt, ") = ", stat, ", *p* ", p_fmt,
        efekt_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_fisher <- function(fisher_result, alpha = 0.05) {
    p <- as.numeric(fisher_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    metoda <- if (!is.null(fisher_result$method) && grepl("binomial|dwumian", fisher_result$method,
        ignore.case = TRUE)) {
        "Dok\u0142adny test dwumianowy"
    }
    else {
        "Dok\u0142adny test Fishera"
    }
    or_txt <- if (!is.null(fisher_result$estimate) && grepl("odds ratio", names(fisher_result$estimate)[1],
        ignore.case = TRUE)) {
        paste0(", OR = ", format_liczba_apa(fisher_result$estimate, digits = 2))
    }
    else {
        ""
    }
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 zale\u017cno\u015b\u0107 istotn\u0105 statystycznie mi\u0119dzy zmiennymi",
        "nie wykaza\u0142 podstaw do stwierdzenia zale\u017cno\u015bci istotnej statystycznie mi\u0119dzy zmiennymi")
    paste0(metoda, " ", decyzja, ", *p* ", p_fmt, or_txt, ".")
}

#' @rdname wnioski
#' @expor
wniosek_cor <- function(cor_result, alpha = 0.05) {
    metoda <- if (is.null(cor_result$method))
        ""
    else cor_result$method
    symbol <- if (grepl("Spearman", metoda, ignore.case = TRUE)) {
        "$\\rho$"
    }
    else if (grepl("Kendall", metoda, ignore.case = TRUE)) {
        "$\\tau$"
    }
    else {
        "*r*"
    }
    r <- format_liczba_apa(cor_result$estimate, digits = 2, zero_wiodace = FALSE)
    df <- if (!is.null(cor_result$parameter))
        paste0("(", format_df_apa(cor_result$parameter), ")")
    else ""
    p <- as.numeric(cor_result$p.value)[1]
    p_fmt <- format_p_apa(p)
    decyzja <- tekst_istotnosci(p, alpha, "wykaza\u0142 zwi\u0105zek istotny statystycznie mi\u0119dzy zmiennymi",
        "nie wykaza\u0142 podstaw do stwierdzenia zwi\u0105zku istotnego statystycznie mi\u0119dzy zmiennymi")
    paste0("Test korelacji ", decyzja, ", ", symbol, df, " = ", r, ", *p* ", p_fmt,
           fragment_przedzialu(cor_result$conf.int), ".")
}

#' @rdname wnioski
#' @expor
wniosek_reg <- function(lm_result, alpha = 0.05) {
    s <- summary(lm_result)
    if (is.null(s$fstatistic))
        return("Model zawiera tylko wyraz wolny; nie ma testu F zwi\u0105zku z predyktorem.")
    f_stat <- format_liczba_apa(s$fstatistic[1], digits = 2)
    df1 <- format_df_apa(s$fstatistic[2])
    df2 <- format_df_apa(s$fstatistic[3])
    p <- stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
    p_fmt <- format_p_apa(p)
    r2 <- format_liczba_apa(s$r.squared, digits = 2, zero_wiodace = FALSE)
    decyzja <- tekst_istotnosci(p, alpha, "okaza\u0142 si\u0119 istotny statystycznie", "nie okaza\u0142 si\u0119 istotny statystycznie")
    paste0("Model regresji ", decyzja, ", *F*(", df1, ", ", df2, ") = ", f_stat, ", *p* ", p_fmt,
        ", R\u00b2 = ", r2, ".")
}

fragment_przedzialu <- function(x) {
    if (length(x) != 2L || anyNA(x)) return("")
    poziom <- attr(x, "conf.level")
    etykieta <- if (is.null(poziom)) "CI" else paste0(round(100 * poziom), "% CI")
    paste0(", ", etykieta, " [", format_liczba_apa(x[1]), "; ", format_liczba_apa(x[2]), "]")
}
