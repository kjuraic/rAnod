

# izraz za struju anodizacije iz clanka:
# R. Jin, et al., A capacitor circuit model for theoretical derivation of anodizing current, Electrochim. Acta (2016),
# http://dx.doi.org/10.1016/j.electacta.2016.11.066

#' j_anod_jin(t, k1, k2, k3, k4, k5, t0 = 0, U0 = 60, d = 0.005)
#' @author K. Juraic
#' @description Theoretical expresion for anodisation current as a function of time.
#'              Equation is adopted from publication:
#'              R. Jin, et al., A capacitor circuit model for theoretical derivation of anodizing current, Electrochim. Acta (2016), http://dx.doi.org/10.1016/j.electacta.2016.11.066
#'              For detailed description of the model and used parameters check
#'              R. Jin publication
#' @param t time variable (usualy given in seconds)
#' @param k1 model parameter (see R. Jin)
#' @param k2 model parameter (see R. Jin)
#' @param k3 model parameter (see R. Jin)
#' @param k4 model parameter (see R. Jin)
#' @param k5 model parameter (see R. Jin)
#' @param t0 model parameter (see R. Jin) Starting moment of experiment (voltage switched on)
#' @param U0 model parameter (see R. Jin) anodisation voltage
#' @param d model parameter (see R. Jin) distance between electrodes
#' @return j(t) for given set of parameters
#' @export
#' @importFrom forcats %>%
#' @importFrom ggplot2 ggplot geom_line theme_linedraw
#' @examples
#'            library(ggplot2)
#'            k1 <- 55.556
#'            k2 <- 9.086
#'            k3 <- 6.5734
#'            k4 <- 0.047
#'            k5 <- 5# 6.6772
#'            t0 <- 0
#'            U0 <- 60
#'            d <- 0.005
#'            t <- 0:900
#'            j1 <- U0 / k2 * exp(-(t - t0) / k1) / (1 - exp(-(t - t0) / k3))
#'            j2 <- k4 / sqrt(d) * (U0  * (1 - exp(-(t - t0) / k1)) - k5 * log(t - t0))
#'            anod_sim <- data.frame(
#'              t = t,
#'              struja = j_anod_jin(t = t, t0 = t0, k1 = k1, k2 = k2, k3 = k3, k4 = k4, k5 = k5),
#'              j1 = j1,
#'              j2 = j2,
#'              j_uk = j1 + j2
#'            )
#'            head(anod_sim)
#'            ggplot2::ggplot(data = anod_sim) +
#'              geom_line(mapping = aes(x = t, y = struja), color = "red") +
#'              geom_line(data = anod_sim, mapping = aes(x = t, y = j1), color = "blue") +
#'              geom_line(data = anod_sim, mapping = aes(x = t, y = j2), color = "green") +
#'              geom_line(data = anod_sim, mapping = aes(x = t, y = j_uk), color = "purple") +
#'              theme_linedraw(base_size = 20)
j_anod_jin <- function(t, k1, k2, k3, k4, k5, t0 = 0, U0 = 60, d = 0.005) {
  exp_k1 <- exp(-(t - t0) / k1)
  exp_k3 <- exp(-(t - t0) / k3)
  janod <- U0 / k2 * exp_k1 / (1 - exp_k3) + k4 / sqrt(d) * (U0  * (1 - exp_k1) - k5 * log(t - t0))
  janod
}
