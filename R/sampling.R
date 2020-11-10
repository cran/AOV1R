#' @title Simulation of one-way random effect ANOVA
#' @description Simulates a balanced one-way random effect ANOVA model.
#'
#' @param I integer, number of groups
#' @param J integer, number of replicates per group
#' @param mu numeric, overall mean
#' @param sigmab positive number, the between standard deviation
#' @param sigmaw positive number, the within standard deviation
#'
#' @return A dataframe.
#'
#' @export
#' @importFrom utils stack
#' @importFrom stats setNames rnorm
#' @importFrom purrr map
#' @importFrom cellranger num_to_letter
#'
#' @examples
#' simAOV1R(I=2, J=3, mu=10, sigmab=1, sigmaw=1)
simAOV1R <- function(I, J, mu, sigmab, sigmaw){
  setNames(
    stack(
      setNames(
        purrr::map(rnorm(I,mu,sigmab), ~ rnorm(J, .x, sigmaw)),
        cellranger::num_to_letter(1:I))),
    c("y", "group"))
}
