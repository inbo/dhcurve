#' Modelparameters berekenen voor opgegeven basismodel
#'
#' Functie die de modelparameters berekent op basis van een opgegeven
#' basismodel.  Ze berekent zowel de parameters voor het domeinmodel als voor
#' het Vlaams model, en geeft ook de grenzen van het bruikbaar interval
#'
#' @param Soortmodel model van boomsoort
#' @param BMS boomsoort
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en
#' parameters voor Vlaams model (Avl, Bvl en Cvl)
#'
#' @importFrom dplyr %>% select distinct left_join
#' @importFrom rlang .data
#' @importFrom nlme fixef
#' @importFrom stats coef
#'

modelparameters.basis <- function(Soortmodel, BMS) {

  Parameters.soort <- data.frame(BMS = BMS,
                                 DOMEIN_ID = rownames(coef(Soortmodel)),
                                 Ad = coef(Soortmodel)[[1]],
                                 Bd = coef(Soortmodel)[[2]],
                                 Cd = coef(Soortmodel)[[3]],
                                 Avl = fixef(Soortmodel)[[1]],
                                 Bvl = fixef(Soortmodel)[[2]],
                                 Cvl = fixef(Soortmodel)[[3]],
                                 stringsAsFactors = FALSE)

  Soortparameters <- Soortmodel$data %>%
    select(
      .data$DOMEIN_ID,
      .data$Q5k,
      .data$Q95k
    ) %>%
    distinct() %>%
    left_join(
      Parameters.soort,
      by = c("DOMEIN_ID")
    ) %>%
    select(
      "BMS", "DOMEIN_ID", "Q5k", "Q95k",
      "Ad", "Bd", "Cd", "Avl", "Bvl", "Cvl"
    )

  return(Soortparameters)
}
