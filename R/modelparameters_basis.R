#' Modelparameters berekenen voor opgegeven basismodel
#'
#' Functie die de modelparameters berekent op basis van een opgegeven
#' basismodel.  Ze berekent zowel de parameters voor het domeinmodel als voor
#' het Vlaams model, en geeft ook de grenzen van het bruikbaar interval
#'
#' @param Soortmodel model van boomsoort
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en
#' parameters voor Vlaams model (Avl, Bvl en Cvl)
#'
#' @importFrom dplyr %>% select_ distinct_ left_join
#' @importFrom nlme fixef
#' @importFrom stats coef
#'

modelparameters.basis <- function(Soortmodel) {

  Parameters.soort <- data.frame(BMS = unique(Soortmodel$data$BMS),
                                 DOMEIN_ID = rownames(coef(Soortmodel)),
                                 Ad = coef(Soortmodel)[[1]],
                                 Bd = coef(Soortmodel)[[2]],
                                 Cd = coef(Soortmodel)[[3]],
                                 Avl = fixef(Soortmodel)[[1]],
                                 Bvl = fixef(Soortmodel)[[2]],
                                 Cvl = fixef(Soortmodel)[[3]],
                                 stringsAsFactors = FALSE)

  Soortparameters <- Soortmodel$data %>%
    select_(
      ~DOMEIN_ID,
      ~Q5k,
      ~Q95k
    ) %>%
    distinct_() %>%
    left_join(
      Parameters.soort,
      by = c("DOMEIN_ID")
    ) %>%
    select_(
      ~"BMS", ~"DOMEIN_ID", ~"Q5k", ~"Q95k",
      ~"Ad", ~"Bd", ~"Cd", ~"Avl", ~"Bvl", ~"Cvl"
    )

  return(Soortparameters)
}
