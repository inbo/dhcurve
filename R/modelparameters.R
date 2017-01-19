#' Modelparameters berekenen voor opgegeven basismodel
#'
#' Functie die de modelparameters berekent op basis van een opgegeven basismodel.  Ze berekent zowel de parameters voor het domeinmodel als voor het Vlaams model, en geeft ook de grenzen van het bruikbaar interval
#'
#' @param Basismodel model per boomsoort
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en parameters voor Vlaams model (Avl, Bvl en Cvl)
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ select_ distinct_ left_join bind_rows
#' @importFrom nlme fixef
#' @importFrom stats coef
#'

modelparameters <- function(Basismodel) {

  Parameters <- data.frame(NULL)
  for (Boomsoort in Basismodel$BMS) {
    #dataset ophalen uit model
    Soortmodel <- (Basismodel %>% filter_(~ BMS == Boomsoort))$Model[[1]]

    Parameters.soort <- data.frame(BMS = Boomsoort,
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
        ~Q5,
        ~Q95
      ) %>%
      distinct_() %>%
      left_join(
        Parameters.soort,
        by = c("DOMEIN_ID")
      )

    Parameters <- Parameters %>%
      bind_rows(data.frame(BMS = Boomsoort, Soortparameters,
                           stringsAsFactors = FALSE))
  }

  return(Parameters)
}
