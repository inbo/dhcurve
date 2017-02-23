#' Modelparameters berekenen voor opgegeven extra model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven extra model.  Ze berekent de parameters voor het domeinmodel en geeft de grenzen van het bruikbaar interval
#'
#' @param Extramodellen model per boomsoort-domein-combinatie
#' @param Data
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd)
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ select_ distinct_ left_join bind_rows
#' @importFrom stats coef
#'

modelparameters.extra <- function(Extramodellen, Data) {

  Parameters <- data.frame(NULL)
  for (Rijnummer in seq_along(Extramodellen$BMS)) {
    #dataset ophalen uit model
    Soortmodel <- (Extramodellen[Rijnummer, "Model"] %>% "[["(1))[[1]]
    Boomsoort <- Extramodellen[Rijnummer,]$BMS
    Domein <- Extramodellen[Rijnummer,]$DOMEIN_ID
    Soortdata <- Data %>%
      filter_(~ BMS == Boomsoort &
                DOMEIN_ID == Domein)

    Parameters.soort <- data.frame(BMS = Boomsoort,
                                   DOMEIN_ID = Domein,
                                   Ad = coef(Soortmodel)[[1]],
                                   Bd = coef(Soortmodel)[[2]],
                                   Cd = coef(Soortmodel)[[3]],
                                   stringsAsFactors = FALSE)

    Soortparameters <- Soortdata %>%
      select_(
        ~DOMEIN_ID,
        ~Q5k,
        ~Q95k
      ) %>%
      distinct_() %>%
      left_join(
        Parameters.soort,
        by = c("DOMEIN_ID")
      )

    Parameters <- Parameters %>%
      bind_rows(Soortparameters)
  }

  return(Parameters)
}
