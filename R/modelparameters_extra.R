#' Modelparameters berekenen voor opgegeven extra model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven extra model.  Ze berekent de parameters voor het domeinmodel en geeft de grenzen van het bruikbaar interval
#'
#' @param Soortmodel model voor boomsoort-domein-combinatie
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd)
#'
#' @export
#'
#' @importFrom stats coef
#'

modelparameters.extra <- function(Soortmodel) {

    Parameters.soort <- data.frame(Ad = coef(Soortmodel)[[1]],
                                   Bd = coef(Soortmodel)[[2]],
                                   Cd = coef(Soortmodel)[[3]],
                                   stringsAsFactors = FALSE)

  return(Parameters.soort)
}
