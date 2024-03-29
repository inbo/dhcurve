#' @title Modelparameters berekenen voor opgegeven afgeleid model
#'
#' @description
#' Interne functie die de modelparameters berekent op basis van een opgegeven
#' afgeleid model.  Ze berekent de parameter voor de verschuiving van het
#' basismodel.
#'
#' @param Soortmodel model voor boomsoort-domeincombinatie
#'
#' @return dataframe met parameter voor de verschuiving t.o.v. het basismodel
#' (Ad)
#'
#' @noRd
#'
#' @importFrom stats coef
#'

modelparameters.afgeleid <- function(Soortmodel) {

    Parameters.soort <- data.frame(Ad = coef(Soortmodel)[[1]],
                                   stringsAsFactors = FALSE)

  return(Parameters.soort)
}
