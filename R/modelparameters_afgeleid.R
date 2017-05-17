#' Modelparameters berekenen voor opgegeven afgeleid model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven afgeleid model.  Ze berekent de parameter voor de verschuiving van het basismodel
#'
#' @param Soortmodel model voor boomsoort-domein-combinatie
#'
#' @return dataframe met parameter voor de verschuiving t.o.v. het basismodel (Ad)
#'
#' @importFrom stats coef
#'

modelparameters.afgeleid <- function(Soortmodel) {

    Parameters.soort <- data.frame(Ad = coef(Soortmodel)[[1]],
                                   stringsAsFactors = FALSE)

  return(Parameters.soort)
}
