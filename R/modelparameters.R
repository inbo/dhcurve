#' Modelparameters berekenen voor opgegeven model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven basismodel of extra.  Ze berekent de parameters voor het domeinmodel en ingeval van het basismodel ook voor het Vlaams model, en geeft ook de grenzen van het bruikbaar interval.  (Deze functie verwijst naar de functies modelparameters.basis of modelparameters.extra, afhankelijk van de situatie)
#'
#' @param Basismodel model per boomsoort of model per boomsoort-domein-combinatie
#' @param Data meetgegevens (enkel nodig voor model per boomsoort-domein-combinatie)
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en ingeval van het basismodel de parameters voor Vlaams model (Avl, Bvl en Cvl)
#'
#' @export
#'
#' @importFrom assertthat has_name
#'

modelparameters <- function(Basismodel, Data = NULL) {

  if (has_name(Basismodel, "DOMEIN_ID")) {
    Parameters <- modelparameters.extra(Basismodel, Data)
  } else {
    Parameters <- modelparameters.basis(Basismodel)
  }

  return(Parameters)
}
