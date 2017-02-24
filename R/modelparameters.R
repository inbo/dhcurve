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
#' @importFrom dplyr %>% rowwise do_ inner_join group_by_ ungroup
#'

modelparameters <- function(Basismodel, Data = NULL) {

  if (has_name(Basismodel, "DOMEIN_ID")) {
    Parameters <- Basismodel %>%
      inner_join(
        Data,
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      group_by_(
        ~BMS,
        ~DOMEIN_ID,
        ~Q5k,
        ~Q95k
      ) %>%
      do_(
        ~modelparameters.extra(.$Model[[1]])
      ) %>%
      ungroup()
  } else {
    Parameters <- Basismodel %>%
      rowwise() %>%
      do_(
        ~modelparameters.basis(.$Model)
      )
  }

  return(Parameters)
}
