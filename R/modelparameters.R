#' Modelparameters berekenen voor opgegeven model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven basismodel, extra model of afgeleid model (verschoven Vlaams model).  Ze berekent de parameters voor het domeinmodel en ingeval van het basismodel ook voor het Vlaams model, en geeft ook de grenzen van het bruikbaar interval.  (Deze functie verwijst naar de functies modelparameters.basis, modelparameters.extra of modelparameters.afgeleid, afhankelijk van de situatie)
#'
#' @param Basismodel model per boomsoort (basismodel) of model per boomsoort-domein-combinatie (extramodel)
#' @param Data meetgegevens (enkel nodig voor model per boomsoort-domein-combinatie)
#' @param Afgeleidmodel voor de berekening van de modelparameters van het afgeleid model (verschoven Vlaams model) moeten zowel het basismodel als het afgeleid model gegeven worden, dus in dit geval wordt hier het afgeleid model meegegeven.  Voor de andere modellen mag dit argument niet toegevoegd worden.
#'
#' @return dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en ingeval van het basismodel de parameters voor Vlaams model (Avl, Bvl en Cvl).  Ingeval van een afgeleid model worden de parameters voor het Vlaams model gegeven (Avl, Bvl en Cvl), en een parameter Ad die de verschuiving van het Vlaams model naar het afgeleide domeinmodel weergeeft (dus een extra intercept)
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr %>% rowwise do_ inner_join group_by_ ungroup select_ distinct_
#'

modelparameters <- function(Basismodel, Data = NULL, Afgeleidmodel = NULL) {

  if (!is.null(Afgeleidmodel)) {
    Parameters <- Afgeleidmodel[[1]] %>%
      inner_join(
        Afgeleidmodel[[2]],
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      group_by_(
        ~BMS,
        ~DOMEIN_ID,
        ~Q5k,
        ~Q95k
      ) %>%
      do_(
        ~modelparameters.afgeleid(.$Model[[1]])
      ) %>%
      ungroup() %>%
      inner_join(
        modelparameters(Basismodel) %>%
          select_(~BMS, ~Avl, ~Bvl, ~Cvl) %>%
          distinct_(),
        by = c("BMS")
      )
  } else {
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
  }



  return(Parameters)
}
