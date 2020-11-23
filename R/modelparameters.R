#' Berekent modelparameters voor opgegeven model
#'
#' Functie die de modelparameters berekent op basis van een opgegeven
#' basismodel, lokaal model of afgeleid model.  Ze berekent de parameters voor
#' het domeinmodel en ingeval van het basismodel ook voor het Vlaams model, en
#' geeft ook de grenzen van het bruikbaar interval.
#'
#' (Deze functie verwijst naar de interne functies modelparameters.basis,
#' modelparameters.lokaal of modelparameters.afgeleid, afhankelijk van de
#' situatie)
#'
#' @param Basismodel model per boomsoort (basismodel) of model per
#' boomsoort-domein-combinatie (lokaalmodel)
#' @param Data meetgegevens (enkel nodig voor lokaal model)
#' @param Afgeleidmodel voor de berekening van de modelparameters van het
#' afgeleid model moeten zowel het basismodel als het afgeleid model gegeven
#' worden, dus in dit geval wordt hier het afgeleid model meegegeven.  Voor de
#' andere modellen mag dit argument niet toegevoegd worden.
#'
#' @return Dataframe met parameters voor domeinmodel (Ad, Bd en Cd) en ingeval
#' van het basismodel de parameters voor Vlaams model (Avl, Bvl en Cvl).
#' Ingeval van een afgeleid model worden de parameters voor het Vlaams model
#' gegeven (Avl, Bvl en Cvl), en een parameter Ad die de verschuiving van het
#' Vlaams model naar het afgeleide domeinmodel weergeeft (dus een extra
#' intercept)
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr %>% rowwise do inner_join group_by ungroup select
#' distinct
#' @importFrom plyr .
#' @importFrom rlang .data
#'

modelparameters <- function(Basismodel, Data = NULL, Afgeleidmodel = NULL) {

  invoercontrole(Basismodel, "basismodel")
  if (!is.null(Data)) {
    invoercontrole(Data, "fit")
  }
  if (!is.null(Afgeleidmodel)) {
    invoercontrole(Afgeleidmodel, "afgeleidmodel")
  }

  if (!is.null(Afgeleidmodel)) {
    Parameters <- Afgeleidmodel[[1]] %>%
      inner_join(
        Afgeleidmodel[[2]],
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      group_by(
        .data$BMS,
        .data$DOMEIN_ID,
        .data$BOS_BHI,
        .data$nBomenInterval,
        .data$nBomenOmtrek05,
        .data$Q5k,
        .data$Q95k
      ) %>%
      do(
        modelparameters.afgeleid(.$Model[[1]])
      ) %>%
      ungroup() %>%
      inner_join(
        modelparameters(Basismodel) %>%
          select(.data$BMS, .data$Avl, .data$Bvl, .data$Cvl) %>%
          distinct(),
        by = c("BMS")
      )
  } else {
    if (has_name(Basismodel, "DOMEIN_ID")) {
      Parameters <- Basismodel %>%
        inner_join(
          Data,
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        group_by(
          .data$BMS,
          .data$DOMEIN_ID,
          .data$BOS_BHI,
          .data$nBomenInterval,
          .data$nBomenOmtrek05,
          .data$Q5k,
          .data$Q95k
        ) %>%
        do(
          modelparameters.lokaal(.$Model[[1]])
        ) %>%
        ungroup()
    } else {
      Parameters <- Basismodel %>%
        rowwise() %>%
        do(
          modelparameters.basis(.$Model, .$BMS)
        )
     }
  }



  return(Parameters)
}
