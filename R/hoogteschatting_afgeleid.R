#' Hoogteschatting op basis van opgegeven afgeleid model
#'
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves en Vlaamse curves van het opgegeven afgeleid model.  De teruggegeven dataframe kan gebruikt worden om grafieken te maken of afwijkende metingen te bestuderen.
#'
#' @param Afgeleidmodel verschuiving per boomsoort en domein (verschoven Vlaams model)
#' @param Data.afgeleid dataframe 10-50
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes voor het domeinmodel en de Vlaamse model
#'
#' @export
#'
#' @importFrom dplyr %>% left_join mutate_
#'

hoogteschatting.afgeleid <- function(Afgeleidmodel, Data.afgeleid) {

  Schatting <- Data.afgeleid %>%
    left_join(
      Afgeleidmodel,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    mutate_(
      H_D_finaal = ~Ad + Bvl * logOmtrek + Cvl * logOmtrek2,
      H_VL_finaal = ~Avl + Bvl * logOmtrek + Cvl * logOmtrek2
    )

  return(Schatting)
}
