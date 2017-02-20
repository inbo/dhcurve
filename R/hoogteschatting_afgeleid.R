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
#' @importFrom dplyr %>% filter_ full_join mutate_ select_
#'

hoogteschatting.afgeleid <- function(Afgeleidmodel, Data.afgeleid) {

  AlleKlassen <- seq(55, 245, 10)

  Schatting <- merge(Afgeleidmodel, AlleKlassen) %>%
    filter_(
      ~y >= (100 * Q5k) - 1,
      ~y <= (100 * Q95k) + 1
    ) %>%
    mutate_(
      Omtrek = ~y / 100,
      logOmtrek = ~log(Omtrek),
      logOmtrek2 = ~logOmtrek^2
    ) %>%
    mutate_(
      H_D_finaal = ~Ad + Bvl * logOmtrek + Cvl * logOmtrek2,
      H_VL_finaal = ~Avl + Bvl * logOmtrek + Cvl * logOmtrek2
    ) %>%
    select_(~-logOmtrek, ~-logOmtrek2, ~-Q5, ~-Q95) %>%
    full_join(
      Data.afgeleid %>%
        mutate_(y = ~as.integer(round(100 * Omtrek))) %>%
        select_(~-Omtrek),
      by = c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomen", "nBomenInterval", "nBomenOmtrek05",
             "Q5k", "Q95k", "y")
    ) %>%
    select_(~-y)

  return(Schatting)
}
