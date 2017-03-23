#' Lokaal model fitten op basis van de opgegeven dataset
#'
#' Functie die het lineair (fixed) model fit op basis van de opgegeven dataset.  Idee is dat deze functie gebruikt wordt voor het fitten van het volledige model, maar ook voor het fitten van modellen op basis van een gedeeltelijke dataset (zie rmse.lokaal)
#'
#' @param Data.lokaal dataframe > 50 en < 6 domeinen
#'
#' @return list met model per boomsoort-domein-combinatie
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom dplyr %>% group_by_ do_ ungroup
#'

fit.lokaal <- function(Data.lokaal) {

  Lokaalmodel <- Data.lokaal %>%
    group_by_(~BMS, ~DOMEIN_ID) %>%
    do_(
      Model = ~ lm(
        HOOGTE ~ logOmtrek + logOmtrek2,
        data = .
      )
    ) %>%
    ungroup()

  return(Lokaalmodel)
}
