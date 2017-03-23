#' Basismodel fitten op basis van de opgegeven dataset
#'
#' Functie die het lineair mixed model fit op basis van de opgegeven dataset.  Idee is dat deze functie gebruikt wordt voor het fitten van het volledige model, maar ook voor het fitten van modellen op basis van een gedeeltelijke dataset (zie rmse.basis)
#'
#' @param Data.basis dataframe > 50 en min. 6 domeinen
#'
#' @return list met model per boomsoort
#'
#' @export
#'
#' @importFrom nlme lme lmeControl
#' @importFrom dplyr %>% group_by_ do_ ungroup
#'

fit.basis <- function(Data.basis) {

  Basismodel <- Data.basis %>%
    group_by_(~BMS) %>%
    do_(
      Model = ~ lme(
        HOOGTE ~ logOmtrek + logOmtrek2,
        random = ~ (logOmtrek + logOmtrek2) | DOMEIN_ID,
        data = .,
        control = lmeControl(opt = "optim", singular.ok = TRUE,
                                         returnObject = TRUE)
      )
    ) %>%
    ungroup()

  return(Basismodel)
}
