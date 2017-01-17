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
#'

fit.basis <- function(Data.basis) {

  Modellijst <- list()
  for (Boomsoort in unique(Data.basis$BMS)) {
    Mixed_model <- lme(HOOGTE ~ logOmtrek + logOmtrek2, data = Data.basis,
                       random = ~ (logOmtrek + logOmtrek2)|DOMEIN_ID,
                       control = lmeControl(opt = "optim", singular.ok = TRUE,
                                            returnObject = TRUE))
    Modellijst[[Boomsoort]] <- Mixed_model
  }

  return(Modellijst)
}
