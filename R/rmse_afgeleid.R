#' RMSE berekenen van afgeleid model (verschoven Vlaams model)
#'
#' deze functie berekent de rmse op basis van verschil tussen geschatte domeinwaarde en gemeten waarde (voor omtrekklassen > 0.5 m)
#'
#' DEZE FUNCTIE IS NIET NODIG, fit.afgeleid BEREKENT METEEN DE RMSE!!!
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (verschoven Vlaams model)
#' @param Data.afgeleid dataframe 10-50
#'
#' @return dataframe met rmse_verschuiving per boomsoort en domein
#'
#' @export
#'

rmse.afgeleid <- function(Basismodel, Afgeleidmodel, Data.afgeleid){

}
