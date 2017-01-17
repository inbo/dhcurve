#' Geeft de afwijkende metingen uit een gegeven model
#'
#' Berekent afwijkende metingen, dit zijn metingen met een afwijking > 2,5 * rmse, en geeft deze weer volgens dalende afwijking
#'
#' @param Basismodel model per boomsoort
#' @param Rmse.basis rmse_domein en rmse_Vlaams
#' @param Afgeleidmodel ingeval van verschoven Vlaams model
#' @param Rmse.afgeleid ingeval van verschoven Vlaams model
#'
#' @return lijst met afwijkende metingen (> 2,5 * rmse), inclusief vlag uit databank
#'
#' @export
#'

afwijkendeMetingen <- function(Basismodel, Rmse_domein, Verschuiving, Rmse_verschuiving){

}
