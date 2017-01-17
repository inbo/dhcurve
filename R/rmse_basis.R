#' RMSE berekenen van basismodel
#'
#' Deze functie berekent de rmse op basis van testgroepen en omvat de volgende deelstappen:
#'
#' - metingen opdelen in 6 testgroepen (veld testgroep)
#'
#' - modellen fitten voor testgroepen, waarbij ze de functie fit.basis 6 keer oproept
#'
#' - rmse berekenen voor domeinmodellen en Vlaams model op basis van testgroep-modellen
#'
#' @param Basismodel model per boomsoort als argument meegeven en hier de nodige gegevens uit halen  (Vermits de 2 hoofdfuncties waarin deze hulpfunctie opgeroepen wordt allebei het argument model beschikbaar hebben en de dataframe niet, lijkt het me het meest logisch om hier van het model te vertrekken, dan moet het script om de meetgegevens uit het model te halen, enkel in deze functie geschreven worden)  Een alternatief is vertrekken van het dataframe > 50 en min. 6 domeinen
#'
#' @return dataframe met rmse_domein en rmse_Vlaams
#'
#' @export
#'

rmse.basis <- function(Basismodel){

}
