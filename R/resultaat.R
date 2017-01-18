#' berekent de modelparameters per domein
#'
#' Berekeningen die leiden tot de gevraagde tabel.  Hiervoor worden volgende hulpfuncties aangeroepen:
#'
#' rmse.basis
#' rmse.afgeleid
#'
#' Verder moeten parameters A, B en C uit het model gehaald worden (coef) en een aantal berekende gegevens uit de eerste stap toegevoegd worden.
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (Vlaams model)
#' @param Data.afgeleid dataframe 10-50 (nodig voor rmse.afgeleid)
#' @param Data.ontbrekend evt. lijst van domeinen < 10 per boomsoort (en dan voor deze domeinen het Vlaams model invullen?)
#'
#' @return dataframe met modellen per domein en per boomsoort met velden:
#'
#' - ModelID
#'
#' - DomeinID
#'
#' - BoomsoortID
#'
#' - type model (‘eigen model’/‘verschoven Vlaams model’/‘Vlaams model’)
#'
#' - paramaters A,B en C
#'
#' - bruikbaar interval
#'
#' - rmse
#'
#' - aantal metingen waarop model gebaseerd is
#'
#' - aantal metingen > 0.5 m (dus waarop rmse-berekening gebaseerd is)
#'
#' evt. kan een tweede dataframe toegevoegd worden met Vlaamse modellen per boomsoort, of deze kan toegevoegd worden aan de vorige dataframe, waarbij DomeinID leeg gelaten wordt of een specifieke waarde ‘Vlaams model’ krijgt
#'

resultaat <- function(Basismodel, Afgeleidmodel, Data.afgeleid, Data.ontbrekend){

}