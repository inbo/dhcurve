#' Validatie van het afgeleid model
#'
#' Functie die de validatie uitvoert op het verschoven Vlaams model. Vermits fit.afgeleid geen echte modelfit is en je deze validatiestap normaliter niet moet overslaan (na de laatste validatie exporteer je logischerwijs direct de gegevens), zou evt. overwogen kunnen worden om de voorgaande (fit.afgeleid) en deze stap (validatie.afgeleid) samen te nemen.  Een andere mogelijke piste is om Rmse.afgeleid toe te voegen aan fit.afgeleid.
#'
#' validatie.afgeleid roept meerdere hulpfuncties op:
#'
#' - rmse.afgeleid
#'
#' - afwijkendeMetingen
#'
#' - validatierapport
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de slechtste modellen opgelijst (op basis van rmse en afwijkende metingen).
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (verschoven Vlaams model)
#' @param Data.afgeleid dataframe 10-50
#'
#' @return Dataframe met te controleren metingen en document (html/pdf) met te controleren curves (incl. aantal metingen per curve) en grafieken van te controleren metingen
#'
#' @export
#'

validatie.afgeleid <- function(Basismodel, Afgeleidmodel, Data.afgeleid){

  Dataset <- hoogteschatting.afgeleid(Afgeleidmodel, Data.afgeleid)
  AfwijkendeMetingen <- afwijkendeMetingen(Dataset)

  #anomalieen nog verder selecteren
  #functie validatierapport nog uitwerken

}
