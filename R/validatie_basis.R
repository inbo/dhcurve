#' Validatie van het basismodel
#'
#' Functie die alle nodige validaties uitvoert op het opgegeven (volledige) model en een overzicht geeft van de afwijkende metingen en slechte curves (zodat de gebruiker deze kan valideren). De functie roept meerdere hulpfuncties op:
#'
#' - rmse.basis
#'
#' - afwijkendeMetingen
#'
#' - curvekarakteristieken
#'
#' - validatierapport
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de slechtste modellen opgelijst (op basis van rmse, curvekarakteristieken en afwijkende metingen).
#'
#' @param Basismodel model per boomsoort
#'
#' @return Dataframe met te controleren metingen en document (html/pdf) met te controleren curves (incl. aantal metingen per curve) en grafieken van te controleren metingen
#'
#' @export
#'

validatie.basis <- function(Basismodel){

}
