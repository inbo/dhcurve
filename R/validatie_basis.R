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
#' @importFrom dplyr %>% filter_ transmute_
#'

validatie.basis <- function(Basismodel){

  Rmse <- rmse.basis(Basismodel)
  #slechte modellen nog uitselecteren

  #functie afwijkendeMetingen nog uitwerken
  AfwijkendeMetingen <- NULL

  #afwijkende curves
  Parameters_Extr <- curvekarakteristieken(Basismodel) %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d > 0.1 | Omtrek_Extr_Hoogte.vl > 0.1,
      ~Omtrek_Extr_Hoogte.d < Q95 | Omtrek_Extr_Hoogte.vl < Q95
    )

  #hoog minimum domeinmodel
  HoogMin <- Parameters_Extr %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d > 0.1,
      ~Hoogteverschil.d < 0,
      ~Omtrek_Buigpunt.d > Q5,
      ~Verschil_rico_BP_Q5.d > 1
    ) %>%
    transmute_(
      ~DOMEIN_ID,
      ~BMS
    )

  #laag maximum domeinmodel
  LaagMax <- Parameters_Extr %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d < Q95,
      ~Hoogteverschil.d > 0
    ) %>%
    transmute_(
      ~DOMEIN_ID,
      ~BMS
    )


  #functie validatierapport nog uitwerken

  return(AfwijkendeMetingen)

}
