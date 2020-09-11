#' Berekent curvekarakteristieken van de domeinmodellen
#'
#' Berekent de curvekarakteristieken die toelaten om mogelijke afwijkingen in
#' de curvevorm te detecteren: extremen (min en max), buigpunt,...
#'
#' @inheritParams afwijkendeCurves
#'
#' @return Dataframe met curvekarakteristieken per domein en boomsoort met de
#' velden:
#'
#' - parameters Ad, Bd en Cd van het model en enkele generieke velden
#' (DOMEIN_ID, BMS,...)
#'
#' - Omtrek_Extr_Hoogte.d: midden van omtrekklasse waarin een extreem (minimum
#' of maximum) van de curve van het domeinmodel ligt
#'
#' - Extr_Hoogte.d: hoogteschatting die aansluit bij het vorige veld
#'
#' - Hoogteverschil.d: verschil tussen de hoogteschattingen van het extreem van
#' de curve en de bovengrens van het bruikbaar interval.  Deze maat is relevant
#' als het extreem een maximum is (enkel dan is deze waarde positief).
#'
#' - Omtrek_Buigpunt.d: midden van omtrekklasse waarin een buigpunt van de curve
#' van het domeinmodel ligt (deze en volgende variabelen zijn relevante maten
#' bij het voorkomen van een minimum)
#'
#' - Verschil_rico_BP_Q5.d: het verschil in de waarde van de
#' richtingscoefficient tussen het buigpunt en de ondergrens van het bruikbaar
#' interval.
#'
#' - Verschil_rico_BP_Q5_per_omtrek.d: de voorgaande variabele gedeeld door het
#' verschil in omtrek tussen het buigpunt en de ondergrens van het bruikbaar
#' interval.
#'
#' Ingeval van een basismodel worden deze variabelen aangevuld met dezelfde
#' variabelen voor het Vlaams model.  Hierbij is de 'd' op het einde van de
#' variabelenaam vervangen door 'vl'.
#'
#'
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_
#' @importFrom assertthat has_name
#'

curvekarakteristieken <- function(Basismodel, Data = NULL) {

  #controle invoer
  if (has_name(Basismodel, "DOMEIN_ID")) {
    invoercontrole(Basismodel, "lokaalmodel")
    if (is.null(Data)) {
      stop("Bij opgave van een lokaal model moet je ook de dataset meegeven")
    } else {
      invoercontrole(Data, "fit")
    }
  } else {
    invoercontrole(Basismodel, "basismodel")
  }

  Parameters <- modelparameters(Basismodel, Data) %>%
    mutate_(
      Omtrek_Extr_Hoogte.d = ~exp(-Bd / (2 * Cd)),
      Extr_Hoogte.d =
        ~Ad + Bd * log(Omtrek_Extr_Hoogte.d) +
        Cd * (log(Omtrek_Extr_Hoogte.d)) ^ 2,
      Hoogteverschil.d =
        ~Extr_Hoogte.d - (Ad + Bd * log(Q95k) + Cd * (log(Q95k)) ^ 2),
      Omtrek_Buigpunt.d = ~exp(1 - Bd / (2 * Cd)),
      Verschil_rico_BP_Q5.d =
        ~ (2 * Cd * log(Omtrek_Buigpunt.d) + Bd) / Omtrek_Buigpunt.d -
        (2 * Cd * log(Q5k) + Bd) / Q5k,
      Verschil_rico_BP_Q5_per_omtrek.d =
        ~Verschil_rico_BP_Q5.d * (Omtrek_Buigpunt.d - Q5k)
    )

  if (!has_name(Basismodel, "DOMEIN_ID")) {
    Parameters <- Parameters %>%
      mutate_(
        Omtrek_Extr_Hoogte.vl = ~exp(-Bvl / (2 * Cvl)),
        Extr_Hoogte.vl =
          ~Avl + Bvl * log(Omtrek_Extr_Hoogte.vl) +
          Cvl * (log(Omtrek_Extr_Hoogte.vl)) ^ 2,
        Hoogteverschil.vl =
          ~Extr_Hoogte.vl - (Avl + Bvl * log(Q95k) + Cvl * (log(Q95k)) ^ 2),
        Omtrek_Buigpunt.vl = ~exp(1 - Bvl / (2 * Cvl)),
        Verschil_rico_BP_Q5.vl =
          ~ (2 * Cvl * log(Omtrek_Buigpunt.vl) + Bvl) / Omtrek_Buigpunt.vl -
          (2 * Cvl * log(Q5k) + Bvl) / Q5k,
        Verschil_rico_BP_Q5_per_omtrek.vl =
          ~Verschil_rico_BP_Q5.vl * (Omtrek_Buigpunt.vl - Q5k)
      )
  }

  return(Parameters)

}
