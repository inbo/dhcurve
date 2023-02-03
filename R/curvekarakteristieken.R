#' @title Berekent curvekarakteristieken van de domeinmodellen
#'
#' @description
#' Berekent de curvekarakteristieken die toelaten om mogelijke afwijkingen in
#' de curvevorm te detecteren: extremen (min en max), buigpunt,...
#'
#' @inheritParams afwijkendeCurves
#'
#' @return Dataframe met curvekarakteristieken per domein en boomsoort met de
#' velden:
#' \itemize{
#'   \item{parameters `Ad`, `Bd` en `Cd` van het model en enkele generieke
#'     velden (`DOMEIN_ID`, `BMS`,...)}
#'   \item{`Omtrek_Extr_Hoogte.d`: midden van omtrekklasse waarin een extreem
#'     (minimum of maximum) van de curve van het domeinmodel ligt}
#'   \item{`Extr_Hoogte.d`: hoogteschatting die aansluit bij het vorige veld}
#'   \item{`Hoogteverschil.d`: verschil tussen de hoogteschattingen van het
#'     extreem van de curve en de bovengrens van het bruikbaar interval.
#'     Deze maat is relevant als het extreem een maximum is
#'     (enkel dan is deze waarde positief).}
#'   \item{`Omtrek_Buigpunt.d`: midden van omtrekklasse waarin een buigpunt van
#'     de curve van het domeinmodel ligt (deze en volgende variabelen zijn
#'     relevante maten bij het voorkomen van een minimum)}
#'   \item{`Verschil_rico_BP_Q5.d`: het verschil in de waarde van de
#'     richtingscoëfficiënt tussen het buigpunt en de ondergrens van het
#'     bruikbaar interval.}
#'   \item{`Verschil_rico_BP_Q5_per_omtrek.d`: de voorgaande variabele gedeeld
#'     door het verschil in omtrek tussen het buigpunt en de ondergrens van het
#'     bruikbaar interval.}
#' }
#'
#' Ingeval van een basismodel worden deze variabelen aangevuld met dezelfde
#' variabelen voor het Vlaams model.  Hierbij is de "d" op het einde van de
#' variabelenaam vervangen door "vl".
#'
#'
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @importFrom assertthat has_name
#'

curvekarakteristieken <- function(Basismodel, Data = NULL) {

  #controle invoer
  if (has_name(Basismodel, "DOMEIN_ID")) {
    invoercontrole(Basismodel, "lokaalmodel")
    if (is.null(Data)) {
      stop("Bij opgave van een lokaal model moet je ook de dataset meegeven")
    } else {
      if (has_name(Data, "VoorModelFit")) {
        Data <- Data %>%
          filter(.data$VoorModelFit) %>%
          select(-"VoorModelFit")
      }
      invoercontrole(Data, "fit")
    }
  } else {
    invoercontrole(Basismodel, "basismodel")
  }

  Parameters <- modelparameters(Basismodel, Data) %>%
    mutate(
      Omtrek_Extr_Hoogte.d = exp(-.data$Bd / (2 * .data$Cd)),
      Extr_Hoogte.d =
        .data$Ad + .data$Bd * log(.data$Omtrek_Extr_Hoogte.d) +
        .data$Cd * (log(.data$Omtrek_Extr_Hoogte.d)) ^ 2,
      Hoogteverschil.d =
        .data$Extr_Hoogte.d - (.data$Ad + .data$Bd * log(.data$Q95k) +
                                 .data$Cd * (log(.data$Q95k)) ^ 2),
      Omtrek_Buigpunt.d = exp(1 - .data$Bd / (2 * .data$Cd)),
      Verschil_rico_BP_Q5.d =
        (2 * .data$Cd * log(.data$Omtrek_Buigpunt.d) + .data$Bd) /
          .data$Omtrek_Buigpunt.d -
          (2 * .data$Cd * log(.data$Q5k) + .data$Bd) / .data$Q5k,
      Verschil_rico_BP_Q5_per_omtrek.d =
        .data$Verschil_rico_BP_Q5.d * (.data$Omtrek_Buigpunt.d - .data$Q5k)
    )

  if (!has_name(Basismodel, "DOMEIN_ID")) {
    Parameters <- Parameters %>%
      mutate(
        Omtrek_Extr_Hoogte.vl = exp(-.data$Bvl / (2 * .data$Cvl)),
        Extr_Hoogte.vl =
          .data$Avl + .data$Bvl * log(.data$Omtrek_Extr_Hoogte.vl) +
            .data$Cvl * (log(.data$Omtrek_Extr_Hoogte.vl)) ^ 2,
        Hoogteverschil.vl =
          .data$Extr_Hoogte.vl - (.data$Avl + .data$Bvl * log(.data$Q95k) +
                                    .data$Cvl * (log(.data$Q95k)) ^ 2),
        Omtrek_Buigpunt.vl = exp(1 - .data$Bvl / (2 * .data$Cvl)),
        Verschil_rico_BP_Q5.vl =
          (2 * .data$Cvl * log(.data$Omtrek_Buigpunt.vl) + .data$Bvl) /
            .data$Omtrek_Buigpunt.vl -
            (2 * .data$Cvl * log(.data$Q5k) + .data$Bvl) / .data$Q5k,
        Verschil_rico_BP_Q5_per_omtrek.vl =
          .data$Verschil_rico_BP_Q5.vl * (.data$Omtrek_Buigpunt.vl - .data$Q5k)
      )
  }

  return(Parameters)

}
