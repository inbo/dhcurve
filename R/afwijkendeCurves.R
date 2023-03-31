#' @title Lijst de afwijkende curves op
#'
#' @description
#' Functie die op basis van de curvekarakteristieken van de modellen (extremen
#' en buigpunten) de mogelijk slechtste modellen oplijst met de reden.  Deze
#' functie kan ook gebruikt worden voor lokale modellen, in dat geval moet ook
#' de dataset als parameter meegegeven worden.
#'
#' Deze functie roept voor de berekening van de extremen en buigpunten de
#' functie `curvekarakteristieken()` op.
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' `fit.basis()` of model per boomsoort-domeincombinatie zoals teruggegeven door
#' de functie `fit.lokaal()`
#' @param Data Dataset op basis waarvan het lokaal model berekend is (enkel
#' nodig voor lokaal model)
#'
#' @return Dataframe dat de mogelijk afwijkende curves oplijst, met volgende
#' velden:
#' - `BMS`: Boomsoort
#' - `DOMEIN_ID`
#' - `Reden`: reden waarom de curve afwijkend is
#' - `Omtrek_Buigpunt.d`: midden van omtrekklasse waarin het buigpunt van
#'     de curve van het domeinmodel ligt
#' - `Omtrek_Extr_Hoogte.d`: midden van omtrekklasse waarin het maximum
#'     van de curve van het domeinmodel ligt
#'
#' @export
#'
#' @importFrom dplyr %>% filter transmute mutate bind_rows
#' @importFrom rlang .data
#' @importFrom assertthat has_name
#'

afwijkendeCurves <- function(Basismodel, Data = NULL) {

  #berekeningen
  Parameters_Extr <- curvekarakteristieken(Basismodel, Data) %>%
    filter(
      .data$Omtrek_Extr_Hoogte.d > 0.1,
      .data$Omtrek_Extr_Hoogte.d < .data$Q95k
    )

  #hoog minimum domeinmodel
  HoogMin <- Parameters_Extr %>%
    filter(
      .data$Omtrek_Extr_Hoogte.d > 0.1,
      .data$Hoogteverschil.d < 0,
      .data$Omtrek_Buigpunt.d > .data$Q5k,
      .data$Verschil_rico_BP_Q5.d > 1
    ) %>%
    transmute(
      .data$DOMEIN_ID,
      .data$BMS,
      .data$Omtrek_Buigpunt.d
    )

  #laag maximum domeinmodel
  LaagMax <- Parameters_Extr %>%
    filter(
      .data$Omtrek_Extr_Hoogte.d < .data$Q95k,
      .data$Hoogteverschil.d > 0
    ) %>%
    transmute(
      .data$DOMEIN_ID,
      .data$BMS,
      .data$Omtrek_Extr_Hoogte.d
    )


  AfwijkendeCurves <- HoogMin %>%
    mutate(
      Reden = "curvevorm hol bij lage omtrekklassen"
    ) %>%
    bind_rows(
      LaagMax %>%
        mutate(
          Reden = "curve daalt terug bij hoge omtrekklassen"
        )
    )

  return(AfwijkendeCurves)
}
