#' Geeft de afwijkende metingen uit een gegeven model
#'
#' Functie die de curvekarakteristieken van de modellen berekent (roept hiervoor de functie curvekarakteristieken op) en op basis van deze parameters de mogelijk slechtste modellen oplijst met de reden.
#'
#' Deze functie kan ook gebruikt worden voor lokale modellen
#'
#' @param Basismodel model per boomsoort
#' @param Data dataset op basis waarvan het model berekend is (nodig voor lokaal model)
#'
#' @return Dataframe met te controleren curves (incl. aantal metingen per curve)
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ transmute_ mutate_ bind_rows
#' @importFrom assertthat has_name
#'

afwijkendeCurves <- function(Basismodel, Data = NULL) {

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

  #berekeningen
  Parameters_Extr <- curvekarakteristieken(Basismodel, Data) %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d > 0.1,
      ~Omtrek_Extr_Hoogte.d < Q95k
    )

  #hoog minimum domeinmodel
  HoogMin <- Parameters_Extr %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d > 0.1,
      ~Hoogteverschil.d < 0,
      ~Omtrek_Buigpunt.d > Q5k,
      ~Verschil_rico_BP_Q5.d > 1
    ) %>%
    transmute_(
      ~DOMEIN_ID,
      ~BMS,
      ~Omtrek_Buigpunt.d
    )

  #laag maximum domeinmodel
  LaagMax <- Parameters_Extr %>%
    filter_(
      ~Omtrek_Extr_Hoogte.d < Q95k,
      ~Hoogteverschil.d > 0
    ) %>%
    transmute_(
      ~DOMEIN_ID,
      ~BMS,
      ~Omtrek_Extr_Hoogte.d
    )


  AfwijkendeCurves <- HoogMin %>%
    mutate_(
      Reden = ~"curvevorm hol bij lage omtrekklassen"
    ) %>%
    bind_rows(
      LaagMax %>%
        mutate_(
          Reden = ~"curve daalt terug bij hoge omtrekklassen"
        )
    )

  return(AfwijkendeCurves)
}
