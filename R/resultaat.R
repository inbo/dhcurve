#' berekent de modelparameters per domein
#'
#' Berekeningen die leiden tot de gevraagde tabel.  Hiervoor worden volgende hulpfuncties aangeroepen:
#'
#' modelparameters
#' rsme.basis
#' fit.afgeleid
#'
#' Verder moeten parameters A, B en C uit het model gehaald worden (coef) en een aantal berekende gegevens uit de eerste stap toegevoegd worden.
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (Vlaams model)
#' @param Extramodellen model per boomsoort-domein-combinatie
#' @param Data.extra data voor model per boomsoort-domein-combinatie
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
#' - type model ('basismodel'(eigen model op basis van mixed model)/‘eigen model’ (eigen fixed model)/‘afgeleid model' (= verschoven Vlaams model, afgeleid van fixed factor uit basismodel)/‘Vlaams model’ (= fixed factor uit basismodel)/ 'domeinmodel' (= apart model voor 1 boomsoort-domein-combinatie))
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
#' @export
#'
#' @importFrom dplyr %>% select_ left_join rename_ mutate_ group_by summarise_ ungroup
#'

resultaat <- function(Basismodel, Afgeleidmodel, Extramodellen, Data.extra, Data.ontbrekend = NULL){

  Modellen.basis <- modelparameters(Basismodel) %>%
    select_(~-Q5k, ~-Q95k) %>%
    left_join(rmse.basis(Basismodel),
              c("BMS","DOMEIN_ID"))

  Modellen.domein <- Modellen.basis %>%
    select_(~-Avl, ~-Bvl, ~-Cvl, ~-rmseVL) %>%
    rename_(
      A = ~Ad,
      B = ~Bd,
      C = ~Cd,
      RMSE = ~rmseD
    ) %>%
    mutate_(
      Modeltype = ~"basismodel"
    )

  # volgende code is om ook de Vlaamse modellen toe te voegen aan de resultatenlijst.  Omdat het niet wenselijk is om deze te gebruiken (als een Vlaams model gebaseerd is op enkel bomen van 1 streek en de te schatten boom ligt in een andere streek, is de schatting onbetrouwbaar), voegen we het niet toe

  # Modellen.Vlaams <- Modellen.basis %>%
  #   select_(~-Ad, ~-Bd, ~-Cd, ~-rmseD) %>%
  #   mutate_(
  #     sseVL = ~(rmseVL)^2 * (nBomenOmtrek05 - 2)
  #   ) %>%
  #   group_by_(~BMS, ~Avl, ~Bvl, ~Cvl) %>%
  #   summarise_(
  #     nBomen = ~sum(nBomen),
  #     nBomenInterval = ~sum(nBomenInterval),
  #     nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
  #     RMSE = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
  #   ) %>%
  #   ungroup() %>%
  #   rename_(
  #     A = ~Avl,
  #     B = ~Bvl,
  #     C = ~Cvl,
  #     nBomenOmtrek05 = ~nBomenOmtrek05VL
  #   ) %>%
  #   mutate_(
  #     Modeltype = ~"Vlaams model"
  #   )

  Modellen <- Modellen.domein %>%
#    bind_rows(Modellen.Vlaams) %>%
    bind_rows(
      Afgeleidmodel %>%
        select_(
          ~DOMEIN_ID,
          ~BMS,
          A = ~Ad,
          B = ~Bvl,
          C = ~Cvl,
          ~nBomen,
          ~nBomenInterval,
          ~nBomenOmtrek05,
          ~Q5k,
          ~Q95k,
          RMSE = ~rmseD
        ) %>%
        mutate_(
          Modeltype = ~"afgeleid model"
        )
    ) %>%
    bind_rows(
      modelparameters(Extramodellen, Data.extra) %>%
        left_join(rmse.basis(Extramodellen, Data.extra),
                  c("BMS","DOMEIN_ID")) %>%
        select_(
          ~DOMEIN_ID,
          ~BMS,
          A = ~Ad,
          B = ~Bd,
          C = ~Cd,
          ~nBomen,
          ~nBomenInterval,
          ~nBomenOmtrek05,
          ~Q5k,
          ~Q95k,
          RMSE = ~rmseD
        ) %>%
        mutate_(
          Modeltype = ~"domeinmodel"
        )
    )

  return(Modellen)

}
