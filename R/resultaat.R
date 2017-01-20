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
#' @export
#'
#' @importFrom dplyr %>% select_ left_join rename_ mutate_ group_by summarise_ ungroup
#'

resultaat <- function(Basismodel, Afgeleidmodel, Data.ontbrekend = NULL){

  Modellen.basis <- modelparameters(Basismodel) %>%
    select_(~-Q5, ~-Q95) %>%
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
      Modeltype = ~"eigen model"
    )

  Modellen.Vlaams <- Modellen.basis %>%
    select_(~-Ad, ~-Bd, ~-Cd, ~-rmseD) %>%
    mutate_(
      sseVL = ~(rmseVL)^2 * (nBomenOmtrek05 - 2)
    ) %>%
    group_by_(~BMS, ~Avl, ~Bvl, ~Cvl) %>%
    summarise_(
      nBomen = ~sum(nBomen),
      nBomenInterval = ~sum(nBomenInterval),
      nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
      RMSE = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
    ) %>%
    ungroup() %>%
    rename_(
      A = ~Avl,
      B = ~Bvl,
      C = ~Cvl,
      nBomenOmtrek05 = ~nBomenOmtrek05VL
    ) %>%
    mutate_(
      Modeltype = ~"Vlaams model"
    )

  Modellen <- Modellen.domein %>%
    bind_rows(Modellen.Vlaams) %>%
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
          ~Q5,
          ~Q95,
          RMSE = ~rmseD
        ) %>%
        mutate_(
          Modeltype = ~"afgeleid model"
        )
    )

  return(Modellen)

}
