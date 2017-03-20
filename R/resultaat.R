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
#' @param Lokaalmodel model per boomsoort-domein-combinatie
#' @param Data.lokaal data voor model per boomsoort-domein-combinatie
#' @param Data.onbruikbaar evt. lijst met meetresultaten van domein-boomsoort-combinaties waarvoor geen model opgesteld kan worden
#'
#' @return dataframe met modellen per domein en per boomsoort met velden:
#'
#' - ModelID
#'
#' - DomeinID
#'
#' - BoomsoortID
#'
#' - Modeltype ('basismodel'(= eigen model op basis van mixed model) / ‘afgeleid model'(= verschoven Vlaams model, afgeleid van fixed factor uit basismodel) / ‘Vlaams model’(= fixed factor uit basismodel, niet toegevoegd omdat niet relevant) / 'lokaal model'(= apart model voor 1 boomsoort-domein-combinatie) / 'geen model'(= boomsoort-domein-combinatie waarvoor geen model berekend kan worden))
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
#' @importFrom dplyr %>% select_ left_join rowwise do_ ungroup rename_ mutate_ bind_rows group_by_
#'

resultaat <- function(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal, Data.onbruikbaar = NULL){

  Modellen.basis <- modelparameters(Basismodel) %>%
    select_(~-Q5k, ~-Q95k) %>%
    left_join(Basismodel %>%
                rowwise() %>%
                do_(
                  ~rmse.basis(.$Model$data, "Basis")
                ) %>%
                ungroup(),
              c("BMS","DOMEIN_ID")) %>%
    select_(~-maxResid)

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


  #Rmse van Vlaams model berekenen
  RmseVL <- Basismodel %>%
    rowwise() %>%
    do_(
      ~rmse.basis(.$Model$data, "Basis")
    ) %>%
    ungroup() %>%
    mutate_(
      sseVL = ~(rmseVL)^2 * (nBomenOmtrek05 - 2)
    ) %>%
    group_by_(~BMS) %>%
    summarise_(
      nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
      rmseVL = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
    ) %>%
    ungroup()

  #Rmse van afgeleid model berekenen en combineren met die van Vlaams model
  RmseAfg <- Afgeleidmodel[[1]] %>%
    rowwise() %>%
    do_(
      ~rmse.afgeleid(.$Model, .$BMS, .$DOMEIN_ID)
    ) %>%
    ungroup() %>%
    inner_join(
      RmseVL %>% select_(~BMS, ~rmseVL),
      by = c("BMS")
    ) %>%
    mutate_(
      rmseD = ~sqrt(rmseVL^2 + RmseVerschuiving^2)
    )


  Modellen <- Modellen.domein %>%
#    bind_rows(Modellen.Vlaams) %>%
    bind_rows(
      Afgeleidmodel[[2]] %>%
        select_(
          ~BMS,
          ~DOMEIN_ID,
          ~nBomen,
          ~nBomenInterval,
          ~nBomenOmtrek05
        ) %>%
        distinct_() %>%
        left_join(
          RmseAfg %>% select_(~BMS, ~DOMEIN_ID, ~rmseD),
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        left_join(
          modelparameters(Basismodel, Afgeleidmodel = Afgeleidmodel),
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        transmute_(
          ~DOMEIN_ID,
          ~BMS,
          A = ~Ad + Avl,
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
      modelparameters(Lokaalmodel, Data.lokaal) %>%
        select_(~-Q5k, ~-Q95k) %>%
        left_join(Data.lokaal %>%
                    group_by_(
                      ~BMS,
                      ~DOMEIN_ID
                    ) %>%
                    do_(
                      ~rmse.basis(., "Lokaal")
                    ) %>%
                    ungroup(),
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
          Modeltype = ~"lokaal model"
        )
    ) %>%
    bind_rows(
      Data.onbruikbaar %>%
        select_(
          ~DOMEIN_ID, ~BMS,
          ~nBomen, ~nBomenInterval, ~nBomenOmtrek05,
          ~Q5k, ~Q95k
        ) %>%
        distinct_() %>%
        mutate_(
          Modeltype = ~"Geen model"
        )
    )

  return(Modellen)

}
