#' berekent de geschatte hoogtes per omtrekklasse
#'
#' Berekeningen die leiden tot de gevraagde tabel om in IVANHO te importeren.  Hiervoor worden volgende hulpfuncties aangeroepen:
#'
#' hoogteschatting.basis, hoogteschatting.afgeleid
#'
#' Verder moeten parameters A, B en C uit het model gehaald worden (coef) en een aantal berekende gegevens uit de eerste stap toegevoegd worden.
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (Vlaams model)
#' @param Lokaalmodel model per boomsoort-domein-combinatie
#' @param Data.lokaal data voor model per boomsoort-domein-combinatie
#'
#' @return dataframe met geschatte hoogtes per domein en per boomsoort met velden:
#'
#' - ModelID
#'
#' - DOMEIN_ID: domeincode
#'
#' - BOS_BHI: domeinnaam
#'
#' - BoomsoortID
#'
#' - BMS: boomsoort
#'
#' - Modeltype ('basismodel'(= eigen model op basis van mixed model) / ‘afgeleid model'(= verschoven Vlaams model, afgeleid van fixed factor uit basismodel) / ‘Vlaams model’(= fixed factor uit basismodel, niet toegevoegd omdat niet relevant) / 'lokaal model'(= apart model voor 1 boomsoort-domein-combinatie) / 'geen model'(= boomsoort-domein-combinatie waarvoor geen model berekend kan worden))
#'
#' - Omtrek: klassemidden
#'
#' - OmtrekklassetypeID
#'
#' - Omtrekklasse
#'
#' - Hoogte
#'
#' @export
#'
#' @importFrom dplyr %>% select_ rowwise do_ ungroup mutate_ bind_rows group_by_ transmute_ distinct_ inner_join
#'

outputIVANHO <-
  function(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal) {

  Hoogteschatting <- Basismodel %>%
    rowwise() %>%
    do_(
      ~hoogteschatting.basis(.$Model, .$Model$data, "Basis")
    ) %>%
    ungroup() %>%
    mutate_(
      Modeltype = ~"basismodel"
    ) %>%
    bind_rows(
      Afgeleidmodel[[1]] %>%
        inner_join(
          Afgeleidmodel[[2]],
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        group_by_(
          ~BMS,
          ~DOMEIN_ID
        ) %>%
        do_(
          ~hoogteschatting.afgeleid(.$Model[[1]],
                                    select_(., ~-Model))
        ) %>%
        ungroup() %>%
        mutate_(
          Modeltype = ~"afgeleid model"
        )
    ) %>%
    bind_rows(
      Lokaalmodel %>%
        inner_join(
          Data.lokaal,
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        group_by_(
          ~BMS,
          ~DOMEIN_ID
        ) %>%
        do_(
          ~hoogteschatting.basis(.$Model[[1]],
                                 select_(., ~-Model),
                                 "Lokaal")
        ) %>%
        ungroup() %>%
        mutate_(
          Modeltype = ~"lokaal model"
        )
    ) %>%
    transmute_(
      ~BMS,
      ~DOMEIN_ID,
      ~BOS_BHI,
      ~Omtrek,
      OmtrekklassetypeID = ~as.integer(Omtrek * 10 + 1.5),
      Omtrekklasse = ~paste(Omtrek * 100 - 5, Omtrek * 100 + 5, sep = " - "),
      Hoogte = ~H_D_finaal,
      ~Modeltype
    ) %>%
    distinct_()

  return(Hoogteschatting)
}
