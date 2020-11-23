#' Berekent de modelparameters per domein
#'
#' De functie resultaat berekent op basis van de opgegeven modellen voor elke
#' boomsoort-domeincombinatie de modelparameters A, B en C voor een model van
#' de vorm \eqn{Hoogte \sim A + B\log(Omtrek) + C\log(Omtrek)^2}{Hoogte ~ A +
#' B.log(Omtrek) + C.log(Omtrek)^2}.
#'
#' Voor deze functie worden volgende hulpfuncties aangeroepen: modelparameters,
#' rsme.basis en rsme.afgeleid
#'
#' Verder worden parameters A, B en C uit het model gehaald (coef) en een
#' aantal in de functie initiatie berekende gegevens toegevoegd .
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' fit.basis: tibble met de velden BMS (boomsoort) en Model (lme-object met het
#' gefit mixed model voor die boomsoort).
#' @param Afgeleidmodel Model per domein-boomsoortcombinatie zoals teruggegeven
#' door de functie fit.afgeleid: list met 2 tibbles.
#' @param Lokaalmodel Model per boomsoort-domeincombinatie zoals teruggegeven
#' door de functie fit.lokaal: tibble met de velden BMS (boomsoort), DOMEIN_ID
#' en Model (lm-object met het gefit lineair model voor die
#' boomsoort-domeincombinatie).
#' @param Data.lokaal Dataset op basis waarvan het opgegeven lokaal model
#' berekend is.
#' @param Data.onbruikbaar Evt. lijst met meetresultaten van
#' domein-boomsoort-combinaties waarvoor geen model opgesteld kan worden, in de
#' vorm van een dataframe zoals de dataframe "Rest" uit de list die door de
#' funtie initiatie teruggegeven wordt.
#'
#' @return Dataframe met modellen per domein en per boomsoort met velden:
#'
#' - DomeinID (identificatienummer voor domein)
#'
#' - BMS (boomsoort)
#'
#' - Modeltype ('basismodel'(= eigen model op basis van mixed model) /
#' ‘afgeleid model'(= verschoven Vlaams model, afgeleid van fixed factor uit
#' basismodel) /
#' ‘Vlaams model’(= fixed factor uit basismodel, niet toegevoegd
#' omdat niet relevant) / 'lokaal model'(= eigen model voor 1
#' boomsoort-domeincombinatie) / 'geen model'(= boomsoort-domeincombinatie
#' waarvoor minstens 1 boom opgemeten is maar geen model berekend kan worden))
#'
#' - paramaters A, B en C (zie description)
#'
#' - bruikbaar interval (Q5k en Q95k, zie vignette voor meer info)
#'
#' - RMSE (root mean square error, zie vignette voor meer info)
#'
#' - nBomen (totaal aantal opgemeten bomen met omtrek tussen 0,2 en 2,4 m)
#'
#' - nBomenInterval (aantal metingen waarop model gebaseerd is)
#'
#' - nBomenOmtrek05 (aantal metingen > 0.5 m, dus waarop rmse-berekening
#' gebaseerd is)
#'
#' evt. kan een tweede dataframe toegevoegd worden met Vlaamse modellen per
#' boomsoort, of deze kan toegevoegd worden aan de vorige dataframe, waarbij
#' DomeinID leeg gelaten wordt of een specifieke waarde
#' ‘Vlaams model’ krijgt
#'
#' @export
#'
#' @importFrom dplyr %>% select left_join rowwise do ungroup rename mutate
#' bind_rows group_by
#' @importFrom plyr .
#' @importFrom rlang .data
#'

resultaat <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL, Data.onbruikbaar = NULL) {

  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")
    Modellen.basis <- modelparameters(Basismodel) %>%
    select(
      -.data$BOS_BHI, -.data$nBomenInterval, -.data$nBomenOmtrek05,
      -.data$Q5k, -.data$Q95k
    ) %>%
    left_join(Basismodel %>%
                rowwise() %>%
                do(
                  rmse.basis(.$Model$data, "Basis", .$BMS)
                ) %>%
                ungroup(),
              c("BMS", "DOMEIN_ID")) %>%
    select(-.data$maxResid)

    Modellen <- Modellen.basis %>%
      select(-.data$Avl, -.data$Bvl, -.data$Cvl, -.data$rmseVL) %>%
      rename(
        A = .data$Ad,
        B = .data$Bd,
        C = .data$Cd,
        RMSE = .data$rmseD
      ) %>%
      mutate(
        Modeltype = "basismodel"
      )

    # volgende code is om ook de Vlaamse modellen toe te voegen aan de
    # resultatenlijst.  Omdat het niet wenselijk is om deze te gebruiken (als
    # een Vlaams model gebaseerd is op enkel bomen van 1 streek en de te
    # schatten boom ligt in een andere streek, is de schatting onbetrouwbaar),
    # voegen we het niet toe

    # Modellen.Vlaams <- Modellen.basis %>%
    #   select(-.data$Ad, -.data$Bd, -.data$Cd, -.data$rmseD) %>%
    #   mutate(
    #     sseVL = (.data$rmseVL)^2 * (.data$nBomenOmtrek05 - 2)    #nolint
    #   ) %>%
    #   group_by(.data$BMS, .data$Avl, .data$Bvl, .data$Cvl) %>%
    #   summarise(
    #     nBomen = sum(.data$nBomen),
    #     nBomenInterval = sum(.data$nBomenInterval),
    #     nBomenOmtrek05VL = sum(.data$nBomenOmtrek05),
    #     RMSE = sqrt(sum(.data$sseVL) / (.data$nBomenOmtrek05VL - 2))   #nolint
    #   ) %>%
    #   ungroup() %>%
    #   rename(
    #     A = .data$Avl,
    #     B = .data$Bvl,
    #     C = .data$Cvl,
    #     nBomenOmtrek05 = .data$nBomenOmtrek05VL    #nolint
    #   ) %>%
    #   mutate(
    #     Modeltype = "Vlaams model"     #nolint
    #   )

    if (!is.null(Afgeleidmodel)) {
      invoercontrole(Afgeleidmodel, "afgeleidmodel")

      #Rmse van Vlaams model berekenen
      RmseVL <- Basismodel %>%
        rowwise() %>%
        do(
          rmse.basis(.$Model$data, "Basis", .$BMS)
        ) %>%
        ungroup() %>%
        mutate(
          sseVL = (.data$rmseVL) ^ 2 * (.data$nBomenOmtrek05 - 2)
        ) %>%
        group_by(.data$BMS) %>%
        summarise(
          nBomenOmtrek05VL = sum(.data$nBomenOmtrek05),
          rmseVL = sqrt(sum(.data$sseVL) / (.data$nBomenOmtrek05VL - 2))
        ) %>%
        ungroup()

      #Rmse van verschuiving berekenen en combineren met die van Vlaams model
      RmseAfg <- Afgeleidmodel[[1]] %>%
        rowwise() %>%
        do(
          rmse.verschuiving(.$Model, .$BMS, .$DOMEIN_ID)
        ) %>%
        ungroup() %>%
        inner_join(
          RmseVL %>% select(.data$BMS, .data$rmseVL),
          by = c("BMS")
        ) %>%
        mutate(
          rmseD = sqrt(.data$rmseVL ^ 2 + .data$RmseVerschuiving ^ 2)
        )


      Modellen <- Modellen %>%
    #    bind_rows(Modellen.Vlaams) %>%
        bind_rows(
          Afgeleidmodel[[2]] %>%
            select(
              .data$BMS,
              .data$DOMEIN_ID,
              .data$nBomen,
              .data$nBomenInterval,
              .data$nBomenOmtrek05
            ) %>%
            distinct() %>%
            left_join(
              RmseAfg %>% select(.data$BMS, .data$DOMEIN_ID, .data$rmseD),
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            left_join(
              modelparameters(Basismodel, Afgeleidmodel = Afgeleidmodel) %>%
                select(
                  -.data$BOS_BHI, -.data$nBomenInterval, -.data$nBomenOmtrek05
                ),
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            transmute(
              .data$DOMEIN_ID,
              .data$BMS,
              A = .data$Ad + .data$Avl,
              B = .data$Bvl,
              C = .data$Cvl,
              .data$nBomen,
              .data$nBomenInterval,
              .data$nBomenOmtrek05,
              .data$Q5k,
              .data$Q95k,
              RMSE = .data$rmseD
            ) %>%
            mutate(
              Modeltype = "afgeleid model"
            )
        )
    }

  } else {
    if (!is.null(Afgeleidmodel)) {
      stop("Als je een afgeleid model opgeeft, moet je ook het basismodel
           opgeven waarvan dit afgeleid is.")
    }
  }

  if (!is.null(Lokaalmodel)) {
    invoercontrole(Lokaalmodel, "lokaalmodel")
    if (is.null(Data.lokaal)) {
      stop("Bij opgave van een lokaal model moet je ook de dataset meegeven")
    } else {
      invoercontrole(Data.lokaal, "fit")
    }

    Modellen.lokaal <-
      modelparameters(Lokaalmodel, Data.lokaal) %>%
      select(
        -.data$BOS_BHI, -.data$nBomenInterval, -.data$nBomenOmtrek05,
        -.data$Q5k, -.data$Q95k
      ) %>%
      left_join(Data.lokaal %>%
                  group_by(
                    .data$BMS,
                    .data$DOMEIN_ID
                  ) %>%
                  do(
                    rmse.basis(., "Lokaal", .data$BMS)
                  ) %>%
                  ungroup(),
                c("BMS", "DOMEIN_ID")) %>%
      select(
        .data$DOMEIN_ID,
        .data$BMS,
        A = .data$Ad,
        B = .data$Bd,
        C = .data$Cd,
        .data$nBomen,
        .data$nBomenInterval,
        .data$nBomenOmtrek05,
        .data$Q5k,
        .data$Q95k,
        RMSE = .data$rmseD
      ) %>%
      mutate(
        Modeltype = "lokaal model"
      )

    if (exists("Modellen") & !is.null(Basismodel)) {
      Modellen <- Modellen %>%
        bind_rows(Modellen.lokaal)
    } else {
      Modellen <- Modellen.lokaal
    }
  }

  if (!is.null(Data.onbruikbaar)) {
    invoercontrole(Data.onbruikbaar, "fit")

    Lijst.onbruikbaar <- Data.onbruikbaar %>%
      select(
        .data$DOMEIN_ID, .data$BMS,
        .data$nBomen, .data$nBomenInterval, .data$nBomenOmtrek05,
        .data$Q5k, .data$Q95k
        ) %>%
        distinct() %>%
        mutate(
          Modeltype = "Geen model"
        )

    if (exists("Modellen")) {
      Modellen <- Modellen %>%
        bind_rows(Lijst.onbruikbaar)
    } else {
      Modellen <- Lijst.onbruikbaar
    }
  }

  if (exists("Modellen")) {
    return(Modellen)
  } else {
    message("Er zijn geen modellen opgegeven.")
  }


}
