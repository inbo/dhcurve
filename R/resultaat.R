#' @title Berekent de modelparameters per domein
#'
#' @description
#' De functie resultaat berekent op basis van de opgegeven modellen voor elke
#' boomsoort-domeincombinatie de modelparameters A, B en C voor een model van
#' de vorm
#' \eqn{Hoogte \sim A + B\log(Omtrek) + C\log(Omtrek)^2}{Hoogte ~ A + B.log(Omtrek) + C.log(Omtrek)^2}.
#'
#' Voor deze functie worden volgende hulpfuncties aangeroepen:
#' `modelparameters()`, `rsme.basis()` en `rsme.afgeleid()`
#'
#' Verder worden parameters `A`, `B` en `C` uit het model gehaald en een
#' aantal in de functie `initiatie()` berekende gegevens toegevoegd.
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' `fit.basis()`: tibble met de velden `BMS` (boomsoort) en `Model`
#' (`lme`-object met het gefit mixed model voor die boomsoort).
#' @param Afgeleidmodel Model per boomsoort-domeincombinatie zoals teruggegeven
#' door de functie `fit.afgeleid()`: list met 2 tibbles.
#' @param Lokaalmodel Model per boomsoort-domeincombinatie zoals teruggegeven
#' door de functie `fit.lokaal()`: tibble met de velden `BMS` (boomsoort),
#' `DOMEIN_ID` en `Model` (`lm`-object met het gefit lineair model voor die
#' boomsoort-domeincombinatie).
#' @param Data.lokaal Dataset op basis waarvan het opgegeven lokaal model
#' berekend is.
#' @param Data.onbruikbaar Evt. lijst met meetresultaten van
#' boomsoort-domeincombinaties waarvoor geen model opgesteld kan worden, in de
#' vorm van een dataframe zoals de dataframe `Rest` uit de list die door de
#' functie `initiatie()` teruggegeven wordt.
#'
#' @return Dataframe met modellen per domein en per boomsoort met velden:
#' - `DomeinID` (identificatienummer voor domein)
#' - `BMS` (boomsoort)
#' - `Modeltype` ("basismodel"(= eigen model op basis van mixed model) of
#'     "afgeleid model"(= verschoven Vlaams model, afgeleid van fixed factor uit
#'     basismodel) of
#'     "Vlaams model"(= fixed factor uit basismodel, niet toegevoegd
#'     omdat niet relevant) of "lokaal model"(= eigen model voor 1
#'     boomsoort-domeincombinatie) of "geen model"(= boomsoort-domeincombinatie
#'     waarvoor minstens 1 boom opgemeten is maar geen model berekend kan
#'     worden))
#' - parameters `A`, `B` en `C` (zie 'Description' bovenaan)
#' - bruikbaar interval (`Q5k` en `Q95k`, zie vignet voor meer info)
#' - `RMSE` (root mean square error, zie vignet voor meer info)
#' - `nBomen` (totaal aantal opgemeten bomen met omtrek tussen 0,2 en
#'     3,0 m)
#' - `nBomenOmtrek05` (aantal metingen met omtrek tussen 0.5 en 2,8 m,
#'     dus waarop afgeleid model gebaseerd is)
#' - `nBomenInterval` (aantal metingen binnen bruikbaar interval, dus
#'     waarop basismodel of lokaal model gebaseerd is)
#' - `nBomenIntervalOmtrek05` (aantal metingen binnen bruikbaar interval
#'     met omtrek > 0.5 m, dus waarop RMSE-berekening gebaseerd is)
#' - `nExtra` (aantal metingen boven bruikbaar interval waarop een eventuele
#'     uitbreiding gebaseerd is)
#'
#' evt. kan een tweede dataframe toegevoegd worden met Vlaamse modellen per
#' boomsoort, of deze kan toegevoegd worden aan de vorige dataframe, waarbij
#' `DomeinID` leeg gelaten wordt of een specifieke waarde
#' "Vlaams model" krijgt
#'
#' @export
#'
#' @importFrom dplyr %>% select left_join rowwise do ungroup rename mutate
#' bind_rows group_by filter
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat has_name
#'

resultaat <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL, Data.onbruikbaar = NULL) {

  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")
    Modellen.basis <- modelparameters(Basismodel) %>%
      select(-"Q5k", -"Q95k") %>%
      left_join(Basismodel %>%
                  rowwise() %>%
                  do(
                    rmse.basis(.$Model$data, "Basis", .$BMS)
                  ) %>%
                  ungroup(),
                c("BMS", "DOMEIN_ID")) %>%
      select(-"maxResid")

    Modellen <- Modellen.basis %>%
      select(-"Avl", -"Bvl", -"Cvl", -"rmseVL") %>%
      rename(
        A = "Ad",
        B = "Bd",
        C = "Cd",
        RMSE = "rmseD"
      ) %>%
      mutate(
        Modeltype = "basismodel"
      ) %>%
      left_join(
        Basismodel %>%
          rowwise() %>%
          do(
            merge(
              .$BMS,
              (.$Model$data %>%
                 select("DOMEIN_ID", "nExtra") %>%
                 distinct()
              )
            )
          ) %>%
          transmute(
            BMS = .data$x,
            .data$DOMEIN_ID, .data$nExtra
          ),
        by = c("BMS", "DOMEIN_ID")
      )

    # volgende code is om ook de Vlaamse modellen toe te voegen aan de
    # resultatenlijst.  Omdat het niet wenselijk is om deze te gebruiken (als
    # een Vlaams model gebaseerd is op enkel bomen van 1 streek en de te
    # schatten boom ligt in een andere streek, is de schatting onbetrouwbaar),
    # voegen we het niet toe

    # Modellen.Vlaams <- Modellen.basis %>%
    #   select(-"Ad", -"Bd", -"Cd", -"rmseD") %>%
    #   mutate(
    #     sseVL = (.data$rmseVL)^2 * (.data$nBomenIntervalOmtrek05 - 2) #nolint: commented_code_linter, line_length_linter
    #   ) %>%
    #   group_by(.data$BMS, .data$Avl, .data$Bvl, .data$Cvl) %>%
    #   summarise(
    #     nBomen = sum(.data$nBomen),  #nolint: commented_code_linter
    #     nBomenInterval = sum(.data$nBomenInterval),  #nolint: commented_code_linter, line_length_linter
    #     nBomenIntervalOmtrek05VL = sum(.data$nBomenIntervalOmtrek05),  #nolint: commented_code_linter, line_length_linter
    #     RMSE = sqrt(sum(.data$sseVL) / (.data$nBomenIntervalOmtrek05VL - 2))   #nolint: commented_code_linter, line_length_linter
    #   ) %>%
    #   ungroup() %>%
    #   rename(
    #     A = .data$Avl,  #nolint: commented_code_linter
    #     B = .data$Bvl,  #nolint: commented_code_linter
    #     C = .data$Cvl,  #nolint: commented_code_linter
    #     nBomenIntervalOmtrek05 = .data$nBomenIntervalOmtrek05VL    #nolint: commented_code_linter, line_length_linter
    #   ) %>%
    #   mutate(
    #     Modeltype = "Vlaams model"     #nolint:  commented_code_linter
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
          sseVL = (.data$rmseVL) ^ 2 * (.data$nBomenIntervalOmtrek05 - 2)
        ) %>%
        group_by(.data$BMS) %>%
        summarise(
          nBomenIntervalOmtrek05VL = sum(.data$nBomenIntervalOmtrek05),
          rmseVL = sqrt(sum(.data$sseVL) / (.data$nBomenIntervalOmtrek05VL - 2))
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
          RmseVL %>% select("BMS", "rmseVL"),
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
              "BMS",
              "DOMEIN_ID",
              "nBomen",
              "nBomenOmtrek05",
              "nBomenInterval",
              "nBomenIntervalOmtrek05"
            ) %>%
            distinct() %>%
            left_join(
              RmseAfg %>% select("BMS", "DOMEIN_ID", "rmseD"),
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            left_join(
              modelparameters(Basismodel, Afgeleidmodel = Afgeleidmodel),
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            transmute(
              .data$DOMEIN_ID,
              .data$BMS,
              A = .data$Ad + .data$Avl,
              B = .data$Bvl,
              C = .data$Cvl,
              .data$nBomen,
              .data$nBomenOmtrek05,
              .data$nBomenInterval,
              .data$nBomenIntervalOmtrek05,
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
      if (has_name(Data.lokaal, "VoorModelFit")) {
        Data.lokaal <- Data.lokaal %>%
          filter(.data$VoorModelFit) %>%
          select(-"VoorModelFit")
      }
      invoercontrole(Data.lokaal, "fit")
    }

    Modellen.lokaal <-
      modelparameters(Lokaalmodel, Data.lokaal) %>%
      select(-"Q5k", -"Q95k") %>%
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
        "DOMEIN_ID",
        "BMS",
        A = "Ad",
        B = "Bd",
        C = "Cd",
        "nBomen",
        "nBomenOmtrek05",
        "nBomenInterval",
        "nBomenIntervalOmtrek05",
        "Q5k",
        "Q95k",
        RMSE = "rmseD"
      ) %>%
      mutate(
        Modeltype = "lokaal model"
      ) %>%
      left_join(
        Data.lokaal %>%
          select("DOMEIN_ID", "BMS", "nExtra") %>%
          distinct(),
        by = c("DOMEIN_ID", "BMS")
      )

    if (exists("Modellen") && !is.null(Basismodel)) {
      Modellen <- Modellen %>%
        bind_rows(Modellen.lokaal)
    } else {
      Modellen <- Modellen.lokaal
    }
  }

  if (!is.null(Data.onbruikbaar)) {
    invoercontrole(Data.onbruikbaar, "fit", Uitbreiding = TRUE)

    Lijst.onbruikbaar <- Data.onbruikbaar %>%
      select(
        "DOMEIN_ID", "BMS",
        "nBomen", "nBomenOmtrek05", "nBomenInterval", "nBomenIntervalOmtrek05",
        "Q5k", "Q95k"
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
