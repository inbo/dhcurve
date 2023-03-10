#' @title Berekent de geschatte hoogtes per omtrekklasse
#'
#' @description
#' De functie `outputIVANHO()` voert de berekeningen uit die leiden tot de
#' gevraagde tabel om in `IVANHO` te importeren: een tabel met per boomsoort,
#' domein en omtrekklasse een schatting van de hoogte.  Als de curve een
#' maximum hoogte vertoont binnen het bestudeerde interval, wordt deze
#' maximumwaarde als hoogte meegegeven aan alle omtrekklassen hoger dan de
#' omtrekklasse van dit maximum.  (Dus verschillend van de validatierapporten
#' daalt de hoogte hier niet terug na het maximum, maar de hoogste waarde wordt
#' aangehouden.)
#' Als de curve een minimum hoogte vertoont binnen het bestudeerde interval,
#' wordt deze minimumwaarde als hoogte meegegeven aan alle omtrekklassen lager
#' dan de omtrekklasse van dit minimum.
#' Als voor een bepaalde omtrekklasse de hoogteschatting lager is dan 2,5 m,
#' dan wordt deze waarde vervangen door 2,5 m.
#'
#' Voor deze functie worden volgende hulpfuncties aangeroepen:
#' `hoogteschatting.basis()`, `hoogteschatting.afgeleid()` en
#' `curvekarakteristieken()`.
#'
#' @param Uitbreiding Dataset met velden `BMS`, `DOMEIN_ID` en `MaxOmtrek`
#' die teruggegeven wordt door functie `validatie.uitbreiding()`.
#' Als zowel een Basismodel en als Lokaal model opgegeven worden, voeg dan de
#' uitvoer van `validatie.uitbreiding()` van beide modeltypen samen.
#' @inheritParams resultaat
#'
#' @return Dataframe met geschatte hoogtes per domein en per boomsoort met
#' velden:
#' - `DOMEIN_ID`: domeincode
#' - `BOS_BHI`: domeinnaam
#' - `IDbms` (identificatienummer van de boomsoort)
#' - `BMS`: boomsoort
#' - `Modeltype` ("basismodel"(= eigen model op basis van mixed model) of
#'     "afgeleid model"(= verschoven Vlaams model, afgeleid van fixed factor uit
#'     basismodel) of
#'     "Vlaams model"(= fixed factor uit basismodel, niet toegevoegd
#'     omdat niet relevant) of "lokaal model"(= apart model voor 1
#'     boomsoort-domeincombinatie) of "geen model"(= boomsoort-domeincombinatie
#'     waarvoor geen model berekend kan worden))
#' - `Omtrek`: klassenmidden van omtrekklasse
#' - `OmtrekklassetypeID`: het overeenkomstige identificatienummer van de
#'     omtrekklasse
#' - `Omtrekklasse`
#' - `Hoogte`: de geschatte hoogte
#' - `RMSE` (root mean square error, zie vignet voor meer info:
#'     `vignette("Handleiding", package = "dhcurve")`)
#'
#' @export
#'
#' @importFrom dplyr %>% select filter rowwise do ungroup mutate bind_rows
#' group_by transmute distinct inner_join left_join summarise
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name
#'

outputIVANHO <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL, Uitbreiding = NULL) {

  if (!is.null(Uitbreiding)) {
    assert_that(is.data.frame(Uitbreiding))
    assert_that(has_name(Uitbreiding, "BMS"))
    assert_that(has_name(Uitbreiding, "DOMEIN_ID"))
    assert_that(has_name(Uitbreiding, "MaxOmtrek"))
    assert_that(
      inherits(
        Uitbreiding$MaxOmtrek, c("integer", "numeric")),
      msg = "Elke waarde van MaxOmtrek in de dataframe Uitbreiding moet een getal zijn" #nolint: line_length_linter
    )
    if (inherits(Uitbreiding$MaxOmtrek, "numeric")) {
      assert_that(
        all(
          round(
            Uitbreiding$MaxOmtrek * 10 - (Uitbreiding$MaxOmtrek * 100) %/% 10,
            2
          ) == 0.5),
        msg = "Elke waarde van MaxOmtrek in de dataframe Uitbreiding moet een klassemidden zijn van een omtrekklasse" #nolint: line_length_linter
      )
    }
  }

  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")

    #maxima binnen interval opzoeken om achteraf deze hoogte toe te kennen aan
    #lagere of hogere omtrekklassen
    MaxCurveBasis <- curvekarakteristieken(Basismodel) %>%
      filter(
        .data$Omtrek_Extr_Hoogte.d > 0.1,
        .data$Omtrek_Extr_Hoogte.d < .data$Q95k
      ) %>%
      transmute(
        .data$DOMEIN_ID,
        .data$BMS,
        .data$Omtrek_Extr_Hoogte.d,
        .data$Extr_Hoogte.d,
        Maximum = .data$Hoogteverschil.d > 0   #is het een hoog maximum?
      )

    #rsme_basis berekenen
    rmse_basis <- Basismodel %>%
      rowwise() %>%
      do(
        rmse.basis(.$Model$data, "Basis", .$BMS)
      ) %>%
      ungroup()

    #hoogtes van basismodel schatten
    if (is.null(Uitbreiding)) {
      Hoogteschatting <- Basismodel %>%
        rowwise() %>%
        do(
          hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
        ) %>%
        ungroup()
    } else {
      extractData <- function(BMS, Data) {
        Data %>%
          mutate(BMS = BMS)
      }
      Hoogteschatting <- Basismodel %>%
        rowwise() %>%
        do(
          extractData(.$BMS, .$Model$data)
        ) %>%
        left_join(
          Basismodel,
          by = c("BMS")
        ) %>%
        left_join(
          Uitbreiding,
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        mutate(
          Q95k = ifelse(is.na(.data$MaxOmtrek), .data$Q95k, .data$MaxOmtrek)
        ) %>%
        group_by(.data$BMS) %>%
        do(
          hoogteschatting.basis(.$Model[[1]], select(., -"Model"),
            "Basis", unique(.$BMS), Uitbreiding = TRUE)
        ) %>%
        ungroup()
    }

    Hoogteschatting <- Hoogteschatting %>%
      mutate(
        Modeltype = "basismodel"
      ) %>%
      left_join(
        MaxCurveBasis,
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      left_join(
        rmse_basis %>%
          transmute(
            .data$BMS, .data$DOMEIN_ID,
            RMSE = .data$rmseD
          ),
        c("BMS", "DOMEIN_ID")
      )

    if (!is.null(Afgeleidmodel)) {
      invoercontrole(Afgeleidmodel, "afgeleidmodel")

      #Rmse van Vlaams model berekenen
      RmseVL <- rmse_basis %>%
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
        transmute(
          .data$BMS, .data$DOMEIN_ID,
          RMSE = sqrt(.data$rmseVL ^ 2 + .data$RmseVerschuiving ^ 2)
        )

      Hoogteschatting <- Hoogteschatting %>%
        bind_rows(
          Afgeleidmodel[[2]] %>%
            group_by(.data$BMS) %>%
            mutate(
              IDbms = max(.data$IDbms, na.rm = TRUE)
            ) %>%
            ungroup() %>%
            inner_join(
              Afgeleidmodel[[1]],
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            group_by(
              .data$BMS,
              .data$DOMEIN_ID
            ) %>%
            do(
              hoogteschatting.afgeleid(.$Model[[1]],
                                       select(., -"Model"))
            ) %>%
            ungroup() %>%
            mutate(
              Modeltype = "afgeleid model"
            ) %>%
            left_join(RmseAfg, by = c("BMS", "DOMEIN_ID"))
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

    #maxima binnen interval opzoeken om achteraf deze hoogte toe te kennen aan
    #hogere omtrekklassen
    MaxCurveLokaal <- curvekarakteristieken(Lokaalmodel, Data.lokaal) %>%
      filter(
        .data$Omtrek_Extr_Hoogte.d > 0.1,
        .data$Omtrek_Extr_Hoogte.d < .data$Q95k
      ) %>%
      transmute(
        .data$DOMEIN_ID,
        .data$BMS,
        .data$Omtrek_Extr_Hoogte.d,
        .data$Extr_Hoogte.d,
        Maximum = .data$Hoogteverschil.d > 0   #is het een hoog maximum?
      )

    Hoogte.lokaal <- Data.lokaal %>%
      inner_join(
        Lokaalmodel,
        by = c("BMS", "DOMEIN_ID")
      )

    if (!is.null(Uitbreiding)) {
      Hoogte.lokaal <- Hoogte.lokaal %>%
        left_join(
          Uitbreiding,
          by = c("BMS", "DOMEIN_ID")
        ) %>%
        mutate(
          Q95k = ifelse(is.na(.data$MaxOmtrek), .data$Q95k, .data$MaxOmtrek)
        )
    }

    Hoogte.lokaal <- Hoogte.lokaal %>%
      group_by(
        .data$BMS,
        .data$DOMEIN_ID
      ) %>%
      do(
        hoogteschatting.basis(.$Model[[1]],
                               select(., -"Model"),
                               "Lokaal", unique(.$BMS),
                              Uitbreiding = !is.null(Uitbreiding))
      ) %>%
      ungroup() %>%
      mutate(
        Modeltype = "lokaal model"
      ) %>%
      left_join(
        MaxCurveLokaal,
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      left_join(
        Data.lokaal %>%
          group_by(
            .data$BMS,
            .data$DOMEIN_ID
          ) %>%
          do(
            rmse.basis(., "Lokaal", .data$BMS)
          ) %>%
          ungroup()  %>%
          transmute(
            .data$BMS, .data$DOMEIN_ID,
            RMSE = .data$rmseD
          ),
        c("BMS", "DOMEIN_ID")
      )

    if (exists("Hoogteschatting")) {
      Hoogteschatting <- Hoogteschatting %>%
        bind_rows(Hoogte.lokaal)
    } else {
      Hoogteschatting <- Hoogte.lokaal
    }
  }

  if (exists("Hoogteschatting")) {
    Hoogteschatting <- Hoogteschatting %>%
      transmute(
        .data$BMS,
        .data$IDbms,
        .data$DOMEIN_ID,
        .data$BOS_BHI,
        .data$Omtrek,
        OmtrekklassetypeID = as.integer(.data$Omtrek * 10 + 1.5),
        Omtrekklasse =
          paste(.data$Omtrek * 100 - 5, .data$Omtrek * 100 + 5, sep = " - "),
        Hoogte =  #voor laag maximum: max waarde aanhouden
          ifelse(!is.na(.data$Omtrek_Extr_Hoogte.d) & .data$Maximum &
                   .data$Omtrek > .data$Omtrek_Extr_Hoogte.d,
                 .data$Extr_Hoogte.d,
                 .data$H_D_finaal),
        Hoogte =  #voor hoog minimum: min waarde aanhouden
          ifelse(!is.na(.data$Omtrek_Extr_Hoogte.d) & !.data$Maximum &
                 .data$Omtrek < .data$Omtrek_Extr_Hoogte.d,
                 .data$Extr_Hoogte.d,
                 .data$H_D_finaal),
        Hoogte = ifelse(.data$Hoogte < 2.5, 2.5, .data$Hoogte),
        .data$RMSE,
        .data$Modeltype
      ) %>%
      distinct() %>%
      filter(.data$Hoogte >= 2.5)

    return(Hoogteschatting)
  } else {
    message("Er zijn geen modellen opgegeven.")
  }

}
