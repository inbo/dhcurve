#' Berekent de geschatte hoogtes per omtrekklasse
#'
#' De functie outputIVANHO voert de berekeningen uit die leiden tot de
#' gevraagde tabel om in IVANHO te importeren: een tabel met per boomsoort,
#' domein en omtrekklasse een schatting van de hoogte.  Als de curve een
#' maximum hoogte vertoont binnen het bestudeerde interval, wordt deze
#' maximumwaarde als hoogte meegegeven aan alle omtrekklassen hoger dan de
#' omtrekklasse van dit maximum.  (Dus verschillend van de validatierapporten
#' daalt de hoogte hier niet terug na het maximum, maar de hoogste waarde wordt
#' aangehouden.)
#'
#' Voor deze functie worden volgende hulpfuncties aangeroepen:
#' hoogteschatting.basis, hoogteschatting.afgeleid en curvekarakteristieken.
#'
#' @inheritParams resultaat
#'
#' @return Dataframe met geschatte hoogtes per domein en per boomsoort met
#' velden:
#'
#' - DOMEIN_ID: domeincode
#'
#' - BOS_BHI: domeinnaam
#'
#' - IDbms (nog toe te voegen!!!)
#'
#' - BMS: boomsoort
#'
#' - Modeltype ('basismodel'(= eigen model op basis van mixed model) /
#' ‘afgeleid model'(= verschoven Vlaams model, afgeleid van fixed factor uit
#' basismodel) /
#' ‘Vlaams model’(= fixed factor uit basismodel, niet toegevoegd
#' omdat niet relevant) / 'lokaal model'(= apart model voor 1
#' boomsoort-domein-combinatie) / 'geen model'(= boomsoort-domein-combinatie
#' waarvoor geen model berekend kan worden))
#'
#' - Omtrek: klassemidden van omtrekklasse
#'
#' - OmtrekklassetypeID: het overeenkomstige identificatienummer van de
#' omtrekklasse
#'
#' - Omtrekklasse
#'
#' - Hoogte: de geschatte hoogte
#'
#' @export
#'
#' @importFrom dplyr %>% select filter rowwise do ungroup mutate bind_rows
#' group_by transmute distinct inner_join left_join
#' @importFrom plyr .
#' @importFrom rlang .data
#'

outputIVANHO <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL) {

  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")

    #maxima binnen interval opzoeken om achteraf deze hoogte toe te kennen aan
    #hogere omtrekklassen
    MaxCurveBasis <- curvekarakteristieken(Basismodel) %>%
      filter(
        .data$Omtrek_Extr_Hoogte.d > 0.1,
        .data$Omtrek_Extr_Hoogte.d < .data$Q95k,
        .data$Hoogteverschil.d > 0
      ) %>%
      select(
        .data$DOMEIN_ID,
        .data$BMS,
        .data$Omtrek_Extr_Hoogte.d,
        .data$Extr_Hoogte.d
      )

    #hoogtes van basismodel schatten
    Hoogteschatting <- Basismodel %>%
      rowwise() %>%
      do(
        hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
      ) %>%
      ungroup() %>%
      mutate(
        Modeltype = "basismodel"
      ) %>%
      left_join(
        MaxCurveBasis,
        by = c("BMS", "DOMEIN_ID")
      )

    if (!is.null(Afgeleidmodel)) {
      invoercontrole(Afgeleidmodel, "afgeleidmodel")

      Hoogteschatting <- Hoogteschatting %>%
        bind_rows(
          Afgeleidmodel[[1]] %>%
            inner_join(
              Afgeleidmodel[[2]],
              by = c("BMS", "DOMEIN_ID")
            ) %>%
            group_by(
              .data$BMS,
              .data$DOMEIN_ID
            ) %>%
            do(
              hoogteschatting.afgeleid(.$Model[[1]],
                                        select(., -.data$Model))
            ) %>%
            ungroup() %>%
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

    #maxima binnen interval opzoeken om achteraf deze hoogte toe te kennen aan
    #hogere omtrekklassen
    MaxCurveLokaal <- curvekarakteristieken(Lokaalmodel, Data.lokaal) %>%
      filter(
        .data$Omtrek_Extr_Hoogte.d > 0.1,
        .data$Omtrek_Extr_Hoogte.d < .data$Q95k,
        .data$Hoogteverschil.d > 0
      ) %>%
      select(
        .data$DOMEIN_ID,
        .data$BMS,
        .data$Omtrek_Extr_Hoogte.d,
        .data$Extr_Hoogte.d
      )

    Hoogte.lokaal <- Lokaalmodel %>%
      inner_join(
        Data.lokaal,
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      group_by(
        .data$BMS,
        .data$DOMEIN_ID
      ) %>%
      do(
        hoogteschatting.basis(.$Model[[1]],
                               select(., -.data$Model),
                               "Lokaal", .$BMS)
      ) %>%
      ungroup() %>%
      mutate(
        Modeltype = "lokaal model"
      ) %>%
      left_join(
        MaxCurveLokaal,
        by = c("BMS", "DOMEIN_ID")
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
        .data$DOMEIN_ID,
        .data$BOS_BHI,
        .data$Omtrek,
        OmtrekklassetypeID = as.integer(.data$Omtrek * 10 + 1.5),
        Omtrekklasse =
          paste(.data$Omtrek * 100 - 5, .data$Omtrek * 100 + 5, sep = " - "),
        Hoogte =
          ifelse(!is.na(.data$Omtrek_Extr_Hoogte.d) &
                   .data$Omtrek > .data$Omtrek_Extr_Hoogte.d,
                 .data$Extr_Hoogte.d,
                 .data$H_D_finaal),
        .data$Modeltype
      ) %>%
      distinct()

    return(Hoogteschatting)
  } else {
    message("Er zijn geen modellen opgegeven.")
  }

}
