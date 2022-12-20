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
#'
#' Voor deze functie worden volgende hulpfuncties aangeroepen:
#' `hoogteschatting.basis()`, `hoogteschatting.afgeleid()` en
#' `curvekarakteristieken()`.
#'
#' @inheritParams resultaat
#'
#' @return Dataframe met geschatte hoogtes per domein en per boomsoort met
#' velden:
#' \itemize{
#'   \item{`DOMEIN_ID`: domeincode}
#'   \item{`BOS_BHI`: domeinnaam}
#'   \item{`IDbms` (identificatienummer van de boomsoort)}
#'   \item{`BMS`: boomsoort}
#'   \item{`Modeltype` ("basismodel"(= eigen model op basis van mixed model) /
#'     "afgeleid model"(= verschoven Vlaams model, afgeleid van fixed factor uit
#'     basismodel) /
#'     "Vlaams model"(= fixed factor uit basismodel, niet toegevoegd
#'     omdat niet relevant) / "lokaal model"(= apart model voor 1
#'     boomsoort-domeincombinatie) / "geen model"(= boomsoort-domeincombinatie
#'     waarvoor geen model berekend kan worden))}
#'   \item{`Omtrek`: klassenmidden van omtrekklasse}
#'   \item{`OmtrekklassetypeID`: het overeenkomstige identificatienummer van de
#'     omtrekklasse}
#'   \item{`Omtrekklasse`}
#'   \item{`Hoogte`: de geschatte hoogte}
#'   \item{`RMSE` (root mean square error, zie vignet voor meer info:
#'     \code{vignette("Handleiding", package = "dhcurve")})}
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% select filter rowwise do ungroup mutate bind_rows
#' group_by transmute distinct inner_join left_join summarise
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat has_name
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
        "DOMEIN_ID",
        "BMS",
        "Omtrek_Extr_Hoogte.d",
        "Extr_Hoogte.d"
      )

    #rsme_basis berekenen
    rmse_basis <- Basismodel %>%
      rowwise() %>%
      do(
        rmse.basis(.$Model$data, "Basis", .$BMS)
      ) %>%
      ungroup()

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
          Afgeleidmodel[[1]] %>%
            inner_join(
              Afgeleidmodel[[2]] %>%
                group_by(.data$BMS) %>%
                mutate(
                  IDbms = max(.data$IDbms, na.rm = TRUE)
                ) %>%
                ungroup(),
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
      invoercontrole(Data.lokaal, "fit")
      if (has_name(Data.lokaal, "VoorModelFit")) {
        Data.lokaal <- Data.lokaal %>%
          filter(.data$VoorModelFit) %>%
          select(-"VoorModelFit")
      }
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
        "DOMEIN_ID",
        "BMS",
        "Omtrek_Extr_Hoogte.d",
        "Extr_Hoogte.d"
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
                               select(., -"Model"),
                               "Lokaal", unique(.$BMS))
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
        Hoogte =
          ifelse(!is.na(.data$Omtrek_Extr_Hoogte.d) &
                   .data$Omtrek > .data$Omtrek_Extr_Hoogte.d,
                 .data$Extr_Hoogte.d,
                 .data$H_D_finaal),
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
