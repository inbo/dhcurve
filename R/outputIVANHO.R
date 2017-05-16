#' berekent de geschatte hoogtes per omtrekklasse
#'
#' Berekeningen die leiden tot de gevraagde tabel om in IVANHO te importeren: een tabel met per boomsoort, domein en omtrekklasse een schatting van de hoogte.  Als de curve een maximum hoogte vertoont binnen het bestudeerde interval, wordt deze maximumwaarde als hoogte meegegeven aan alle omtrekklassen hoger dan de omtrekklasse van dit maximum.  (Dus verschillend van de validatierapporten daalt de hoogte hier niet terug maar de hoogste waarde wordt aangehouden.)
#'
#' Hiervoor worden volgende hulpfuncties aangeroepen:
#'
#' hoogteschatting.basis, hoogteschatting.afgeleid, curvekarakteristieken
#'
#'
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
#' @importFrom dplyr %>% select_ filter_ rowwise do_ ungroup mutate_ bind_rows group_by_ transmute_ distinct_ inner_join left_join
#'

outputIVANHO <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL) {

  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")

    #maxima binnen interval opzoeken om achteraf deze hoogte toe te kennen aan
    #hogere omtrekklassen
    MaxCurveBasis <- curvekarakteristieken(Basismodel) %>%
      filter_(
        ~Omtrek_Extr_Hoogte.d > 0.1,
        ~Omtrek_Extr_Hoogte.d < Q95k,
        ~Hoogteverschil.d > 0
      ) %>%
      select_(
        ~DOMEIN_ID,
        ~BMS,
        ~Omtrek_Extr_Hoogte.d,
        ~Extr_Hoogte.d
      )

    #hoogtes van basismodel schatten
    Hoogteschatting <- Basismodel %>%
      rowwise() %>%
      do_(
        ~hoogteschatting.basis(.$Model, .$Model$data, "Basis")
      ) %>%
      ungroup() %>%
      mutate_(
        Modeltype = ~"basismodel"
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
      filter_(
        ~Omtrek_Extr_Hoogte.d > 0.1,
        ~Omtrek_Extr_Hoogte.d < Q95k,
        ~Hoogteverschil.d > 0
      ) %>%
      select_(
        ~DOMEIN_ID,
        ~BMS,
        ~Omtrek_Extr_Hoogte.d,
        ~Extr_Hoogte.d
      )

    Hoogte.lokaal <- Lokaalmodel %>%
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
      transmute_(
        ~BMS,
        ~DOMEIN_ID,
        ~BOS_BHI,
        ~Omtrek,
        OmtrekklassetypeID = ~as.integer(Omtrek * 10 + 1.5),
        Omtrekklasse = ~paste(Omtrek * 100 - 5, Omtrek * 100 + 5, sep = " - "),
        Hoogte =
          ~ifelse(!is.na(Omtrek_Extr_Hoogte.d) & Omtrek > Omtrek_Extr_Hoogte.d,
                  Extr_Hoogte.d,
                  H_D_finaal),
        ~Modeltype
      ) %>%
      distinct_()

    return(Hoogteschatting)
  } else {
    message("Er zijn geen modellen opgegeven.")
  }

}
