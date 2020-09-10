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
#' basismodel) / ‘Vlaams model’(= fixed factor uit basismodel, niet toegevoegd
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
#' DomeinID leeg gelaten wordt of een specifieke waarde ‘Vlaams model’ krijgt
#'
#' @export
#'
#' @importFrom dplyr %>% select_ left_join rowwise do_ ungroup rename_ mutate_
#' bind_rows group_by_
#'

resultaat <-
  function(Basismodel = NULL, Afgeleidmodel = NULL, Lokaalmodel = NULL,
           Data.lokaal = NULL, Data.onbruikbaar = NULL) {


  if (!is.null(Basismodel)) {
    invoercontrole(Basismodel, "basismodel")
    Modellen.basis <- modelparameters(Basismodel) %>%
    select_(~-Q5k, ~-Q95k) %>%
    left_join(Basismodel %>%
                rowwise() %>%
                do_(
                  ~rmse.basis(.$Model$data, "Basis")
                ) %>%
                ungroup(),
              c("BMS", "DOMEIN_ID")) %>%
    select_(~-maxResid)

    Modellen <- Modellen.basis %>%
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

    # volgende code is om ook de Vlaamse modellen toe te voegen aan de
    # resultatenlijst.  Omdat het niet wenselijk is om deze te gebruiken (als
    # een Vlaams model gebaseerd is op enkel bomen van 1 streek en de te
    # schatten boom ligt in een andere streek, is de schatting onbetrouwbaar),
    # voegen we het niet toe

    # Modellen.Vlaams <- Modellen.basis %>%
    #   select_(~-Ad, ~-Bd, ~-Cd, ~-rmseD) %>%
    #   mutate_(
    #     sseVL = ~(rmseVL)^2 * (nBomenOmtrek05 - 2)    #nolint
    #   ) %>%
    #   group_by_(~BMS, ~Avl, ~Bvl, ~Cvl) %>%
    #   summarise_(
    #     nBomen = ~sum(nBomen),
    #     nBomenInterval = ~sum(nBomenInterval),
    #     nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
    #     RMSE = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))   #nolint
    #   ) %>%
    #   ungroup() %>%
    #   rename_(
    #     A = ~Avl,
    #     B = ~Bvl,
    #     C = ~Cvl,
    #     nBomenOmtrek05 = ~nBomenOmtrek05VL    #nolint
    #   ) %>%
    #   mutate_(
    #     Modeltype = ~"Vlaams model"     #nolint
    #   )

    if (!is.null(Afgeleidmodel)) {
      invoercontrole(Afgeleidmodel, "afgeleidmodel")

      #Rmse van Vlaams model berekenen
      RmseVL <- Basismodel %>%
        rowwise() %>%
        do_(
          ~rmse.basis(.$Model$data, "Basis")
        ) %>%
        ungroup() %>%
        mutate_(
          sseVL = ~ (rmseVL) ^ 2 * (nBomenOmtrek05 - 2)
        ) %>%
        group_by_(~BMS) %>%
        summarise_(
          nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
          rmseVL = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
        ) %>%
        ungroup()

      #Rmse van verschuiving berekenen en combineren met die van Vlaams model
      RmseAfg <- Afgeleidmodel[[1]] %>%
        rowwise() %>%
        do_(
          ~rmse.verschuiving(.$Model, .$BMS, .$DOMEIN_ID)
        ) %>%
        ungroup() %>%
        inner_join(
          RmseVL %>% select_(~BMS, ~rmseVL),
          by = c("BMS")
        ) %>%
        mutate_(
          rmseD = ~sqrt(rmseVL ^ 2 + RmseVerschuiving ^ 2)
        )


      Modellen <- Modellen %>%
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
                c("BMS", "DOMEIN_ID")) %>%
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

    if (exists("Modellen")) {
      Modellen <- Modellen %>%
        bind_rows(Modellen.lokaal)
    } else {
      Modellen <- Modellen.lokaal
    }
  }

  if (!is.null(Data.onbruikbaar)) {
    invoercontrole(Data.onbruikbaar, "fit")

    Lijst.onbruikbaar <- Data.onbruikbaar %>%
      select_(
          ~DOMEIN_ID, ~BMS,
          ~nBomen, ~nBomenInterval, ~nBomenOmtrek05,
          ~Q5k, ~Q95k
        ) %>%
        distinct_() %>%
        mutate_(
          Modeltype = ~"Geen model"
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
