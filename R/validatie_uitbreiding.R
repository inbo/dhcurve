#' @title Validatie van een uitbreiding van een model naar hogere omtrekklassen
#'
#' @description
#' Functie die toelaat om de opgegeven modellen te valideren ter hoogte van
#' omtrekklassen boven het bruikbaar interval ingeval hiervoor voldoende
#' gegevens beschikbaar zijn.
#' De functie berekent de waarden `diff_min`, `diff_mediaan` en `diff_max` en
#' geeft een overzichtsrapport met de 20 slechtste niet eerder afgekeurde
#' curves (= curves met hoogste absolute waarde voor `diff_mediaan`),
#' waarvan het de bedoeling is dat de gebruiker ze valideert.
#'
#' @param Model Basismodel per boomsoort of
#' lokaal model per boomsoort-domein-combinatie zoals teruggegeven
#' door de functies `fit.basis()` of `fit.lokaal()`
#' @param Dataset Dataset zoals teruggegeven door functie `initiatie()`, op
#' basis waarvan het opgegeven model (basismodel of lokaal model) berekend is.
#' @param GoedgekeurdeUitbreidingen Optie om goedgekeurde uitbreidingen
#' niet meer te tonen in het validatierapport zolang er geen extra metingen
#' toegevoegd zijn voor de boomsoort-domeincombinatie (om als gebruiker enkel de
#' te keuren curves in het rapport over te houden). De goedgekeurde en dus te
#' negeren curves moeten opgegeven worden in een dataframe met velden
#' `DOMEIN_ID`, `BMS` en `nBomenTerugTonen`, met `nBomenTerugTonen` het aantal
#' bomen (`nExtra`) in de boomsoort-domeincombinatie vanaf wanneer de
#' curve terug getoond moet worden. (In dit geval wordt de curve uiteraard enkel
#' terug getoond als ze bij de 20 slechtste curves hoort, met hoogste
#' `diff_mediaan`.)
#' @param AantalDomValidatie Standaard worden de 20 domeinen met de hoogste
#' gemiddelde afwijking (DiffMediaan) geselecteerd om te valideren.
#' (Hierbij worden `GoedgekeurdeUitbreidingen` niet meer getoond.)
#' `AantalDomHogeRMSE` laat toe om dit aantal van 20 domeinen aan te passen.
#'
#' @inheritParams validatierapport
#' @inheritParams initiatie
#'
#' @return
#'
#' De functie genereert een validatierapport (`.html`-bestand) in de working
#' directory met informatie en grafieken van de te controleren modellen.  De
#' afwijkende metingen en curvedelen zijn in rood aangeduid; boven de curve is
#' het probleem ook woordelijk beschreven (zie `?validatierapport` of vignet
#' voor meer informatie).
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join filter select mutate group_by
#' summarise ungroup do rowwise count arrange desc distinct
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name
#' @importFrom stats median
#' @importFrom utils head
#'

validatie.uitbreiding <-
  function(Model, Dataset, AantalDomValidatie = 20,
           GoedgekeurdeUitbreidingen = NULL,
           Bestandsnaam = "Default", TypeRapport = "Dynamisch", PathWD = getwd()
           ) {

  Dataset <- Dataset %>%
    filter(.data$nExtra > 0)
  invoercontrole(Dataset, "fit", Uitbreiding = TRUE)
  assert_that(
    has_name(Dataset, "VoorModelFit"),
    msg = "De opgegeven dataset heeft geen veld 'VoorModelFit' zoals de data gegenereerd door functie initiatie() voor het berekenen van een basismodel of lokaal model. Geef de juiste dataset mee.") #nolint: line_length_linter
  assert_that(is.count(AantalDomValidatie) | AantalDomValidatie == 0,
              msg = "AantalDomValidatie moet een positief geheel getal zijn.")
  if (!is.null(GoedgekeurdeUitbreidingen)) {
    assert_that(has_name(GoedgekeurdeUitbreidingen, "DOMEIN_ID"))
    assert_that(has_name(GoedgekeurdeUitbreidingen, "BMS"))
    assert_that(has_name(GoedgekeurdeUitbreidingen, "nBomenTerugTonen"))
    assert_that(
      inherits(
        GoedgekeurdeUitbreidingen$nBomenTerugTonen, c("integer", "numeric")),
      msg = "Elke waarde van nBomenTerugTonen in de dataframe GoedgekeurdeUitbreidingen moet een getal zijn" #nolint: line_length_linter
    )
    if (inherits(GoedgekeurdeUitbreidingen$nBomenTerugTonen, "numeric")) {
      assert_that(
        max(
          abs(
            GoedgekeurdeUitbreidingen$nBomenTerugTonen -
              as.integer(GoedgekeurdeUitbreidingen$nBomenTerugTonen)
          ),
          na.rm = TRUE
        ) < 1e-6
        , msg = "Elke waarde van nBomenTerugTonen in de dataframe GoedgekeurdeUitbreidingen moet een geheel getal zijn" #nolint: line_length_linter
      )
      GoedgekeurdeUitbreidingen$nBomenTerugTonen <-
        as.integer(GoedgekeurdeUitbreidingen$nBomenTerugTonen)
    }
    ZonderJoin <- GoedgekeurdeUitbreidingen %>%
      anti_join(Dataset, by = c("DOMEIN_ID", "BMS"))
    if (nrow(ZonderJoin) > 0) {
      warning("Niet elk opgegeven record in GoedgekeurdeUitbreidingen heeft een afwijkende curve") #nolint: line_length_linter
    }
  }

  DatasetUitbreiding <- Dataset %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    mutate(
      MaxOmtrek = max(.data$Omtrek),
      Uitbreiden = !min(.data$VoorModelFit, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(.data$Uitbreiden) %>%
    mutate(
      Q95k = .data$MaxOmtrek
    )

  if (has_name(Model, "DOMEIN_ID")) {
    invoercontrole(Model, "lokaalmodel")
    MaxCurve <- curvekarakteristieken(Model, DatasetUitbreiding)
    Hoogteschatting <- Model %>%
      inner_join(
        DatasetUitbreiding,
        by = c("BMS", "DOMEIN_ID")
      ) %>%
      group_by(
        .data$BMS,
        .data$DOMEIN_ID
      ) %>%
      do(
        hoogteschatting.basis(.$Model[[1]], select(., -"Model"), "Lokaal",
                              unique(.$BMS), Uitbreiding = TRUE)
      ) %>%
      ungroup()
  } else {
    invoercontrole(Model, "basismodel")
    MaxCurve <- curvekarakteristieken(Model) %>%
      select(-"Q95k") %>%
      inner_join(
        DatasetUitbreiding %>%
          select("DOMEIN_ID", "BMS", "Q95k") %>%
          distinct(),
        by = c("BMS", "DOMEIN_ID")
      )
    Hoogteschatting <- Model %>%
      inner_join(
        DatasetUitbreiding,
        by = c("BMS")
      ) %>%
      group_by(.data$BMS) %>%
      do(
        hoogteschatting.basis(.$Model[[1]], select(., -"Model"), "Basis",
                              unique(.$BMS), Uitbreiding = TRUE)
      ) %>%
      ungroup()
  }

  MaxCurve <- MaxCurve %>%
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
  
  Hoogteschatting <- Hoogteschatting %>%
    left_join(
      MaxCurve,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    mutate(
      H_D_finaal =
        ifelse(!is.na(.data$Omtrek_Extr_Hoogte.d) &
                 .data$Omtrek > .data$Omtrek_Extr_Hoogte.d,
               .data$Extr_Hoogte.d,
               .data$H_D_finaal)
    )

  Diff <- Hoogteschatting %>%
    filter(!.data$VoorModelFit) %>%
    mutate(
      Diff = .data$HOOGTE - .data$H_D_finaal
    ) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    summarise(
      DiffMin = min(.data$Diff),
      DiffMediaan = median(.data$Diff),
      DiffMax = max(.data$Diff)
    ) %>%
    ungroup()

  SlechtsteModellen <- Diff %>%
    arrange(desc(abs(.data$DiffMediaan))) %>%
    head(AantalDomValidatie) %>%
    mutate(
      Reden = "Om uitbreiding goed te keuren"
    )

  Hoogteschatting <- Hoogteschatting %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    mutate(
      nExtraDomeinBMS = max(.data$nExtra, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(
      Diff %>%
        select("BMS", "DOMEIN_ID", "DiffMediaan"),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    mutate(
      nExtra = ifelse(is.na(.data$nExtra), .data$nExtraDomeinBMS, .data$nExtra),
      nExtraDomeinBMS = NULL,
      rmseD = NA_real_,
      maxResid = abs(.data$DiffMediaan), #voor sortering in rapport
      DiffMediaan = NULL
    )
  if (!is.null(GoedgekeurdeUitbreidingen)) {
    Hoogteschatting <- Hoogteschatting %>%
      left_join(GoedgekeurdeUitbreidingen, by = c("BMS", "DOMEIN_ID")) %>%
      filter(
        is.na(.data$nBomenTerugTonen) |
          (!is.na(.data$nBomenTerugTonen) &
             .data$nExtra > .data$nBomenTerugTonen)
      )
    SlechtsteModellen <- SlechtsteModellen %>%
      inner_join(
        Hoogteschatting %>%
          select("BMS", "DOMEIN_ID") %>%
          distinct(),
        by = c("BMS", "DOMEIN_ID")
      )
  }

  AfwijkendeMetingen <-
    data.frame(
      BMS = character(0), DOMEIN_ID = character(0), C13 = numeric(0),
      HOOGTE = numeric(0), Status = character(0)
    )

  Bestandsnaam <- ifelse(Bestandsnaam == "Default",
                         "Validatie_Uitbreiding.html",
                         Bestandsnaam)
  validatierapport(
    SlechtsteModellen, AfwijkendeMetingen, Dataset = Hoogteschatting,
    Bestandsnaam, TypeRapport, Uitbreidingsrapport = TRUE, PathWD = PathWD)

  Uitbreiding <- DatasetUitbreiding %>%
    select("BMS", "DOMEIN_ID", "MaxOmtrek") %>%
    distinct()

  return(Uitbreiding)

}
