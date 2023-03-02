#' @title Validatie van het basismodel
#'
#' @description
#' Functie die alle nodige validaties uitvoert op het opgegeven (volledige)
#' model en een overzicht geeft van de afwijkende metingen en slechte curves
#' (zodat de gebruiker deze kan valideren).
#'
#' De functie roept meerdere hulpfuncties op:
#'
#' - `rmse.basis()`
#'
#' - `afwijkendeMetingen()`
#'
#' - `afwijkendeCurves()`
#'
#' - `validatierapport()`
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de
#' slechtste modellen opgelijst (op basis van RMSE, afwijkende metingen en
#' afwijkende curves).
#'
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' `fit.basis()`: tibble met de velden `BMS` (boomsoort) en `Model`
#' (`lme`-object met het gefit mixed model voor die boomsoort).
#' @param ExtraCurvesRapport Optie om een lijst met extra
#' boomsoort-domeincombinaties op te geven waardoor de curve getoond moet worden
#' in het validatierapport.
#' Deze moeten opgegeven worden als een dataframe met velden `DOMEIN_ID` en
#' `BMS`, met benamingen die overeenkomen met deze in de opgegeven dataset.
#' @param GoedgekeurdeAfwijkendeCurves Optie om goedgekeurde afwijkende curves
#' niet meer te tonen in het validatierapport zolang er geen extra metingen
#' toegevoegd zijn voor de boomsoort-domeincombinatie (om als gebruiker enkel de
#' te keuren curves in het rapport over te houden). De goedgekeurde en dus te
#' negeren curves moeten opgegeven worden in een dataframe met velden
#' `DOMEIN_ID`, `BMS` en `nBomenTerugTonen`, met `nBomenTerugTonen` het aantal
#' bomen (`nBomenInterval`) in de boomsoort-domeincombinatie vanaf wanneer de
#' curve terug getoond moet worden. (In dit geval wordt de curve uiteraard enkel
#' terug getoond als ze nog steeds afwijkend is.)
#'
#' @inheritParams afwijkendeMetingen
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
#' De functie geeft een dataframe terug met de te controleren metingen, met
#' behalve de informatie uit de databank een aantal berekende waarden:
#' - `H_D_finaal`: een geschatte hoogte voor de omtrekklasse volgens het
#'     domeinmodel (dus specifiek voor de boomsoort-domeincombinatie)
#' - `H_VL_finaal`: een geschatte hoogte voor de omtrek volgens het Vlaams
#'     model (dus voor het overkoepelend boomsoort-model)
#' - `rmseD`: de foutenschatting voor het domeinmodel
#' - `HogeRmse`: `TRUE` als het domeinmodel een hoge RMSE heeft, anders
#'     `NA`
#'
#' @export
#'
#' @importFrom dplyr %>% filter select mutate distinct group_by
#' summarise ungroup bind_rows do rowwise anti_join left_join
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name is.count
#'

validatie.basis <-
  function(Basismodel, AantalDomHogeRMSE = 20, ExtraCurvesRapport = NULL,
           GoedgekeurdeAfwijkendeCurves = NULL,
           Bestandsnaam = "Default", TypeRapport = "Dynamisch", PathWD = getwd()
           ) {

  invoercontrole(Basismodel, "basismodel")

  Rmse <- Basismodel %>%
    rowwise() %>%
    do(
      rmse.basis(.$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  Hoogteschatting <- Basismodel %>%
    rowwise() %>%
    do(
      hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
    ) %>%
    ungroup()

  Dataset <- Hoogteschatting %>%
    left_join(
      Rmse %>%
        select("BMS", "DOMEIN_ID", "rmseD", "maxResid"),
      by = c("BMS", "DOMEIN_ID")
    )

  AfwijkendeMetingen <- afwijkendeMetingen(Dataset, AantalDomHogeRMSE)

  #afwijkende curves
  AfwijkendeCurves <- afwijkendeCurves(Basismodel)

  if (!is.null(ExtraCurvesRapport)) {
    assert_that(has_name(ExtraCurvesRapport, "DOMEIN_ID"))
    assert_that(has_name(ExtraCurvesRapport, "BMS"))
    ZonderJoin <- ExtraCurvesRapport %>%
      anti_join(Dataset, by = c("DOMEIN_ID", "BMS"))
    if (nrow(ZonderJoin) > 0) {
      warning(
        "Niet elk opgegeven record in ExtraCurvesRapport heeft een basismodel"
      )
    }
  } else {
    ExtraCurvesRapport <-
      data.frame(DOMEIN_ID = character(0), BMS = character(0))
  }
  if (!is.null(GoedgekeurdeAfwijkendeCurves)) {
    assert_that(has_name(GoedgekeurdeAfwijkendeCurves, "DOMEIN_ID"))
    assert_that(has_name(GoedgekeurdeAfwijkendeCurves, "BMS"))
    assert_that(has_name(GoedgekeurdeAfwijkendeCurves, "nBomenTerugTonen"))
    assert_that(
      inherits(
        GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen, c("integer", "numeric")),
      msg = "Elke waarde van nBomenTerugTonen in de dataframe GoedgekeurdeAfwijkendeCurves moet een getal zijn" #nolint: line_length_linter
    )
    if (inherits(GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen, "numeric")) {
      assert_that(
        max(
          abs(
            GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen -
              as.integer(GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen)
          ),
          na.rm = TRUE
        ) < 1e-6
        , msg = "Elke waarde van nBomenTerugTonen in de dataframe GoedgekeurdeAfwijkendeCurves moet een geheel getal zijn" #nolint: line_length_linter
      )
      GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen <-
        as.integer(GoedgekeurdeAfwijkendeCurves$nBomenTerugTonen)
    }
    ZonderJoin <- GoedgekeurdeAfwijkendeCurves %>%
      anti_join(AfwijkendeCurves, by = c("DOMEIN_ID", "BMS"))
    if (nrow(ZonderJoin) > 0) {
      warning("Niet elk opgegeven record in GoedgekeurdeAfwijkendeCurves heeft een afwijkende curve") #nolint: line_length_linter
    }
    AfwijkendeCurvesNegeren <- GoedgekeurdeAfwijkendeCurves %>%
      left_join(
        Dataset %>%
          select("DOMEIN_ID", "BMS", "nBomenInterval") %>%
          distinct(),
        by = c("DOMEIN_ID", "BMS")
      ) %>%
      filter(
        .data$nBomenInterval < .data$nBomenTerugTonen
      )
  } else {
    AfwijkendeCurvesNegeren <-
      data.frame(DOMEIN_ID = character(0), BMS = character(0))
  }

  SlechtsteModellen <- AfwijkendeMetingen %>%
    filter(.data$HogeRmse & .data$Status != "Goedgekeurd") %>%
    select("DOMEIN_ID", "BMS") %>%
    distinct() %>%
    mutate(
      Reden = "hoge RMSE"
    ) %>%
    bind_rows(
      AfwijkendeCurves %>%
        anti_join(
          AfwijkendeCurvesNegeren,
          by = c("DOMEIN_ID", "BMS")
        )
    ) %>%
    bind_rows(
      AfwijkendeMetingen %>%
        filter(
          .data$Status != "Goedgekeurd"
        ) %>%
        select(
          "BMS", "DOMEIN_ID"
        ) %>%
        distinct() %>%
        mutate(
          Reden = "afwijkende metingen"
        )
    ) %>%
    bind_rows(
      ExtraCurvesRapport %>%
        mutate(
          Reden = "opgegeven als extra curve"
        )
    ) %>%
    mutate(
      Omtrek_Buigpunt.d =
        ifelse(is.na(.data$Omtrek_Buigpunt.d), "", .data$Omtrek_Buigpunt.d),
      Omtrek_Extr_Hoogte.d =
        ifelse(
          is.na(.data$Omtrek_Extr_Hoogte.d), "", .data$Omtrek_Extr_Hoogte.d)
    ) %>%
    group_by(
      .data$BMS, .data$DOMEIN_ID
    ) %>%
    summarise(
      Reden = paste(.data$Reden, collapse = ", "),
      Omtrek_Buigpunt =
        as.numeric(paste(.data$Omtrek_Buigpunt.d, collapse = "")),
      Omtrek_Extr_Hoogte =
        as.numeric(paste(.data$Omtrek_Extr_Hoogte.d, collapse = ""))
    ) %>%
    ungroup()

  Bestandsnaam <- ifelse(Bestandsnaam == "Default",
                         "Validatie_Basis.html",
                         Bestandsnaam)
  validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                   Bestandsnaam, TypeRapport, PathWD = PathWD)

  return(AfwijkendeMetingen)

}
