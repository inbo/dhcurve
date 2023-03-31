#' @title Validatie van het afgeleid model
#'
#' @description
#' Functie die de validatie uitvoert op het verschoven Vlaams model en een
#' overzicht geeft van de afwijkende metingen (zodat de gebruiker deze kan
#' valideren).
#'
#' `validatie.afgeleid()` roept meerdere hulpfuncties op:
#' - `rmse.basis()` en `rmse.verschuiving()`
#' - `afwijkendeMetingen()`
#' - `validatierapport()`
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de
#' slechtste modellen opgelijst (op basis van RMSE en afwijkende metingen).
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' `fit.basis()`: tibble met de velden `BMS` (boomsoort) en `Model`
#' (`lme`-object met het gefit mixed model voor die boomsoort)
#' @param Afgeleidmodel Model per boomsoort-domeincombinatie zoals teruggegeven
#' door de functie `fit.afgeleid()`: list met 2 tibbles.
#'
#' @inheritParams afwijkendeMetingen
#' @inheritParams validatierapport
#' @inheritParams validatie.basis
#' @inheritParams initiatie
#'
#' @return De functie genereert een validatierapport (`.html`-bestand) in de
#' working directory met informatie en grafieken van de te controleren metingen.
#' De afwijkende metingen zijn in rood aangeduid (zie `?validatierapport` of
#' vignet voor meer informatie).
#'
#' De functie geeft een dataframe terug met de te controleren metingen, met
#' behalve de informatie uit de databank een aantal berekende waarden:
#' - `H_D_finaal`: een geschatte hoogte voor de omtrekklasse volgens het
#'     domeinmodel
#' - `rsmeD`: de foutenschatting voor het domeinmodel
#' - `H_VL_finaal`: een geschatte hoogte voor de omtrekklasse volgens het
#'     Vlaams model waarvan het domeinmodel afgeleid is
#' - `rmseVL`: de foutenschatting voor dit Vlaams model
#' - `HogeRmse`: `TRUE` als het domeinmodel een hoge RMSE heeft, anders `NA`
#'
#' @export
#'
#' @importFrom dplyr %>% filter rowwise do select distinct mutate bind_rows
#' group_by summarise ungroup inner_join transmute
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat assert_that is.count
#'

validatie.afgeleid <-
  function(Basismodel, Afgeleidmodel, AantalDomHogeRMSE = 20,
           ExtraCurvesRapport = NULL, Bestandsnaam = "Validatie_Afgeleid.html",
           TypeRapport = "Dynamisch", PathWD = getwd()) {

  invoercontrole(Basismodel, "basismodel")
  invoercontrole(Afgeleidmodel, "afgeleidmodel")

  AModel <- Afgeleidmodel[[1]]

  #Rmse van Vlaams model berekenen
  RmseVL <- Basismodel %>%
    filter(.data$BMS %in% unique(AModel$BMS)) %>%
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
      nBomen = sum(.data$nBomen),
      nBomenInterval = sum(.data$nBomenInterval),
      nBomenIntervalOmtrek05VL = sum(.data$nBomenIntervalOmtrek05),
      rmseVL = sqrt(sum(.data$sseVL) / (.data$nBomenIntervalOmtrek05VL - 2))
    ) %>%
    ungroup()

  #Rmse van verschuiving berekenen en combineren met die van Vlaams model
  Rmse <- AModel %>%
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


  Hoogteschatting <- AModel %>%
    inner_join(
      x = Afgeleidmodel[[2]],
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
      ResidD2 = (.data$HOOGTE - .data$H_D_finaal) ^ 2
    )

  Dataset <- Hoogteschatting %>%
    select("BMS", "DOMEIN_ID", "ResidD2") %>%
    filter(!is.na(.data$ResidD2)) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    summarise(
      maxResid = max(c(.data$ResidD2))
    ) %>%
    ungroup() %>%
    inner_join(
      Hoogteschatting,
      by = c("BMS", "DOMEIN_ID"),
      multiple = "all"
    ) %>%
    left_join(
      Rmse,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    mutate(
      Q5k = min(.data$Omtrek) + 0.3,
      Q95k = max(.data$Omtrek) - 0.2
    ) %>%
    ungroup()


  AfwijkendeMetingen <- afwijkendeMetingen(Dataset, AantalDomHogeRMSE)

  if (!is.null(ExtraCurvesRapport)) {
    assert_that(has_name(ExtraCurvesRapport, "DOMEIN_ID"))
    assert_that(has_name(ExtraCurvesRapport, "BMS"))
    ZonderJoin <- ExtraCurvesRapport %>%
      anti_join(Dataset, by = c("DOMEIN_ID", "BMS"))
    if (nrow(ZonderJoin) > 0) {
      warning("Niet elk opgegeven record in ExtraCurvesRapport heeft een afgeleid model") #nolint: line_length_linter
    }
  } else {
    ExtraCurvesRapport <-
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
        transmute(
          .data$DOMEIN_ID,
          .data$BMS,
          Reden = "opgegeven als extra curve"
        )
    ) %>%
    group_by(
      .data$BMS, .data$DOMEIN_ID
    ) %>%
    summarise(
      Reden = paste(.data$Reden, collapse = ", ")
    ) %>%
    ungroup()

  validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                   Bestandsnaam, TypeRapport, PathWD = PathWD)

  return(AfwijkendeMetingen)

}
