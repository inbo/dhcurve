#' @title Validatie van het afgeleid model
#'
#' @description
#' Functie die de validatie uitvoert op het verschoven Vlaams model en een
#' overzicht geeft van de afwijkende metingen (zodat de gebruiker deze kan
#' valideren).
#'
#' validatie.afgeleid roept meerdere hulpfuncties op:
#' \itemize{
#'   \item{rmse.basis en rmse.verschuiving}
#'   \item{afwijkendeMetingen}
#'   \item{validatierapport}
#' }
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de
#' slechtste modellen opgelijst (op basis van rmse en afwijkende metingen).
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' fit.basis: tibble met de velden BMS (boomsoort) en Model (lme-object met het
#' gefit mixed model voor die boomsoort)
#' @param Afgeleidmodel Model per domein-boomsoortcombinatie zoals teruggegeven
#' door de functie fit.afgeleid: list met 2 tibbles.
#' #@param Data.afgeleid dataframe 10-50
#'
#' @inheritParams afwijkendeMetingen
#' @inheritParams validatierapport
#' @inheritParams validatie.basis
#' @inheritParams initiatie
#'
#' @return De functie genereert een validatierapport (html-bestand) in de
#' working directory met informatie en grafieken van de te controleren metingen.
#' De afwijkende metingen zijn in rood aangeduid (zie ?validatierapport of
#' vignette voor meer informatie).
#'
#' De functie geeft een dataframe terug met de te controleren metingen, met
#' behalve de informatie uit de databank een aantal berekende waarden:
#' \itemize{
#'   \item{H_D_finaal: een geschatte hoogte voor de omtrekklasse volgens het
#'     domeinmodel}
#'   \item{rsmeD: de foutenschatting voor het domeinmodel}
#'   \item{H_VL_finaal: een geschatte hoogte voor de omtrekklasse volgens het
#'     Vlaams model waarvan het domeinmodel afgeleid is}
#'   \item{rmseVL: de foutenschatting voor dit Vlaams model}
#'   \item{HogeRmse: TRUE als het domeinmodel een hoge rmse heeft, anders NA}
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% filter rowwise do select distinct mutate bind_rows
#' group_by summarise ungroup inner_join
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
      sseVL = (.data$rmseVL) ^ 2 * (.data$nBomenOmtrek05 - 2)
    ) %>%
    group_by(.data$BMS) %>%
    summarise(
      nBomen = sum(.data$nBomen),
      nBomenInterval = sum(.data$nBomenInterval),
      nBomenOmtrek05VL = sum(.data$nBomenOmtrek05),
      rmseVL = sqrt(sum(.data$sseVL) / (.data$nBomenOmtrek05VL - 2))
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
      RmseVL %>% select(.data$BMS, .data$rmseVL),
      by = c("BMS")
    ) %>%
    mutate(
      rmseD = sqrt(.data$rmseVL ^ 2 + .data$RmseVerschuiving ^ 2)
    )


  Hoogteschatting <- AModel %>%
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
      ResidD2 = (.data$HOOGTE - .data$H_D_finaal) ^ 2
    )

  Dataset <- Hoogteschatting %>%
    select(.data$BMS, .data$DOMEIN_ID, .data$ResidD2) %>%
    filter(!is.na(.data$ResidD2)) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    summarise(
      maxResid = max(c(.data$ResidD2))
    ) %>%
    ungroup() %>%
    inner_join(
      Hoogteschatting,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    inner_join(
      Rmse,
      by = c("BMS", "DOMEIN_ID")
    )


  AfwijkendeMetingen <- afwijkendeMetingen(Dataset, AantalDomHogeRMSE)

  if (!is.null(ExtraCurvesRapport)) {
    assert_that(has_name(ExtraCurvesRapport, "DOMEIN_ID"))
    assert_that(has_name(ExtraCurvesRapport, "BMS"))
    ZonderJoin <- ExtraCurvesRapport %>%
      anti_join(Dataset, by = c("DOMEIN_ID", "BMS"))
    if (nrow(ZonderJoin) > 0) {
      warning("Niet elk opgegeven record in ExtraCurvesRapport heeft een afgeleid model") #nolint
    }
  } else {
    ExtraCurvesRapport <-
      data.frame(DOMEIN_ID = character(0), BMS = character(0))
  }

  SlechtsteModellen <- AfwijkendeMetingen %>%
    filter(.data$HogeRmse & .data$Status != "Goedgekeurd") %>%
    select(.data$DOMEIN_ID, .data$BMS) %>%
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
          .data$BMS, .data$DOMEIN_ID
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
