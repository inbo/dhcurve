#' @title Geeft de afwijkende metingen uit een gegeven model
#'
#' @description
#' Berekent afwijkende metingen, dit zijn metingen met een afwijking > 2,5 *
#' rmse, en geeft deze weer volgens dalende afwijking.  Voor de 20 domeinen met
#' de hoogste RMSE, waarbij de 'foutenmarge' breder is, worden minstens 10
#' meetresultaten geselecteerd als 'afwijkend' (nl. de 10 meetresultaten met de
#' hoogste error).
#'
#' @param Dataset Dataframe met meetresultaten, geschatte waarden voor het
#' domeinmodel en het Vlaamse model en de rmse voor het domeinmodel.  Deze
#' dataframe moet dezelfde velden bevatten als de dataframes in de list
#' teruggegeven in de functie initiatie, en daarnaast de velden H_D_finaal,
#' rmseD en maxResid.
#' @param AantalDomHogeRMSE Standaard worden de 20 domeinen met de hoogste RMSE
#' geselecteerd en voor elk van deze domeinen worden minstens 10 afwijkende
#' metingen geselecteerd. AantalDomHogeRMSE laat toe om dit aantal van 20
#' domeinen aan te passen.
#'
#' @return Lijst met afwijkende metingen (> 2,5 * rmse), inclusief vlag uit
#' databank
#'
#' @export
#'
#' @importFrom dplyr %>% filter select distinct arrange transmute left_join
#' mutate group_by arrange slice ungroup desc
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name is.count
#'

afwijkendeMetingen <- function(Dataset, AantalDomHogeRMSE = 20) {

  invoercontrole(Dataset, "afgeleidedata")
  assert_that(has_name(Dataset, "H_D_finaal"))
  assert_that(inherits(Dataset$H_D_finaal, "numeric"))
  assert_that(has_name(Dataset, "rmseD"))
  assert_that(inherits(Dataset$rmseD, "numeric"))
  assert_that(has_name(Dataset, "maxResid"))
  assert_that(inherits(Dataset$maxResid, "numeric"))

  assert_that(is.count(AantalDomHogeRMSE) | AantalDomHogeRMSE == 0,
              msg = "AantalDomHogeRMSE moet een positief geheel getal zijn.")

  HogeRmse <- Dataset %>%
    select(.data$BMS, .data$DOMEIN_ID, .data$rmseD) %>%
    distinct() %>%
    arrange(desc(.data$rmseD)) %>%
    slice(seq_len(AantalDomHogeRMSE)) %>%
    transmute(
      .data$DOMEIN_ID,
      .data$BMS,
      HogeRmse = TRUE
    )

  Dataset <- Dataset %>%
    left_join(
      HogeRmse,
      by = c("BMS", "DOMEIN_ID")
    )

  #voor domeinen met hoge RMSE nemen we de 10 hoogste afwijkingen
  CorrectieHogeRMSE <- Dataset %>%
    filter(.data$HogeRmse) %>%
    mutate(
      error = abs(.data$HOOGTE - .data$H_D_finaal),
      HogeAfwijking = TRUE
    ) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    arrange(desc(.data$error)) %>%
    slice(1:10) %>%
    ungroup() %>%
    select(
      .data$BMS, .data$DOMEIN_ID, .data$C13, .data$HOOGTE, .data$HogeAfwijking
    ) %>%
    distinct()

  Dataset <- Dataset %>%
    left_join(
      CorrectieHogeRMSE,
      by = c("BMS", "DOMEIN_ID", "C13", "HOOGTE")
    ) %>%
    mutate(
      Afwijkend =
        ifelse(
          !is.na(.data$HogeAfwijking) & .data$HogeAfwijking,
          .data$HogeAfwijking,
          (.data$HOOGTE > (.data$H_D_finaal + 2.5 * .data$rmseD)) |
            (.data$HOOGTE < (.data$H_D_finaal - 2.5 * .data$rmseD))
        ),
      HogeAfwijking = NULL
    )

  Afwijkend <- Dataset %>%
    filter(
      .data$Afwijkend
    )

  return(Afwijkend)
}
