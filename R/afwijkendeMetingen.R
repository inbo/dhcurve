#' Geeft de afwijkende metingen uit een gegeven model
#'
#' Berekent afwijkende metingen, dit zijn metingen met een afwijking > 2,5 * rmse, en geeft deze weer volgens dalende afwijking.  Voor de 20 domeinen met de hoogste RMSE, waarbij de 'foutenmarge' breder is, worden minstens 10 meetresultaten geselecteerd als 'afwijkend' (nl. de 10 meetresultaten met de hoogste error).
#'
#' @param Dataset Dataframe met meetresultaten, geschatte waarden voor het domeinmodel en het Vlaamse model en de rmse voor het domeinmodel.
#' @param AantalDomHogeRMSE Standaard worden de 20 domeinen met de hoogste RMSE geselecteerd en voor elk van deze domeinen worden minstens 10 afwijkende metingen geselecteerd. AantalDomHogeRMSE laat toe om dit aantal van 20 domeinen aan te passen.
#'
#' @return lijst met afwijkende metingen (> 2,5 * rmse), inclusief vlag uit databank
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ select_ distinct_ arrange_ transmute_ left_join mutate_ group_by_ arrange_ slice_ ungroup desc
#' @importFrom assertthat assert_that has_name
#'

afwijkendeMetingen <- function(Dataset, AantalDomHogeRMSE = 20){

  invoercontrole(Dataset, "afgeleidedata")
  assert_that(has_name(Dataset, "H_D_finaal"))
  assert_that(inherits(Dataset$H_D_finaal, "numeric"))
  assert_that(has_name(Dataset, "rmseD"))
  assert_that(inherits(Dataset$rmseD, "numeric"))
  assert_that(has_name(Dataset, "maxResid"))
  assert_that(inherits(Dataset$maxResid, "numeric"))

  assert_that(inherits(AantalDomHogeRMSE, "numeric"))
  assert_that(AantalDomHogeRMSE == as.integer(AantalDomHogeRMSE))
  assert_that(AantalDomHogeRMSE >= 0)

  HogeRmse <- Dataset %>%
    select_(~BMS, ~DOMEIN_ID, ~rmseD) %>%
    distinct_() %>%
    arrange_(~ desc(rmseD)) %>%
    slice_(~ seq_len(AantalDomHogeRMSE)) %>%
    transmute_(
      ~DOMEIN_ID,
      ~BMS,
      HogeRmse = ~TRUE
    )

  Dataset <- Dataset %>%
    left_join(
      HogeRmse,
      by = c("BMS", "DOMEIN_ID")
    )

  #voor domeinen met hoge RMSE nemen we de 10 hoogste afwijkingen
  CorrectieHogeRMSE <- Dataset %>%
    filter_(~HogeRmse) %>%
    mutate_(
      error = ~abs(HOOGTE - H_D_finaal),
      HogeAfwijking = ~TRUE
    ) %>%
    group_by_(~BMS, ~DOMEIN_ID) %>%
    arrange_(~ desc(error)) %>%
    slice_(~1:10) %>%
    ungroup() %>%
    select_(~BMS, ~DOMEIN_ID, ~C13, ~HOOGTE, ~HogeAfwijking) %>%
    distinct_()

  Dataset <- Dataset %>%
    left_join(
      CorrectieHogeRMSE,
      by = c("BMS", "DOMEIN_ID", "C13", "HOOGTE")
    ) %>%
    mutate_(
      Afwijkend =
        ~ifelse(!is.na(HogeAfwijking) & HogeAfwijking, HogeAfwijking,
                (HOOGTE > (H_D_finaal + 2.5 * rmseD)) |
                  (HOOGTE < (H_D_finaal - 2.5 * rmseD))),
      HogeAfwijking = ~NULL
    )

  Afwijkend <- Dataset %>%
    filter_(
      ~Afwijkend
    )

  return(Afwijkend)
}
