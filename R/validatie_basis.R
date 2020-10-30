#' Validatie van het basismodel
#'
#' Functie die alle nodige validaties uitvoert op het opgegeven (volledige)
#' model en een overzicht geeft van de afwijkende metingen en slechte curves
#' (zodat de gebruiker deze kan valideren).
#'
#' De functie roept meerdere hulpfuncties op:
#'
#' - rmse.basis
#'
#' - afwijkendeMetingen
#'
#' - afwijkendeCurves
#'
#' - validatierapport
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de
#' slechtste modellen opgelijst (op basis van rmse, afwijkende metingen en
#' afwijkende curves).
#'
#'
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' fit.basis: tibble met de velden BMS (boomsoort) en Model (lme-object met het
#' gefit mixed model voor die boomsoort).
#'
#' @inheritParams afwijkendeMetingen
#' @inheritParams validatierapport
#'
#' @return
#'
#' De functie genereert een validatierapport (html-bestand) in de working
#' directory met informatie en grafieken van de te controleren modellen.  De
#' afwijkende metingen en curvedelen zijn in rood aangeduid; boven de curve is
#' het probleem ook woordelijk beschreven (zie ?validatierapport of vignette
#' voor meer informatie).
#'
#' De functie geeft een dataframe terug met de te controleren metingen, met
#' behalve de informatie uit de databank een aantal berekende waarden:
#'
#' - H_D_finaal: een geschatte hoogte voor de omtrekklasse volgens het
#' domeinmodel (dus specifiek voor de boomsoort-domeincombinatie)
#'
#' - H_VL_finaal: een geschatte hoogte voor de omtrek volgens het Vlaams model
#' (dus voor het overkoepelend boomsoort-model)
#'
#' - rmseD: de foutenschatting voor het domeinmodel
#'
#' - HogeRmse: TRUE als het domeinmodel een hoge rmse heeft, anders NA
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join filter select mutate distinct group_by
#' summarise ungroup bind_rows do rowwise
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name is.count
#'

validatie.basis <-
  function(Basismodel, AantalDomHogeRMSE = 20,
           Bestandsnaam = "Default", TypeRapport = "Dynamisch") {

  invoercontrole(Basismodel, "basismodel")

  Rmse <- Basismodel %>%
    rowwise() %>%
    do_(
      ~rmse.basis(.$Model$data, "Basis")
    ) %>%
    ungroup()

  Hoogteschatting <- Basismodel %>%
    rowwise() %>%
    do_(
      ~hoogteschatting.basis(.$Model, .$Model$data, "Basis")
    ) %>%
    ungroup()

  Dataset <- Hoogteschatting %>%
    inner_join(
      Rmse %>%
        select(.data$BMS, .data$DOMEIN_ID, .data$rmseD, .data$maxResid),
      by = c("BMS", "DOMEIN_ID")
    )

  AfwijkendeMetingen <- afwijkendeMetingen(Dataset, AantalDomHogeRMSE)

  #afwijkende curves
  AfwijkendeCurves <- afwijkendeCurves(Basismodel)

  SlechtsteModellen <- AfwijkendeMetingen %>%
    filter(.data$HogeRmse & .data$Status != "Goedgekeurd") %>%
    select(.data$DOMEIN_ID, .data$BMS) %>%
    distinct() %>%
    mutate(
      Reden = "hoge RMSE"
    ) %>%
    bind_rows(
      AfwijkendeCurves
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
                   Bestandsnaam, TypeRapport)

  return(AfwijkendeMetingen)

}
