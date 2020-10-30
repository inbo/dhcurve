#' Validatie van het lokaal model
#'
#' Functie die alle nodige validaties uitvoert op het opgegeven lokaal model en
#' een overzicht geeft van de afwijkende metingen en slechte curves (zodat de
#' gebruiker deze kan valideren).
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
#' @param Lokaalmodel Model per boomsoort-domeincombinatie zoals teruggegeven
#' door de functie fit.lokaal: tibble met de velden BMS (boomsoort), DOMEIN_ID
#' en Model (lm-object met het gefit lineair model voor die
#' boomsoort-domeincombinatie).
#' @param Data Dataset op basis waarvan het opgegeven lokaal model berekend is.
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
#' domeinmodel
#'
#' - rmseD: de foutenschatting voor het domeinmodel
#'
#' - HogeRmse: TRUE als het domeinmodel een hoge rmse heeft, anders NA
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join filter select mutate distinct group_by
#' summarise ungroup bind_rows do rowwise
#' @importFrom rlang .data
#' @importFrom assertthat assert_that has_name is.count
#'

validatie.lokaal <-
  function(Lokaalmodel, Data, AantalDomHogeRMSE = 20,
           Bestandsnaam = "Default", TypeRapport = "Dynamisch") {


  invoercontrole(Lokaalmodel, "lokaalmodel")
  invoercontrole(Data, "fit")

  Rmse <- Data %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID
    ) %>%
    do_(
      ~rmse.basis(., "Lokaal")
    ) %>%
    ungroup()

  Hoogteschatting <- Lokaalmodel %>%
    inner_join(
      Data,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID
    ) %>%
    do_(
      ~hoogteschatting.basis(.$Model[[1]],
                              select_(., ~-Model),
                              "Lokaal")
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
  AfwijkendeCurves <- afwijkendeCurves(Lokaalmodel, Data)

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
      Omtrek_Extr_Hoogte.d = ifelse(is.na(.data$Omtrek_Extr_Hoogte.d), "",
                                    .data$Omtrek_Extr_Hoogte.d)
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
                         "Validatie_Lokaal.html",
                         Bestandsnaam)
  validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                   Bestandsnaam, TypeRapport)

  return(AfwijkendeMetingen)

}
