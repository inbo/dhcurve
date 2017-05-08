#' Validatie van het basismodel
#'
#' Functie die alle nodige validaties uitvoert op het opgegeven (volledige) model en een overzicht geeft van de afwijkende metingen en slechte curves (zodat de gebruiker deze kan valideren). De functie roept meerdere hulpfuncties op:
#'
#' - rmse.basis
#'
#' - afwijkendeMetingen
#'
#' - afwijkendeCurves
#'
#' - validatierapport
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de slechtste modellen opgelijst (op basis van rmse, afwijkende metingen en afwijkende curves).
#'
#' Deze functie kan ook gebruikt worden voor lokale modellen
#'
#' @param Basismodel model per boomsoort
#' @param Data dataset op basis waarvan het model berekend is (nodig voor lokaal model)
#'
#' @inheritParams afwijkendeMetingen
#' @inheritParams validatierapport
#'
#' @return Dataframe met te controleren metingen en document (html/pdf) met te controleren curves (incl. aantal metingen per curve) en grafieken van te controleren metingen
#'
#' @export
#'
#' @importFrom dplyr %>% inner_join filter_ select_ mutate_ distinct_ group_by_ summarise_ ungroup bind_rows do_ rowwise
#' @importFrom assertthat has_name
#'

validatie.basis <- function(Basismodel, Data = NULL, AantalDomHogeRMSE = 20,
                            TypeRapport = "Dynamisch"){

  if (has_name(Basismodel, "DOMEIN_ID")) {
    Rmse <- Data %>%
      group_by_(
        ~BMS,
        ~DOMEIN_ID
      ) %>%
      do_(
        ~rmse.basis(., "Lokaal")
      ) %>%
      ungroup()
  } else {
    Rmse <- Basismodel %>%
      rowwise() %>%
      do_(
        ~rmse.basis(.$Model$data, "Basis")
      ) %>%
      ungroup()
  }

  if (has_name(Basismodel, "DOMEIN_ID")) {
    Hoogteschatting <- Basismodel %>%
      inner_join(
        Data,
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
      ungroup()
  } else {
    Hoogteschatting <- Basismodel %>%
      rowwise() %>%
      do_(
        ~hoogteschatting.basis(.$Model, .$Model$data, "Basis")
      ) %>%
      ungroup()
  }

  Dataset <- Hoogteschatting %>%
    inner_join(Rmse %>% select_(~BMS, ~DOMEIN_ID, ~rmseD, ~maxResid),
               by = c("BMS", "DOMEIN_ID"))

  AfwijkendeMetingen <- afwijkendeMetingen(Dataset, AantalDomHogeRMSE)

  #afwijkende curves
  AfwijkendeCurves <- afwijkendeCurves(Basismodel, Data)

  SlechtsteModellen <- AfwijkendeMetingen %>%
    filter_(~HogeRmse & Status != "Goedgekeurd") %>%
    select_(~DOMEIN_ID, ~BMS) %>%
    distinct_() %>%
    mutate_(
      Reden = ~"hoge RMSE"
    ) %>%
    bind_rows(
      AfwijkendeCurves
    ) %>%
    bind_rows(
      AfwijkendeMetingen %>%
        filter_(
          ~Status != "Goedgekeurd"
        ) %>%
        select_(
          ~BMS, ~DOMEIN_ID
        ) %>%
        distinct_() %>%
        mutate_(
          Reden = ~"afwijkende metingen"
        )
    ) %>%
    mutate_(
      Omtrek_Buigpunt.d =
        ~ifelse(is.na(Omtrek_Buigpunt.d), "", Omtrek_Buigpunt.d),
      Omtrek_Extr_Hoogte.d = ~ifelse(is.na(Omtrek_Extr_Hoogte.d), "",
                                     Omtrek_Extr_Hoogte.d)
    ) %>%
    group_by_(
      ~BMS, ~DOMEIN_ID
    ) %>%
    summarise_(
      Reden = ~paste(Reden, collapse = ", "),
      Omtrek_Buigpunt = ~as.numeric(paste(Omtrek_Buigpunt.d, collapse = "")),
      Omtrek_Extr_Hoogte =
        ~as.numeric(paste(Omtrek_Extr_Hoogte.d, collapse = ""))
    ) %>%
    ungroup()

  if (has_name(Basismodel, "DOMEIN_ID")) {
    validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                     "Validatie_Lokaal.html", TypeRapport)
  } else {
    validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                     "Validatie_Basis.html", TypeRapport)
  }


  return(AfwijkendeMetingen)

}
