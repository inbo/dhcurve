#' Validatie van het afgeleid model
#'
#' Functie die de validatie uitvoert op het verschoven Vlaams model. Vermits fit.afgeleid geen echte modelfit is en je deze validatiestap normaliter niet moet overslaan (na de laatste validatie exporteer je logischerwijs direct de gegevens), zou evt. overwogen kunnen worden om de voorgaande (fit.afgeleid) en deze stap (validatie.afgeleid) samen te nemen.  Een andere mogelijke piste is om Rmse.afgeleid toe te voegen aan fit.afgeleid.
#'
#' validatie.afgeleid roept meerdere hulpfuncties op:
#'
#' - rmse.afgeleid
#'
#' - afwijkendeMetingen
#'
#' - validatierapport
#'
#' Voorafgaand aan het uitvoeren van deze laatste functie worden eerst de slechtste modellen opgelijst (op basis van rmse en afwijkende metingen).
#'
#' @param Basismodel model per boomsoort
#' @param Afgeleidmodel verschuiving per boomsoort en domein (verschoven Vlaams model)
#' #@param Data.afgeleid dataframe 10-50
#'
#' @inheritParams afwijkendeMetingen
#' @inheritParams validatierapport
#'
#' @return Dataframe met te controleren metingen en document (html/pdf) met te controleren curves (incl. aantal metingen per curve) en grafieken van te controleren metingen
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ rowwise do_ select_ distinct_ mutate_ bind_rows group_by_ summarise_ ungroup inner_join
#'

validatie.afgeleid <-
  function(Basismodel, Afgeleidmodel, AantalDomHogeRMSE = 20,
           Bestandsnaam = "Validatie_Afgeleid.html", TypeRapport = "Dynamisch"){

  Model <- Afgeleidmodel[[1]]

  #Rmse van Vlaams model berekenen
  RmseVL <- Basismodel %>%
    filter_(~BMS %in% unique(Model$BMS)) %>%
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
      nBomen = ~sum(nBomen),
      nBomenInterval = ~sum(nBomenInterval),
      nBomenOmtrek05VL = ~sum(nBomenOmtrek05),
      rmseVL = ~sqrt(sum(sseVL) / (nBomenOmtrek05VL - 2))
    ) %>%
    ungroup()

  #Rmse van afgeleid model berekenen en combineren met die van Vlaams model
  Rmse <- Model %>%
    rowwise() %>%
    do_(
      ~rmse.afgeleid(.$Model, .$BMS, .$DOMEIN_ID)
    ) %>%
    ungroup() %>%
    inner_join(
      RmseVL %>% select_(~BMS, ~rmseVL),
      by = c("BMS")
    ) %>%
    mutate_(
      rmseD = ~sqrt(rmseVL ^ 2 + RmseVerschuiving ^ 2)
    )


  Hoogteschatting <- Model %>%
    inner_join(
      Afgeleidmodel[[2]],
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    do_(
      ~hoogteschatting.afgeleid(.$Model[[1]],
                                select_(., ~-Model))
    ) %>%
    ungroup() %>%
    mutate_(
      ResidD2 = ~ (HOOGTE - H_D_finaal) ^ 2
    )

  Dataset <- Hoogteschatting %>%
    select_(~BMS, ~DOMEIN_ID, ~ResidD2) %>%
    filter_(~!is.na(ResidD2)) %>%
    group_by_(~BMS, ~DOMEIN_ID) %>%
    summarise_(
      maxResid = ~max(c(ResidD2))
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

  SlechtsteModellen <- AfwijkendeMetingen %>%
    filter_(~HogeRmse & Status != "Goedgekeurd") %>%
    select_(~DOMEIN_ID, ~BMS) %>%
    distinct_() %>%
    mutate_(
      Reden = ~"hoge RMSE"
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
    group_by_(
      ~BMS, ~DOMEIN_ID
    ) %>%
    summarise_(
      Reden = ~paste(Reden, collapse = ", ")
    ) %>%
    ungroup()

  validatierapport(SlechtsteModellen, AfwijkendeMetingen, Dataset,
                   Bestandsnaam, TypeRapport)

  return(AfwijkendeMetingen)

}
