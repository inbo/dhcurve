#' Validatie van het afgeleid model
#'
#' Functie die de validatie uitvoert op het verschoven Vlaams model en een
#' overzicht geeft van de afwijkende metingen (zodat de gebruiker deze kan
#' valideren).
#'
#' validatie.afgeleid roept meerdere hulpfuncties op:
#'
#' - rmse.basis en rmse.verschuiving
#'
#' - afwijkendeMetingen
#'
#' - validatierapport
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
#'
#' @return De functie genereert een validatierapport (html-bestand) in de
#' working directory met informatie en grafieken van de te controleren metingen.
#' De afwijkende metingen zijn in rood aangeduid (zie ?validatierapport of
#' vignette voor meer informatie).
#'
#' De functie geeft een dataframe terug met de te controleren metingen, met
#' behalve de informatie uit de databank een aantal berekende waarden:
#'
#' - H_D_finaal: een geschatte hoogte voor de omtrekklasse volgens het
#' domeinmodel
#'
#' - rsmeD: de foutenschatting voor het domeinmodel
#'
#' - H_VL_finaal: een geschatte hoogte voor de omtrekklasse volgens het Vlaams
#' model waarvan het domeinmodel afgeleid is
#'
#' - rmseVL: de foutenschatting voor dit Vlaams model
#'
#' - HogeRmse: TRUE als het domeinmodel een hoge rmse heeft, anders NA
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ rowwise do_ select_ distinct_ mutate_ bind_rows
#' group_by_ summarise_ ungroup inner_join
#' @importFrom assertthat assert_that is.count
#'

validatie.afgeleid <-
  function(Basismodel, Afgeleidmodel, AantalDomHogeRMSE = 20,
           Bestandsnaam = "Validatie_Afgeleid.html",
           TypeRapport = "Dynamisch") {

  invoercontrole(Basismodel, "basismodel")
  invoercontrole(Afgeleidmodel, "afgeleidmodel")

  AModel <- Afgeleidmodel[[1]]

  #Rmse van Vlaams model berekenen
  RmseVL <- Basismodel %>%
    filter_(~BMS %in% unique(AModel$BMS)) %>%
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

  #Rmse van verschuiving berekenen en combineren met die van Vlaams model
  Rmse <- AModel %>%
    rowwise() %>%
    do_(
      ~rmse.verschuiving(.$Model, .$BMS, .$DOMEIN_ID)
    ) %>%
    ungroup() %>%
    inner_join(
      RmseVL %>% select_(~BMS, ~rmseVL),
      by = c("BMS")
    ) %>%
    mutate_(
      rmseD = ~sqrt(rmseVL ^ 2 + RmseVerschuiving ^ 2)
    )


  Hoogteschatting <- AModel %>%
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
