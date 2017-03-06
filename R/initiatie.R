#' Initiatiestap met opsplitsen van dataset op basis van aantal metingen per domein
#'
#' Deze functie bevat als initiële stap het berekenen van een aantal extra variabelen (bruikbaar interval (Q5 en Q95), Omtrek, logOmtrek, logOmtrek2, aantal metingen van bruikbaar interval en van interval > 0.5 m).  Bomen met omtrek > 2,3 m worden niet meegenomen voor de analyses en dus verwijderd uit de dataset.
#'
#' Daarna splitst de functie de data op in 3 delen die voor de verschillende analyses kunnen gebruikt worden:
#'
#' (1) boomsoorten waarvoor op minimum 6 domeinen veel metingen uitgevoerd zijn (> 50 metingen), op basis waarvan betrouwbare domeinmodellen en een betrouwbaar Vlaams model berekend kan worden (= basismodel),
#'
#' (2) domeinen met minder metingen (10 - 50 metingen) van boomsoorten waarvoor een Vlaams model berekend kan worden (dus boomsoorten die in dataset (1) voorkomen), op basis waarvan het Vlaams model verschoven kan worden om een domeinspecifiek te model te bekomen (= afgeleid model), en
#'
#' (3) domeinen met veel metingen voor een boomsoort (> 50 metingen) waarvan er te weinig domeinen (< 6) zijn met voldoende metingen om een Vlaams model op te stellen.  Voor deze boomsoort-domein-combinaties kan een domeinspecifiek model opgesteld worden (maar geen Vlaams model voor de boomsoort, dus voor domeinen met < 50 metingen kan hier geen model gemaakt worden)
#'
#' (4) metingen van de domein-boomsoort-combinaties die niet tot de 3 voorgaande categorieën behoren en waar dus geen model voor opgesteld kan worden.
#'
#' De grenswaarden 50 en 10 zijn gebaseerd op het aantal metingen binnen het interval 0,5 - 2,3 m en binnen het bruikbaar interval.  Bij de opsplitsing worden de data meteen gecleand, waarbij metingen met omtrek > 2,4 m en metingen buiten het bruikbaar interval sowieso weggelaten worden; voor het afgeleid model worden ook de metingen met omtrek <= 0,5 m weggelaten.
#'
#' @param Data dataframe met alle metingen waarop het model gebaseerd moet zijn (m.u.v. afgekeurde of te negeren metingen).  Velden DOMEIN_ID, BMS, C13, HOOGTE  evt. TYPE_METING en JAAR, die worden bij rmse.basis als groeperende variabelen gebruikt.
#' @param Uitzonderingen lijst met uitzonderingen op min. 50 en min. 10 bomen.  Velden DOMEIN_ID, BMS, min_basis (= vervangende waarde voor 50), min_afgeleid (= vervangende waarde voor 10)
#'
#' @return Een list van dataframes:
#'
#' - dataframe > 50 metingen en min. 6 domeinen
#'
#' - dataframe 10-50
#'
#' - dataframe > 50 metingen en < 6 domeinen
#'
#' - dataframe met metingen van domeinen en boomsoorten waar geen model voor opgesteld kan worden
#'
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ mutate_ group_by_ ungroup inner_join select_ distinct_ anti_join
#'

initiatie <-
  function(Data,
           Uitzonderingen = data.frame(DOMEIN_ID = "", BMS = "",
                                       min_basis = NA, min_afgeleid = NA,
                                       stringsAsFactors = FALSE)) {
  min_basismodel <- 50
  min_domeinen_basismodel <- 6   #maar 2-6 apart houden om hiervoor aparte fixed modellen te berekenen
  min_afgeleidmodel <- 10

  #hier moet nog controle gebeuren op de ingevoerde data!

  Data2 <- Data %>%
    filter_(~HOOGTE!=0) %>%
    mutate_(
      Omtrek = ~((C13 %/% 10) * 10 + 5)/100,
      Rijnr = ~seq_along(C13),   #geeft vector van lengte C13, dus nummert de rijen oplopend
      logOmtrek = ~log(Omtrek),
      logOmtrek2 = ~logOmtrek^2
    ) %>%
    filter_(
      ~Omtrek < 2.40
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    mutate_(
      nBomen = ~n(),
      Q5 = ~quantile(Omtrek, probs = 0.05) - 0.1,
      Q5k = ~(((Q5 * 100) %/% 10) * 10 + 5)/100,     #het klassemidden van Q5
      Q95 = ~quantile(Omtrek, probs = 0.95) + 0.1,
      Q95k = ~(((Q95 * 100) %/% 10) * 10 + 5)/100     #het klassemidden van Q95
    ) %>%
    ungroup() %>%
    filter_(
      ~Omtrek > Q5k - 0.05,
      ~Omtrek < Q95 + 0.05
    )

  Data.aantallen <- Data2 %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    summarise_(
      nBomenInterval = ~n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter_(
      ~Omtrek > 0.5 - 0.05,
      ~Omtrek > Q5k - 0.05,
      ~Omtrek < Q95k + 0.05
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID,
      ~nBomenInterval
    ) %>%
    summarise_(
      nBomenOmtrek05 = ~n()
    ) %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    left_join(
      Uitzonderingen,
      by = c("BMS", "DOMEIN_ID")
    )


  Data_Selectie_50 <- Data.aantallen %>%
    filter_(
      ~((nBomenOmtrek05 > min_basismodel & is.na(min_basis)) |
        (!is.na(min_basis) & nBomenOmtrek05 > min_basis))
    ) %>%
    select_(
      ~-min_basis, ~-min_afgeleid
    )


  Basisdata <- Data_Selectie_50 %>%
    select_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    distinct_() %>%
    group_by_(
      ~BMS
    ) %>%
    filter_(
      ~n() >= min_domeinen_basismodel
    ) %>%
    ungroup() %>%
    inner_join(
      Data_Selectie_50,
      by = c("DOMEIN_ID", "BMS")
    )


  Extradata <- Data_Selectie_50 %>%
    filter_(
      ~!BMS %in% unique(Basisdata$BMS)
    )


  Data.afgeleid <- Data.aantallen %>%
    filter_(
      ~BMS %in% unique(Basisdata$BMS)
    ) %>%
    anti_join(
      Basisdata %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter_(
      ~((nBomenOmtrek05 > min_afgeleidmodel & is.na(min_afgeleid)) |
        (!is.na(min_afgeleid) & nBomenOmtrek05 > min_afgeleid)),
      ~Omtrek > 0.5 - 0.05
    ) %>%
    select_(
      ~-min_basis, ~-min_afgeleid
    )

  Data.rest <- Data.aantallen %>%
    anti_join(
      Data_Selectie_50 %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    anti_join(
      Data.afgeleid %>%
        select_(~BMS, ~DOMEIN_ID) %>%
        distinct_(),
      by = c("BMS", "DOMEIN_ID")
    )


  return(list(Basisdata, Data.afgeleid, Extradata, Data.rest))
}


