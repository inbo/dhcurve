#' Initiatiestap met opsplitsen van dataset op basis van aantal metingen per domein
#'
#' Deze functie bevat als initiële stap het berekenen van een aantal extra variabelen (bruikbaar interval (Q5 en Q95), Omtrek, logOmtrek, logOmtrek2, aantal metingen van bruikbaar interval en van interval > 0.5 m).  Bomen met omtrek > 2,3 m worden niet meegenomen voor de analyses en dus verwijderd uit de dataset.
#'
#' Daarna splitst de functie de data op in 3 delen die voor de verschillende analyses kunnen gebruikt worden: (1) domeinen met veel metingen voor de boomsoort (> 50) op basis waarvan domeinmodellen en een Vlaams model berekend kan worden (= basismodel), (2) domeinen met minder metingen (10 - 50) op basis waarvan het Vlaams model verschoven kan worden om een domeinspecifiek te model te bekomen (= afgeleid model), en (3) domeinen waarvoor te weinig metingen zijn om een domeinspecifiek model te berekenen.  De grenswaarden 50 en 10 zijn gebaseerd op het aantal metingen binnen het interval 0,5 - 2,3 m.
#'
#' @param Data dataframe met alle metingen waarop het model gebaseerd moet zijn (m.u.v. afgekeurde of te negeren metingen).  Velden DOMEIN_ID, BMS, C13, HOOGTE  evt. TYPE_METING en JAAR, die worden bij rmse.basis als groeperende variabelen gebruikt.
#' @param Uitzonderingen lijst met uitzonderingen op min. 50 en min. 10 bomen.  Velden DOMEIN_ID, BMS, min_basis, min_afgeleid
#'
#' @return Een list van dataframes:
#'
#' - dataframe > 50 metingen en min. 6 domeinen
#'
#' - dataframe 10-50
#'
#' - dataframe > 50 metingen en < 6 domeinen
#'
#' - dataframe met lijst van domeinen < 10 per boomsoort (nog toe te voegen)
#'
#' - evt. lijst van boomsoorten die niet gefit kunnen worden (nog toe te voegen)
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ mutate_ group_by_ ungroup inner_join select_ distinct_ anti_join
#'

initiatie <- function(Data, Uitzonderingen = NULL) {
  min_basismodel <- 50
  min_domeinen_basismodel <- 6   #maar 2-6 apart houden om hiervoor aparte fixed modellen te berekenen?
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
      Q95 = ~quantile(Omtrek, probs = 0.95) + 0.1
    ) %>%
    ungroup()


  Data_Selectie_50 <- Data2 %>%
    filter_(
      ~Omtrek > 0.5
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    filter_(
      ~n() > min_basismodel
    ) %>%
    dplyr::summarise() %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("DOMEIN_ID", "BMS")
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


  Data.afgeleid <- Data2 %>%
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
      ~Omtrek > 0.5
    ) %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID
    ) %>%
    filter_(
      ~n() > min_afgeleidmodel
    ) %>%
    dplyr::summarise() %>%
    ungroup() %>%
    inner_join(
      Data2,
      by = c("DOMEIN_ID", "BMS")
    )

  return(list(Basisdata, Data.afgeleid, Extradata))
}


