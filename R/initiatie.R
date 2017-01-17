#' Initiatiestap met opsplitsen van dataset op basis van aantal metingen per domein
#'
#' Deze functie bevat als initiële stap het berekenen van een aantal extra variabelen (bruikbaar interval (Q5 en Q95), Omtrek, logOmtrek, logOmtrek2, aantal metingen van bruikbaar interval en van interval > 0.5 m).  Bomen met omtrek > 2,3 m worden niet meegenomen voor de analyses en dus verwijderd uit de dataset.
#'
#' Daarna splitst de functie de data op in 3 delen die voor de verschillende analyses kunnen gebruikt worden: (1) domeinen met veel metingen voor de boomsoort (> 50) op basis waarvan domeinmodellen en een Vlaams model berekend kan worden (= basismodel), (2) domeinen met minder metingen (10 - 50) op basis waarvan het Vlaams model verschoven kan worden om een domeinspecifiek te model te bekomen (= afgeleid model), en (3) domeinen waarvoor te weinig metingen zijn om een domeinspecifiek model te berekenen.  De grenswaarden 50 en 10 zijn gebaseerd op het aantal metingen binnen het interval 0,5 - 2,3 m.
#'
#' @param Data dataframe met alle metingen waarop het model gebaseerd moet zijn (m.u.v. afgekeurde of te negeren metingen).  Velden DOMEIN_ID, BMS, C13, HOOGTE
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
}


