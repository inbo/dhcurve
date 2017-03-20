#' RMSE berekenen van afgeleid model (verschoven Vlaams model)
#'
#' deze functie berekent de rmse op basis van verschil tussen geschatte domeinwaarde en gemeten waarde (voor omtrekklassen > 0.5 m)
#'
#'
#' @param Verschovenmodel afgeleid model per boomsoort-domein-combinatie
#' @param Boomsoort BMS
#' @param Domein DOMEIN_ID
#'
#' @return dataframe met rmse_verschuiving per boomsoort en domein
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_ summarise_ select_
#' @importFrom stats influence
#'

rmse.afgeleid <- function(Verschovenmodel, Boomsoort, Domein){

  Rmse <- data.frame(RMSE = influence(Verschovenmodel)$sigma) %>%
    mutate_(
      RMSE2 = ~RMSE ^ 2
    ) %>%
    summarise_(
      RMSE2 = ~sum(RMSE2),
      nBomenModel = ~n()
    ) %>%
    mutate_(
      RmseVerschuiving = ~sqrt(RMSE2 / (nBomenModel - 2)),
      BMS = ~Boomsoort,
      DOMEIN_ID = ~Domein
    ) %>%
    select_(~-RMSE2)

}
