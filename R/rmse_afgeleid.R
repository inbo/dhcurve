#' RMSE berekenen van afgeleid model (verschoven Vlaams model)
#'
#' deze functie berekent de rmse op basis van verschil tussen geschatte domeinwaarde en gemeten waarde (voor omtrekklassen > 0.5 m)
#'
#' Opgelet!  In tegenstelling tot de meeste functies van dit package werkt deze functie op basis van de meetgegevens van 1 model.  Zie voorbeeld voor een methode om deze functie te kunnen toepassen vertrekkend van het berekende model.
#'
#'
#' @param Verschovenmodel afgeleid model per boomsoort-domein-combinatie
#' @param Boomsoort BMS
#' @param Domein DOMEIN_ID
#'
#' @return dataframe met rmse_verschuiving per boomsoort en domein
#'
#' @examples
#' library(dplyr)
#' #nog datasets toevoegen om deze voorbeelden te kunnen runnen
#' \dontrun{
#' Afgeleidmodel[[1]] %>%
#'   rowwise() %>%
#'   do_(
#'     ~rmse.afgeleid(.$Model, .$BMS, .$DOMEIN_ID)
#'   ) %>%
#'   ungroup()
#' #Nota: voor een berekening van de volledige RMSE van dit afgeleid model
#' #moet ook de RMSE van het basismodel in rekening gebracht worden
#' }
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

  return(Rmse)

}
