#' Hoogteschatting op basis van opgegeven basismodel
#'
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves en Vlaamse curves van het opgegeven basismodel.  De teruggegeven dataframe kan gebruikt worden om grafieken te maken of afwijkende metingen te bestuderen.
#'
#' @param Basismodel model per boomsoort
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes voor het domeinmodel en de Vlaamse model
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ mutate_
#' @importFrom nlme fixef
#' @importFrom stats predict
#'

hoogteschatting.basis <- function(Basismodel) {

  Schatting <- data.frame(NULL)
  for (Boomsoort in Basismodel$BMS) {
    #dataset ophalen uit model
    Soortmodel <- (Basismodel %>% filter_(~ BMS == Boomsoort))$Model[[1]]

    Schatting.soort <- Soortmodel$data %>%
      mutate_(
        H_D_finaal = ~predict(Soortmodel, newdata = .),
        H_VL_finaal = ~as.numeric(fixef(Soortmodel)[1]) +
          as.numeric(fixef(Soortmodel)[2]) * logOmtrek +
          as.numeric(fixef(Soortmodel)[3]) * logOmtrek2
      )

    Schatting <- Schatting %>%
      bind_rows(data.frame(BMS = Boomsoort, Schatting.soort,
                           stringsAsFactors = FALSE))
  }

  return(Schatting)
}
