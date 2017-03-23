#' Hoogteschatting op basis van opgegeven afgeleid model
#'
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves en Vlaamse curves van het opgegeven afgeleid model.  De teruggegeven dataframe kan gebruikt worden om grafieken te maken of afwijkende metingen te bestuderen.
#'
#' @param Afgeleidmodel verschoven Vlaams model voor 1 boomsoort-domein-combinatie
#' @param DataAfgeleidmodel de gegevens die hierbij horen: meetresultaten en het berekend Vlaams model (Deze dataset is het tweede item van de list die teruggegeven wordt bij de functie fit.afgeleid)
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes voor het domeinmodel en de Vlaamse model
#'
#' @export
#'
#' @importFrom dplyr %>% mutate_
#' @importFrom stats predict
#'

hoogteschatting.afgeleid <- function(Afgeleidmodel, DataAfgeleidmodel) {

  Schatting <- DataAfgeleidmodel %>%
    mutate_(
      H_D_finaal = ~predict(Afgeleidmodel, newdata = .)
    )

  return(Schatting)
}
