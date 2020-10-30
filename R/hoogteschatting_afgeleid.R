#' Hoogteschatting op basis van opgegeven afgeleid model
#'
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves
#' en Vlaamse curves van het opgegeven afgeleid model.  De teruggegeven
#' dataframe kan gebruikt worden om grafieken te maken of afwijkende metingen
#' te bestuderen.  Opgelet!  In tegenstelling tot de meeste functies van dit
#' package werkt deze functie op basis van 1 model en de bijhorende
#' meetgegevens.  Zie voorbeeld voor een methode om deze functie te kunnen
#' toepassen op de volledige dataset Afgeleidmodel.
#'
#' @param Domeinsoortmodel verschoven Vlaams model voor 1
#' boomsoort-domein-combinatie
#' @param Domeinsoortdata de gegevens die hierbij horen: meetresultaten voor 1
#' boomsoort-domein-combinatie
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes
#' voor het domeinmodel en de Vlaamse model
#'
#' @examples
#' library(dplyr)
#'
#' #Datasets inladen en het basismodel en afgeleid model berekenen
#' Data <- testdataset()
#' Datalijst <- initiatie(Data)
#' Data.basis <- Datalijst[["Basis"]]
#' Basismodel <- fit.basis(Data.basis)
#' Data.afgeleid <- Datalijst[["Afgeleid"]]
#' Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)
#'
#' #De hoogteschatting voor een afgeleid model
#' Afgeleidmodel[[1]] %>%
#'   inner_join(
#'     Afgeleidmodel[[2]],
#'     by = c("BMS", "DOMEIN_ID")
#'   ) %>%
#'   group_by(
#'     .data$BMS,
#'     .data$DOMEIN_ID
#'   ) %>%
#'   do(
#'     hoogteschatting.afgeleid(.$Model[[1]],
#'                               select(., -.data$Model))
#'   ) %>%
#'   ungroup()
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @importFrom stats predict
#' @importFrom assertthat assert_that
#'

hoogteschatting.afgeleid <- function(Domeinsoortmodel, Domeinsoortdata) {

  #controle invoer
  assert_that(inherits(Domeinsoortmodel, "lm"),
              msg = "Domeinsoortmodel moet een lineair model zijn (zie
              documentatie)")
  invoercontrole(Domeinsoortdata, "afgeleidedata")
  assert_that(length(unique(Domeinsoortdata$BMS)) == 1,
              msg = "De dataset Domeinsoortdata mag maar 1 boomsoort bevatten")
  assert_that(length(unique(Domeinsoortdata$DOMEIN_ID)) == 1,
              msg = "De dataset Domeinsoortdata maar 1 domein bevatten")

  Schatting <- Domeinsoortdata %>%
    mutate(
      H_D_finaal = predict(Domeinsoortmodel, newdata = .)
    )

  return(Schatting)
}
