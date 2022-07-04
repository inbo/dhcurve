#' @title Hoogteschatting op basis van opgegeven basismodel
#'
#' @description
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves
#' en Vlaamse curves van het opgegeven basismodel.  De teruggegeven dataframe
#' kan gebruikt worden om grafieken te maken of afwijkende metingen te
#' bestuderen.  Opgelet!  In tegenstelling tot de meeste functies van dit
#' package werkt deze functie op basis van 1 model en de bijhorende
#' meetgegevens.  Zie voorbeelden voor een methode om deze functie te kunnen
#' toepassen op Basismodel of Lokaalmodel + `Data.lokaal`.
#'
#' @param Soortmodel model voor één boomsoort (basis) of één
#' boomsoort-domeincombinatie (lokaal)
#' @param Soortdata meetgegevens van boomsoort (basis) of
#' boomsoort-domeincombinatie (lokaal)
#' @param Typemodel "Basis" of "Lokaal"?
#' @param BMS Boomsoort
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes
#' voor het domeinmodel en de Vlaamse model
#'
#' @examples
#' library(dplyr)
#'
#' #Dataset inladen en het basismodel berekenen
#' Data <- testdataset()
#' Datalijst <- initiatie(Data)
#' Data.basis <- Datalijst[["Basis"]]
#' Basismodel <- fit.basis(Data.basis)
#'
#' #De hoogteschatting voor een basismodel
#' Basismodel %>%
#'   rowwise() %>%
#'   do(
#'     hoogteschatting.basis(.$Model, .$Model$data, "Basis", .$BMS)
#'   ) %>%
#'   ungroup()
#'
#' #Dataset inladen en het lokaal model berekenen
#' Data.lokaal <- Data.basis %>%
#'   filter(DOMEIN_ID == "A")
#' Lokaalmodel <- fit.lokaal(Data.lokaal)
#'
#' #De hoogteschatting voor een lokaal model
#' Lokaalmodel %>%
#'   inner_join(
#'     Data.lokaal,
#'     by = c("BMS", "DOMEIN_ID")
#'   ) %>%
#'   group_by(
#'     BMS,
#'     DOMEIN_ID
#'   ) %>%
#'   do(
#'     hoogteschatting.basis(.$Model[[1]],
#'                            select(., -Model),
#'                            "Lokaal", .$BMS)
#'   ) %>%
#'   ungroup()
#'
#'
#' @export
#'
#' @importFrom dplyr %>% filter mutate select distinct full_join
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom nlme fixef
#' @importFrom stats predict
#' @importFrom assertthat assert_that has_name
#'

hoogteschatting.basis <- function(Soortmodel, Soortdata, Typemodel, BMS) {

  #controle invoer
  assert_that(is.character(Typemodel))
  Typemodel <- tolower(Typemodel)
  assert_that(Typemodel %in% c("basis", "lokaal"))


  invoercontrole(Soortdata, "fit")
  if (Typemodel == "lokaal") {
    assert_that(length(unique(Soortdata$DOMEIN_ID)) == 1,
                msg = "Voor een lokaal model mag de dataset Soortdata maar 1
                domein bevatten")
    assert_that(inherits(Soortmodel, "lm"),
                msg = "Soortmodel moet een lineair model zijn (zie
                documentatie)")
  } else {
    assert_that(inherits(Soortmodel, "lme"),
                msg = "Soortmodel moet een lineair mixed model zijn (zie
                documentatie)")
  }
  if (has_name(Soortdata, "BMS")) {
    Soortdata <- Soortdata %>%
      select(-.data$BMS)
  }


  #Hoogtes schatten voor alle omtrekklassen binnen bruikbaar interval
  AlleKlassen <- seq(15, 245, 10)

  Schatting.soort <- Soortdata %>%
    select(.data$DOMEIN_ID, .data$BOS_BHI, .data$nBomenInterval,
           .data$nBomenOmtrek05, .data$nBomen, .data$Q5k, .data$Q95k) %>%
    distinct()

  Schatting.soort <- merge(Schatting.soort, AlleKlassen) %>%
    filter(
      .data$y >= (100 * .data$Q5k) - 1,
      .data$y <= (100 * .data$Q95k) + 1
    ) %>%
    mutate(
      Omtrek = .data$y / 100,
      logOmtrek = log(.data$Omtrek),
      logOmtrek2 = .data$logOmtrek ^ 2
    ) %>%
    mutate(
      H_D_finaal = predict(Soortmodel, newdata = .)
    )

  #Voor basismodel ook hoogtes voor Vlaams model schatten
  if (grepl(Typemodel, "basis")) {
    Schatting.soort <- Schatting.soort %>%
      mutate(
        H_VL_finaal = as.numeric(fixef(Soortmodel)[1]) +
          as.numeric(fixef(Soortmodel)[2]) * .data$logOmtrek +
          as.numeric(fixef(Soortmodel)[3]) * .data$logOmtrek2
      )
  }

  #resultaten koppelen aan dataset met meetgegevens
  Schatting.soort <- Schatting.soort %>%
    select(-.data$logOmtrek, -.data$logOmtrek2) %>%
    full_join(
      Soortdata %>%
        mutate(y = as.integer(round(100 * .data$Omtrek))) %>%
        select(-.data$Omtrek),
      by = c("DOMEIN_ID", "BOS_BHI", "nBomenInterval",
             "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "y")
    ) %>%
    select(-.data$y) %>%
    mutate(
      BMS = BMS,
      IDbms =
        ifelse(
          is.na(.data$IDbms),
          max(Soortdata$IDbms, na.rm = TRUE),
          .data$IDbms
        )
    )

  return(Schatting.soort)
}
