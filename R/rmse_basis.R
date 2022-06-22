#' @title Berekent RMSE van basismodel
#'
#' @description
#' Deze functie berekent de rmse door cross-validatie op basis van 6 subsets.
#' Deze functie kan ook gebruikt worden voor het lokaal model (ze bepaalt het
#' verschil tussen de datasets op basis van het al dan niet aanwezig zijn van
#' een veld DOMEIN_ID in de dataset).  Opgelet!  In tegenstelling tot de meeste
#' functies van dit package werkt deze functie op basis van de meetgegevens van
#' 1 model.  Zie voorbeelden voor een methode om deze functie te kunnen
#' toepassen vertrekkend van meetgegevens (bv. Data.lokaal) of vertrekkend van
#' een model waar de meetgegevens uit gehaald kunnen worden (bv. Basismodel).
#'
#' Deze functie berekent de rmse op basis van testgroepen en omvat de volgende
#' deelstappen:
#'
#' - metingen opdelen in 6 testgroepen (veld testgroep)
#'
#' - modellen fitten voor testgroepen, waarbij ze de functie fit.basis 6 keer
#' oproept
#'
#' - rmse berekenen voor domeinmodellen en Vlaams model op basis van
#' testgroep-modellen
#'
#' @param Data Meetgegevens van één boomsoort-domein-combinatie (dataframe
#' zoals de dataframes die in de list teruggegeven worden door de functie
#' initiatie)
#' @param Typemodel 'Basis' of 'Lokaal'?
#' @param BMS Boomsoort
#'
#' @return Dataframe met rmse_domein (rmseD), rmse_Vlaams (rmseVL, niet voor
#' lokaal model) en maxResid
#'
#' @examples
#' library(dplyr)
#'
#' #Dataset inladen voor het basismodel
#' Data <- testdataset()
#' Datalijst <- initiatie(Data)
#' Data.basis <- Datalijst[["Basis"]]
#'
#' #De rmse berekenen voor een basismodel op basis van de dataset
#' Data.basis %>%
#'   group_by(
#'     BMS
#'   ) %>%
#'   do(
#'     rmse.basis(., "Basis", .data$BMS)
#'   ) %>%
#'   ungroup()
#'
#' #Dataset inladen voor het lokaal model
#' Data.lokaal <- Data.basis %>%
#'   filter(DOMEIN_ID == "A")
#'
#' #De rmse berekenen voor een lokaal model
#' Data.lokaal %>%
#'   group_by(
#'     BMS,
#'     DOMEIN_ID
#'   ) %>%
#'   do(
#'     rmse.basis(., "Lokaal", .data$BMS)
#'   ) %>%
#'   ungroup()
#'
#' @export
#'
#' @importFrom dplyr %>% group_by ungroup transmute mutate bind_rows
#' summarise arrange row_number
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom nlme fixef
#' @importFrom stats predict
#' @importFrom assertthat assert_that
#'

rmse.basis <- function(Data, Typemodel, BMS) {

  #controle
  assert_that(is.character(Typemodel))
  Typemodel <- tolower(Typemodel)
  assert_that(Typemodel %in% c("basis", "lokaal"))

  invoercontrole(Data, "fit")
  if (Typemodel == "lokaal") {
    assert_that(length(unique(Data$DOMEIN_ID)) == 1,
                msg = "Voor een lokaal model mag de dataset Data maar 1
                domein bevatten")
  }

  #testgroepen aanmaken in dataset
  Soortdata <- Data %>%
    arrange(.data$DOMEIN_ID, .data$Omtrek, .data$HOOGTE) %>%
    mutate(
      Testgroep = (row_number(.data$DOMEIN_ID) - 1) %% 6 + 1
    ) %>%
    mutate(BMS = BMS)


  #model fitten voor de 6 testgroepen
  Data_result <- data.frame(NULL)
  for (i in 1:6) {
    Data_test <- Soortdata[Soortdata$Testgroep == i, ]
    Data_model <- Soortdata[Soortdata$Testgroep != i, ]

    if (grepl(Typemodel, "lokaal")) {
      Model <- fit.lokaal(Data_model)$Model[[1]]  #nolint
    } else {
      Model <- fit.basis(Data_model)$Model[[1]]   #nolint
    }

    Data_Boomsoort <- Data_test %>%
      mutate(
        H_Dmodel = predict(Model, newdata = .),
        ResidD = .data$HOOGTE - .data$H_Dmodel,
        ResidD2 = .data$ResidD ^ 2,
        ResidVL2 = 0
      )

    if (grepl(Typemodel, "basis")) {
      Data_Boomsoort <- Data_Boomsoort %>%
        mutate(
          H_VLmodel = as.numeric(fixef(Model)[1]) +
            as.numeric(fixef(Model)[2]) * .data$logOmtrek +
            as.numeric(fixef(Model)[3]) * .data$logOmtrek2,
          ResidVL = .data$HOOGTE - .data$H_VLmodel,
          ResidVL2 = .data$ResidVL ^ 2
        )
    }

    Data_result <- Data_result %>%
      bind_rows(Data_Boomsoort)
  }


  #rmse berekenen
  Rmse.soort <- Data_result[Data_result$Omtrek > 0.50, ] %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID,
      .data$nBomen,
      .data$nBomenInterval,
      .data$nBomenOmtrek05,
      .data$Q5k,
      .data$Q95k
    ) %>%
    summarise(
      sseD = sum(c(.data$ResidD2)),
      sseVL = sum(c(.data$ResidVL2)),
      maxResid = max(c(.data$ResidD2))
    ) %>%
    ungroup() %>%
    transmute(
      .data$BMS,
      .data$DOMEIN_ID,
      .data$nBomen,
      .data$nBomenInterval,
      .data$nBomenOmtrek05,
      .data$Q5k,
      .data$Q95k,
      rmseD = sqrt(.data$sseD / (.data$nBomenOmtrek05 - 2)),
      rmseVL = sqrt(.data$sseVL / (.data$nBomenOmtrek05 - 2)),
      .data$maxResid
    )

  #voor lokaal model het Vlaams model verwijderen (is gelijkgesteld aan 0)
  if (grepl(Typemodel, "lokaal")) {
    Rmse.soort$rmseVL <- NULL
  }

  return(Rmse.soort)
}
