#' Berekent RMSE van basismodel
#'
#' Deze functie berekent de rmse door cross-validatie op basis van 6 subsets.  Deze functie kan ook gebruikt worden voor het lokaal model (ze bepaalt het verschil tussen de datasets op basis van het al dan niet aanwezig zijn van een veld DOMEIN_ID in de dataset).  Opgelet!  In tegenstelling tot de meeste functies van dit package werkt deze functie op basis van de meetgegevens van 1 model.  Zie voorbeelden voor een methode om deze functie te kunnen toepassen vertrekkend van meetgegevens (bv. Data.lokaal) of vertrekkend van een model waar de meetgegevens uit gehaald kunnen worden (bv. Basismodel).
#'
#' Deze functie berekent de rmse op basis van testgroepen en omvat de volgende deelstappen:
#'
#' - metingen opdelen in 6 testgroepen (veld testgroep)
#'
#' - modellen fitten voor testgroepen, waarbij ze de functie fit.basis 6 keer oproept
#'
#' - rmse berekenen voor domeinmodellen en Vlaams model op basis van testgroep-modellen
#'
#' @param Data Meetgegevens van één boomsoort-domein-combinatie (dataframe zoals de dataframes die in de list teruggegeven worden door de functie initiatie)
#' @param Typemodel 'Basis' of 'Lokaal'?
#'
#' @return Dataframe met rmse_domein (rmseD), rmse_Vlaams (rmseVL, niet voor lokaal model) en maxResid
#'
#' @examples
#' library(dplyr)
#' #nog datasets toevoegen om deze voorbeelden te kunnen runnen
#' \dontrun{
#' Data.lokaal %>%
#'   group_by_(
#'     ~BMS,
#'     ~DOMEIN_ID
#'   ) %>%
#'   do_(
#'     ~rmse.basis(., "Lokaal")
#'   ) %>%
#'   ungroup()
#'
#' Basismodel %>%
#'   rowwise() %>%
#'   do_(
#'     ~rmse.basis(.$Model$data, "Basis")
#'   ) %>%
#'   ungroup()
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_ ungroup transmute_ mutate_ bind_rows summarise_ arrange_ row_number
#' @importFrom nlme fixef
#' @importFrom stats predict
#' @importFrom assertthat assert_that
#'

rmse.basis <- function(Data, Typemodel){

  #controle
  assert_that(is.character(Typemodel))
  Typemodel <- tolower(Typemodel)
  assert_that(Typemodel %in% c("basis", "lokaal"))

  invoercontrole(Data, "fit")
  assert_that(length(unique(Data$BMS)) == 1,
              msg = "De dataset Data mag maar 1 boomsoort bevatten")
  if (Typemodel == "lokaal") {
    assert_that(length(unique(Data$DOMEIN_ID)) == 1,
                msg = "Voor een lokaal model mag de dataset Data maar 1
                domein bevatten")
  }

  #testgroepen aanmaken in dataset
  Soortdata <- Data %>%
    arrange_(~BMS, ~DOMEIN_ID, ~Omtrek, ~HOOGTE) %>%
    mutate_(
      Testgroep = ~ (row_number(DOMEIN_ID) - 1) %% 6 + 1
    )


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
      mutate_(
        H_Dmodel = ~predict(Model, newdata = .),
        ResidD = ~HOOGTE - H_Dmodel,
        ResidD2 = ~ResidD ^ 2,
        ResidVL2 = ~0
      )

    if (grepl(Typemodel, "basis")) {
      Data_Boomsoort <- Data_Boomsoort %>%
        mutate_(
          H_VLmodel = ~as.numeric(fixef(Model)[1]) +
            as.numeric(fixef(Model)[2]) * logOmtrek +
            as.numeric(fixef(Model)[3]) * logOmtrek2,
          ResidVL = ~HOOGTE - H_VLmodel,
          ResidVL2 = ~ResidVL ^ 2
        )
    }

    Data_result <- Data_result %>%
      bind_rows(Data_Boomsoort)
  }


  #rmse berekenen
  Rmse.soort <- Data_result[Data_result$Omtrek > 0.50, ] %>%
    group_by_(
      ~BMS,
      ~DOMEIN_ID,
      ~nBomen,
      ~nBomenInterval,
      ~nBomenOmtrek05,
      ~Q5k,
      ~Q95k
    ) %>%
    summarise_(
      sseD = ~sum(c(ResidD2)),
      sseVL = ~sum(c(ResidVL2)),
      maxResid = ~max(c(ResidD2))
    ) %>%
    ungroup() %>%
    transmute_(
      ~BMS,
      ~DOMEIN_ID,
      ~nBomen,
      ~nBomenInterval,
      ~nBomenOmtrek05,
      ~Q5k,
      ~Q95k,
      rmseD = ~sqrt(sseD / (nBomenOmtrek05 - 2)),
      rmseVL = ~sqrt(sseVL / (nBomenOmtrek05 - 2)),
      ~maxResid
    )

  #voor lokaal model het Vlaams model verwijderen (is gelijkgesteld aan 0)
  if (grepl(Typemodel, "lokaal")) {
    Rmse.soort$rmseVL <- NULL
  }

  return(Rmse.soort)
}
