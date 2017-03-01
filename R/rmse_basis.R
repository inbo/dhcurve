#' RMSE berekenen van basismodel
#'
#' Deze functie berekent de rmse op basis van testgroepen en omvat de volgende deelstappen:
#'
#' - metingen opdelen in 6 testgroepen (veld testgroep)
#'
#' - modellen fitten voor testgroepen, waarbij ze de functie fit.basis 6 keer oproept
#'
#' - rmse berekenen voor domeinmodellen en Vlaams model op basis van testgroep-modellen
#'
#' Deze functie is geschreven voor het basismodel, maar kan door een kleine aanpassing ook gebruikt worden voor het extra model (functie bepaalt verschil op basis van het al dan niet aanwezig zijn van een veld DOMEIN_ID in de dataset)
#'
#' Vroegere param: Basismodel model per boomsoort als argument meegeven en hier de nodige gegevens uit halen  (Vermits de 2 hoofdfuncties waarin deze hulpfunctie opgeroepen wordt allebei het argument model beschikbaar hebben en de dataframe niet, lijkt het me het meest logisch om hier van het model te vertrekken, dan moet het script om de meetgegevens uit het model te halen, enkel in deze functie geschreven worden)  Een alternatief is vertrekken van het dataframe > 50 en min. 6 domeinen
#' @param Data meetgegevens (enkel nodig voor model per boomsoort-domein-combinatie)
#' @param Typemodel 'Basis' of 'Extra'?
#'
#' @return dataframe met rmse_domein en rmse_Vlaams
#'
#' @export
#'
#' @importFrom dplyr %>% group_by_ ungroup transmute_ mutate_ bind_rows summarise_ arrange_ row_number
#' @importFrom nlme fixef
#' @importFrom stats predict
#'

rmse.basis <- function(Data, Typemodel){

  #testgroepen aanmaken in dataset
  Soortdata <- Data %>%
    arrange_(~BMS, ~DOMEIN_ID, ~Omtrek, ~HOOGTE) %>%
    mutate_(
      Testgroep = ~(row_number(DOMEIN_ID) - 1) %% 6 + 1
    )


  #model fitten voor de 6 testgroepen
  Data_result <- data.frame(NULL)
  for (i in 1:6) {
    Data_test <- Soortdata[Soortdata$Testgroep == i,]
    Data_model <- Soortdata[Soortdata$Testgroep != i,]

    if (grepl(Typemodel,"Extra")) {
      Model <- fit.extra(Data_model)$Model[[1]]
    } else {
      Model <- fit.basis(Data_model)$Model[[1]]
    }

    Data_Boomsoort <- Data_test %>%
      mutate_(
        H_Dmodel = ~predict(Model, newdata = .),
        ResidD = ~HOOGTE - H_Dmodel,
        ResidD2 = ~ResidD^2,
        ResidVL2 = ~0
      )

    if (grepl(Typemodel,"Basis")) {
      Data_Boomsoort <- Data_Boomsoort %>%
        mutate_(
          H_VLmodel = ~as.numeric(fixef(Model)[1]) +
            as.numeric(fixef(Model)[2]) * logOmtrek +
            as.numeric(fixef(Model)[3]) * logOmtrek2,
          ResidVL = ~HOOGTE - H_VLmodel,
          ResidVL2 = ~ResidVL^2
        )
    }

    Data_result <- Data_result %>%
      bind_rows(Data_Boomsoort)
  }


  #rmse berekenen
  Rmse.soort <- Data_result[Data_result$Omtrek > 0.50,] %>%
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
      rmseD = ~sqrt(sseD/(nBomenOmtrek05 - 2)),
      rmseVL = ~sqrt(sseVL/(nBomenOmtrek05 - 2)),
      ~maxResid
    )

  #voor extra model het Vlaams model verwijderen (is gelijkgesteld aan 0)
  if (grepl(Typemodel,"Extra")) {
    Rmse.soort$rmseVL <- NULL
  }

  return(Rmse.soort)
}
