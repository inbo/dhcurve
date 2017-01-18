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
#' @param Basismodel model per boomsoort als argument meegeven en hier de nodige gegevens uit halen  (Vermits de 2 hoofdfuncties waarin deze hulpfunctie opgeroepen wordt allebei het argument model beschikbaar hebben en de dataframe niet, lijkt het me het meest logisch om hier van het model te vertrekken, dan moet het script om de meetgegevens uit het model te halen, enkel in deze functie geschreven worden)  Een alternatief is vertrekken van het dataframe > 50 en min. 6 domeinen
#'
#' @return dataframe met rmse_domein en rmse_Vlaams
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ group_by_ sample_frac ungroup transmute_ mutate_ bind_rows summarise_
#'

rmse.basis <- function(Basismodel){

  Rmse <- data.frame(NULL)
  for (Boomsoort in Basismodel$BMS) {
    #dataset ophalen uit model
    Soortdata <- (Basismodel %>% filter_(~ BMS == Boomsoort))$Model[[1]]$data

    #testgroepen aanmaken in dataset
    Soortdata$Testgroep <- NA
    for (i in 1:5) {
      Selectie <- Soortdata %>%
        filter_(
          ~is.na(Testgroep)
        ) %>%
        group_by_(
          ~DOMEIN_ID,
          ~TYPE_METING,
          ~JAAR,
          ~Omtrek
        ) %>%
        sample_frac(1/(7 - i)) %>%
        ungroup() %>%
        transmute_(
          ~Rijnr
        )
      Soortdata$Testgroep <-
        ifelse(Soortdata$Rijnr %in% Selectie$Rijnr,i,Soortdata$Testgroep)
    }
    Soortdata$Testgroep <-
      ifelse(is.na(Soortdata$Testgroep),6,Soortdata$Testgroep)


    #model fitten voor de 6 testgroepen
    Data_result <- data.frame(NULL)
    for (i in 1:6) {
      Data_test <- Soortdata[Soortdata$Testgroep == i,]
      Data_model <- Soortdata[Soortdata$Testgroep != i,]

      Model <- fit.basis(Data_model)$Model[[1]]

      Data_Boomsoort <- Data_test %>%
        mutate_(
          H_Dmodel = ~predict(Model, newdata = .),
          ResidD = ~HOOGTE - H_Dmodel,
          ResidD2 = ~ResidD^2,
          H_VLmodel = ~as.numeric(fixef(Model)[1]) +
            as.numeric(fixef(Model)[2]) * logOmtrek +
            as.numeric(fixef(Model)[3]) * logOmtrek2,
          ResidVL = ~HOOGTE - H_VLmodel,
          ResidVL2 = ~ResidVL^2
        )
      Data_result <- Data_result %>%
        bind_rows(Data_Boomsoort)
    }


    #rmse berekenen
    Rmse.soort <- Data_result[Data_result$Omtrek > 0.50,] %>%
      group_by_(~DOMEIN_ID, ~nBomen, ~Q5, ~Q95) %>%
      summarise_(
        nBomenInterval = ~n(),
        sseD = ~sum(c(ResidD2)),
        sseVL = ~sum(c(ResidVL2))
      ) %>%
      ungroup() %>%
      transmute_(
        ~DOMEIN_ID,
        ~nBomen,
        ~nBomenInterval,
        ~Q5,
        ~Q95,
        rmseD = ~sqrt(sseD/(nBomenInterval - 2)),
        rmseVL = ~sqrt(sseVL/(nBomenInterval - 2))
      )

    Rmse <- Rmse %>%
      bind_rows(data.frame(BMS = Boomsoort, Rmse.soort,
                           stringsAsFactors = FALSE))

  }


  return(Rmse)
}
