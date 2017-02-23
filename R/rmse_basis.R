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
#' @param Basismodel model per boomsoort als argument meegeven en hier de nodige gegevens uit halen  (Vermits de 2 hoofdfuncties waarin deze hulpfunctie opgeroepen wordt allebei het argument model beschikbaar hebben en de dataframe niet, lijkt het me het meest logisch om hier van het model te vertrekken, dan moet het script om de meetgegevens uit het model te halen, enkel in deze functie geschreven worden)  Een alternatief is vertrekken van het dataframe > 50 en min. 6 domeinen
#' @param Data meetgegevens (enkel nodig voor model per boomsoort-domein-combinatie)
#'
#' @return dataframe met rmse_domein en rmse_Vlaams
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ group_by_ ungroup transmute_ mutate_ bind_rows summarise_ arrange_ row_number
#' @importFrom nlme fixef
#' @importFrom stats predict
#' @importFrom assertthat has_name
#'

rmse.basis <- function(Basismodel, Data = NULL){

  Rmse <- data.frame(NULL)
  for (Rijnummer in row_number(Basismodel$BMS)) {
    #dataset ophalen uit model
    if (has_name(Basismodel,"DOMEIN_ID")) {
      Boomsoort <- Basismodel[Rijnummer,]$BMS
      Domein <- Basismodel[Rijnummer,]$DOMEIN_ID
      Soortdata <- Data %>%
        filter_(~ BMS == Boomsoort &
                  DOMEIN_ID == Domein)
    } else {
      Soortdata <- (Basismodel %>% filter_(~ row_number(BMS) == Rijnummer))$Model[[1]]$data
    }

    #testgroepen aanmaken in dataset
    Soortdata <- Soortdata %>%
      arrange_(~BMS, ~DOMEIN_ID, ~Omtrek, ~HOOGTE) %>%
      mutate_(
        Testgroep = ~(row_number(DOMEIN_ID) - 1) %% 6 + 1
      )


    #model fitten voor de 6 testgroepen
    Data_result <- data.frame(NULL)
    for (i in 1:6) {
      Data_test <- Soortdata[Soortdata$Testgroep == i,]
      Data_model <- Soortdata[Soortdata$Testgroep != i,]

      if (has_name(Basismodel,"DOMEIN_ID")) {
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

      if (!has_name(Basismodel,"DOMEIN_ID")) {
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
        ~Q5,
        ~Q95
      ) %>%
      summarise_(
        sseD = ~sum(c(ResidD2)),
        sseVL = ~sum(c(ResidVL2))
      ) %>%
      ungroup() %>%
      transmute_(
        ~BMS,
        ~DOMEIN_ID,
        ~nBomen,
        ~nBomenInterval,
        ~nBomenOmtrek05,
        ~Q5,
        ~Q95,
        rmseD = ~sqrt(sseD/(nBomenOmtrek05 - 2)),
        rmseVL = ~sqrt(sseVL/(nBomenOmtrek05 - 2))
      )

    Rmse <- Rmse %>%
      bind_rows(Rmse.soort)

  }

  #voor extra model het Vlaams model verwijderen (is gelijkgesteld aan domeinmodel)
  if (has_name(Basismodel,"DOMEIN_ID")) {
    Rmse$rmseVL <- NULL
  }


  return(Rmse)
}
