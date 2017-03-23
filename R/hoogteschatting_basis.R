#' Hoogteschatting op basis van opgegeven basismodel
#'
#' Functie die de gemiddelde hoogte per omtrekklasse schat voor de domeincurves en Vlaamse curves van het opgegeven basismodel.  De teruggegeven dataframe kan gebruikt worden om grafieken te maken of afwijkende metingen te bestuderen.
#'
#' @param Soortmodel model voor boomsoort (basis) of boomsoort-domein-combinatie (lokaal)
#' @param Soortdata meetgegevens van boomsoort (basis) of domein-boomsoort-combinatie (lokaal)
#' @param Typemodel 'Basis' of 'Lokaal'?
#'
#' @return dataframe met de meetresultaten en de schattingen van de hoogtes voor het domeinmodel en de Vlaamse model
#'
#' @export
#'
#' @importFrom dplyr %>% filter_ mutate_ select_ distinct_ full_join
#' @importFrom nlme fixef
#' @importFrom stats predict
#'

hoogteschatting.basis <- function(Soortmodel, Soortdata, Typemodel) {

  AlleKlassen <- seq(15, 245, 10)

  Schatting.soort <- Soortdata %>%
    select_(~BMS, ~DOMEIN_ID, ~BOS_BHI, ~nBomenInterval, ~nBomenOmtrek05,
            ~nBomen, ~Q5k, ~Q95k) %>%
    distinct_()

  Schatting.soort <- merge(Schatting.soort, AlleKlassen) %>%
    filter_(
      ~y >= (100 * Q5k) - 1,
      ~y <= (100 * Q95k) + 1
    ) %>%
    mutate_(
      Omtrek = ~y / 100,
      logOmtrek = ~log(Omtrek),
      logOmtrek2 = ~logOmtrek ^ 2
    ) %>%
    mutate_(
      H_D_finaal = ~predict(Soortmodel, newdata = .)
    )

  if (grepl(Typemodel, "Basis")) {
    Schatting.soort <- Schatting.soort %>%
      mutate_(
        H_VL_finaal = ~as.numeric(fixef(Soortmodel)[1]) +
          as.numeric(fixef(Soortmodel)[2]) * logOmtrek +
          as.numeric(fixef(Soortmodel)[3]) * logOmtrek2
      )
  }

  Schatting.soort <- Schatting.soort %>%
    select_(~-logOmtrek, ~-logOmtrek2) %>%
    full_join(
      Soortdata %>%
        mutate_(y = ~as.integer(round(100 * Omtrek))) %>%
        select_(~-Omtrek),
      by = c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
             "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "y")
    ) %>%
    select_(~-y)

  return(Schatting.soort)
}
