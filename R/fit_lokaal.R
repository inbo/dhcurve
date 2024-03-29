#' @title Lokaal model fitten op basis van de opgegeven dataset
#'
#' @description
#' Functie die het lineair (fixed) model
#' \eqn{Hoogte_{lokaal} \sim A_{domein} + B_{domein} \log({Omtrek}) + C_{domein} \log(Omtrek)^2}{Hoogte(lokaal) ~ A(domein) + B(domein).log(Omtrek) + C(domein).log(Omtrek)^2}
#' fit op basis van de opgegeven dataset.
#'
#' @param Data.lokaal De dataframe `Lokaal` uit de list die teruggegeven wordt
#' bij de functie `initiatie()`.  (Een dataframe met minimaal de velden `BMS`
#' (boomsoort), `IDbms` (identificatienummer van de boomsoort), `DOMEIN_ID`
#' (identificatienummer van het domein), `BOS_BHI` (domeinnaam), `HOOGTE`,
#' `Omtrek`, `logOmtrek`, `logOmtrek2`,
#' `Status` (van data: "niet gecontroleerd", "gecontroleerd",...),
#' `nBomen` (aantal bomen per boomsoort-domeincombinatie),
#' `nBomenOmtrek05` (aantal bomen per boomsoort-domeincombinatie met
#' omtrek > 0.5 m),
#' `nBomenInterval` (aantal bomen per boomsoort-domeincombinatie binnen
#' bruikbaar interval), `nBomenIntervalOmtrek05` (aantal bomen per
#' boomsoort-domeincombinatie binnen bruikbaar interval met omtrek > 0.5 m),
#' `Q5k` en `Q95k` ("bruikbaar interval").)
#'
#' @return Dataframe (tibble) met de velden `BMS` (boomsoort), `DOMEIN_ID` en
#' `Model` (`lm`-object met het gefit lineair model voor die
#' boomsoort-domeincombinatie).
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom dplyr %>% group_by do ungroup filter select
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom assertthat has_name
#'

fit.lokaal <- function(Data.lokaal) {

  if (has_name(Data.lokaal, "VoorModelFit")) {
    Data.lokaal <- Data.lokaal %>%
      filter(.data$VoorModelFit) %>%
      select(-"VoorModelFit")
  }
  invoercontrole(Data.lokaal, "fit")

  mod_fun <- function(df) {
    lm(
      HOOGTE ~ logOmtrek + logOmtrek2,
      data = df
    )
  }

  Lokaalmodel <- Data.lokaal %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    nest() %>%
    mutate(
      Model = map(.data$data, mod_fun)
    ) %>%
    ungroup() %>%
    select(-"data")

  return(Lokaalmodel)
}
