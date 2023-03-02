#' @title Fit basismodel op basis van de opgegeven dataset
#'
#' @description
#' Functie die het lineair mixed model
#' \eqn{Hoogte_{basis} \sim A_{Vlaams} + A_{domein} + (B_{Vlaams} + B_{domein}) \log({Omtrek}) + (C_{Vlaams} + C_{domein}) \log(Omtrek)^2}{%Hoogte(basis) ~ A(Vlaams) + A(domein) + (B(Vlaams) + B(domein)).log(Omtrek) + (C(Vlaams) + C(domein)).log(Omtrek)^2}
#' fit op basis van de opgegeven dataset.
#'
#' @param Data.basis De dataframe `Basis` uit de list die teruggegeven wordt
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
#' @return Dataframe (tibble) met de velden `BMS` (boomsoort) en
#' `Model` (`lme`-object met het gefit mixed model voor die boomsoort)
#'
#' @export
#'
#' @importFrom nlme lme lmeControl
#' @importFrom dplyr %>% group_by select ungroup filter
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom assertthat has_name
#'

fit.basis <- function(Data.basis) {

  if (has_name(Data.basis, "VoorModelFit")) {
    Data.basis <- Data.basis %>%
      filter(.data$VoorModelFit) %>%
      select(-"VoorModelFit")
  }
  invoercontrole(Data.basis, "fit")

  mod_fun <- function(df) {
    lme(
      HOOGTE ~ logOmtrek + logOmtrek2,
      random = ~ (logOmtrek + logOmtrek2) | DOMEIN_ID,
      data = df,
      control = lmeControl(opt = "optim", singular.ok = TRUE,
                           returnObject = TRUE)
    )
  }

  Basismodel <- Data.basis %>%
    group_by(.data$BMS) %>%
    nest() %>%
    mutate(
      Model = map(.data$data, mod_fun)
    ) %>%
    ungroup() %>%
    select(-"data")

  return(Basismodel)
}
