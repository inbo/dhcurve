#' @title Fit afgeleid model op basis van het basismodel en de opgegeven dataset
#'
#' @description
#' Functie die op basis van de opgegeven dataset een model opstelt per
#' boomsoort-domeincombinatie door verschuiving van het Vlaams model voor die
#' boomsoort (afgeleid uit het basismodel). Dit komt overeen met het fitten van
#' het lineair model \eqn{Hoogte_{afgeleid} \sim \delta A + Hoogte_{Vlaams}}{%
#' Hoogte(afgeleid) ~ \delta A + Hoogte(Vlaams)} op basis van de opgegeven
#' dataset, waarbij \eqn{Hoogte_{Vlaams}}{Hoogte(Vlaams)} de hoogteschatting
#' is volgens het
#' Vlaams model.
#'
#' @param Data.afgeleid De dataframe "Afgeleid" uit de list die teruggegeven
#' wordt bij de functie `initiatie()`.  (Een dataframe met minimaal de velden
#' `BMS` (boomsoort), `IDbms` (identificatienummer van de boomsoort),
#' `DOMEIN_ID` (identificatienummer van het domein), `BOS_BHI` (domeinnaam),
#' `HOOGTE`, `Omtrek`,
#' `logOmtrek`, `logOmtrek2`, `Status` (van data: "niet gecontroleerd",
#' "gecontroleerd",...), `nBomen` (aantal bomen per boomsoort-domeincombinatie),
#' `nBomenOmtrek05` (aantal bomen per boomsoort-domeincombinatie met
#' omtrek > 0.5 m),
#' `nBomenInterval` (aantal bomen per boomsoort-domeincombinatie binnen
#' bruikbaar interval), `nBomenIntervalOmtrek05` (aantal bomen per
#' boomsoort-domeincombinatie binnen bruikbaar interval met omtrek > 0.5 m),
#' `Q5k` en `Q95k` ("bruikbaar interval").)
#' @param Basismodel Model per boomsoort zoals teruggegeven door de functie
#' `fit.basis()`: tibble met de velden `BMS` (boomsoort) en `Model`
#' (`lme`-object met het gefit mixed model voor die boomsoort)
#'
#' @return List met 2 tibbles:
#' \itemize{
#'    \item{tibble met velden `BMS` (boomsoort), `DOMEIN_ID` en `Model`
#'      (`lm`-object van model per boomsoort-domeincombinatie)}
#'    \item{tibble met de ingevoerde dataset, waaraan het veld `H_VL_finaal` (de
#'      hoogteschatting volgens het Vlaams model) toegevoegd is}
#'  }
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom dplyr %>% inner_join group_by do ungroup select filter
#' @importFrom plyr .
#' @importFrom rlang .data
#' @importFrom tidyr nest
#' @importFrom purrr map
#'

fit.afgeleid <- function(Data.afgeleid, Basismodel) {

  invoercontrole(Data.afgeleid, "fit")
  invoercontrole(Basismodel, "basismodel")
  Omtrekgrenzen <- Data.afgeleid %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    summarise(
      OmtrekMin = min(.data$Omtrek),
      OmtrekMax = max(.data$Omtrek)
    ) %>%
    ungroup()

  #eerst doen we een hoogteschatting op basis van het Vlaams model voor alle
  #omtrekklassen binnen de ranges van de boomsoort-domein-combinaties waarvoor
  #we het afgeleid model maken.
  Hoogteschatting <- Basismodel %>%
    inner_join(
      Data.afgeleid,
      by = c("BMS")
    ) %>%
    group_by(
      .data$BMS,
      .data$DOMEIN_ID
    ) %>%
    do(
      hoogteschatting.basis(.$Model[[1]],
                             select(., -"Model"),
                             "Basis", unique(.data$BMS))
    ) %>%
    ungroup() %>%
    select(-"H_D_finaal") %>%
    inner_join(
      Omtrekgrenzen,
      by = c("BMS", "DOMEIN_ID")
    ) %>%
    filter(
      .data$Omtrek > .data$OmtrekMin - 0.35,
      .data$Omtrek < .data$OmtrekMax + 0.25
    ) %>%
    select(-OmtrekMin, -OmtrekMax)

  mod_fun <- function(df) {
    lm(
      HOOGTE ~ 1 + offset(H_VL_finaal),
      data = df
    )
  }

  Afgeleidmodel <- Hoogteschatting %>%
    filter(!is.na(.data$HOOGTE)) %>%
    group_by(.data$BMS, .data$DOMEIN_ID) %>%
    nest() %>%
    mutate(
      Model = map(.data$data, mod_fun)
    ) %>%
    ungroup() %>%
    select(-"data")

  return(list(Afgeleidmodel, Hoogteschatting))
}
