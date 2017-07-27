#' Fit basismodel op basis van de opgegeven dataset
#'
#' Functie die het lineair mixed model \eqn{Hoogte_{basis} \sim A_{Vlaams} + A_{domein} + (B_{Vlaams} + B_{domein}) \log({Omtrek}) + (C_{Vlaams} + C_{domein}) \log(Omtrek)^2}{Hoogte(basis) ~ A(Vlaams) + A(domein) + (B(Vlaams) + B(domein)).log(Omtrek) + (C(Vlaams) + C(domein)).log(Omtrek)^2} fit op basis van de opgegeven dataset.
#'
#' @param Data.basis Dataframe met minimaal de velden BMS (boomsoort), DOMEIN_ID (identificatienummer van het domein), HOOGTE, logOmtrek en logOmtrek2.  Om alle verdere stappen van de analyse te kunnen doorlopen, wordt best de dataframe "Basis" gebruikt uit de list die teruggegeven wordt bij de functie initiatie.
#'
#' @return Dataframe (tibble) met de velden BMS (boomsoort) en Model (lme-object met het gefit mixed model voor die boomsoort)
#'
#' @export
#'
#' @importFrom nlme lme lmeControl
#' @importFrom dplyr %>% group_by_ do_ ungroup
#'

fit.basis <- function(Data.basis) {

  invoercontrole(Data.basis, "fit")

  Basismodel <- Data.basis %>%
    group_by_(~BMS) %>%
    do_(
      Model = ~ lme(
        HOOGTE ~ logOmtrek + logOmtrek2,
        random = ~ (logOmtrek + logOmtrek2) | DOMEIN_ID,
        data = .,
        control = lmeControl(opt = "optim", singular.ok = TRUE,
                                         returnObject = TRUE)
      )
    ) %>%
    ungroup()

  return(Basismodel)
}
