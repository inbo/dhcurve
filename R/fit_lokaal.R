#' Lokaal model fitten op basis van de opgegeven dataset
#'
#' Functie die het lineair (fixed) model \eqn{Hoogte_{lokaal} \sim A_{domein} + B_{domein} \log({Omtrek}) + C_{domein} \log(Omtrek)^2}{Hoogte(lokaal) ~ A(domein) + B(domein).log(Omtrek) + C(domein).log(Omtrek)^2} fit op basis van de opgegeven dataset.
#'
#' @param Data.lokaal Dataframe met minimaal de velden BMS (boomsoort), DOMEIN_ID (identificatienummer van het domein), HOOGTE, logOmtrek en logOmtrek2.  Om alle verdere stappen van de analyse te kunnen doorlopen, wordt best de dataframe "Lokaal" gebruikt uit de list die teruggegeven wordt bij de functie initiatie.
#'
#' @return Dataframe (tibble) met de velden BMS (boomsoort), DOMEIN_ID en Model (lm-object met het gefit lineair model voor die boomsoort-domeincombinatie).
#'
#' @export
#'
#' @importFrom stats lm
#' @importFrom dplyr %>% group_by_ do_ ungroup
#'

fit.lokaal <- function(Data.lokaal) {

  invoercontrole(Data.lokaal, "fit")

  Lokaalmodel <- Data.lokaal %>%
    group_by_(~BMS, ~DOMEIN_ID) %>%
    do_(
      Model = ~ lm(
        HOOGTE ~ logOmtrek + logOmtrek2,
        data = .
      )
    ) %>%
    ungroup()

  return(Lokaalmodel)
}
