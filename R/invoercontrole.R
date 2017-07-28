#' Hulpfunctie die ingevoerd object controleert
#'
#' Omdat de controle op de invoer voor meerdere functies gelijkaardig is, is deze interne functie ontwikkeld die voor de vaak voorkomende parameters van de functies uit dhcurve controleert of de invoer voldoet aan de vereisten.
#'
#' @param Data Het object dat moet gecontroleerd worden (dataframe of list)
#'
#' @param Type Het type controle dat moet gebeuren, verwijzend naar de functie(s) waarvoor het gebruikt wordt.  Mogelijke waarden zijn: 'initiatie', 'fit', 'basismodel', 'lokaalmodel', 'afgeleidmodel' en 'afgeleidedata'
#'
#' @return Een foutmelding of de geteste dataframe.
#'
#' @importFrom assertthat assert_that has_name
#'

invoercontrole <- function(Data, Type) {

  #controle 'Type'
  assert_that(is.character(Type))
  Type <- tolower(Type)
  assert_that(Type %in% c("initiatie", "fit", "afgeleidedata",
                          "basismodel", "lokaalmodel", "afgeleidmodel"))

  #controle 'afgeleidmodel' (= list van model en data)
  if (Type == "afgeleidmodel") {

    assert_that(inherits(Data, "list"))
    invoercontrole(Data[[1]], "lokaalmodel")
    invoercontrole(Data[[2]], "afgeleidedata")

  } else {

  #controle voor alle andere invoertypes
    assert_that(inherits(Data, "data.frame"))
    assert_that(nrow(Data) > 0, msg = "De opgegeven dataframe is leeg")
    assert_that(has_name(Data, "BMS"),
                msg = "De opgegeven dataframe heeft geen veld met naam BMS")

    if (Type != "basismodel") {
      assert_that(
        has_name(Data, "DOMEIN_ID"),
        msg = "De opgegeven dataframe heeft geen veld met naam DOMEIN_ID"
      )
    }

  # controle specifiek voor de 'echte' dataframes voor 'initiatie', 'fit' en
  # 'afgeleidedata'
    if (Type %in% c("initiatie", "fit", "afgeleidedata")) {
      assert_that(
        has_name(Data, "BOS_BHI"),
        msg = "De opgegeven dataframe heeft geen veld met naam BOS_BHI"
      )
      assert_that(has_name(Data, "IDbms"),
                  msg = "De opgegeven dataframe heeft geen veld met naam IDbms")

      assert_that(has_name(Data, "C13"),
                  msg = "De opgegeven dataframe heeft geen veld met naam C13")
      assert_that(inherits(Data$C13, "numeric"))
      assert_that(
        has_name(Data, "HOOGTE"),
        msg = "De opgegeven dataframe heeft geen veld met naam HOOGTE"
      )
      assert_that(inherits(Data$HOOGTE, "numeric"))

      assert_that(
        has_name(Data, "Status"),
        msg = "De opgegeven dataframe heeft geen veld met naam Status"
      )
      assert_that(inherits(Data$Status, "character"))
      if (Type != "afgeleidedata" &
          !all(Data$Status %in%
               c("Niet gecontroleerd", "Te controleren", "Goedgekeurd"))) {
        stop("De kolom Status in de dataframe heeft niet voor alle records een
             geldige waarde.  Zorg dat enkel de waarden 'Niet gecontroleerd',
             'Te controleren' en 'Goedgekeurd' voorkomen.")
      }
      if (Type == "afgeleidedata" &
          !all(Data$Status %in%
               c("Niet gecontroleerd", "Te controleren", "Goedgekeurd", NA))) {
        stop(
          "De kolom Status in de dataframe heeft niet voor alle records een
          geldige waarde.  Zorg dat enkel de waarden 'Niet gecontroleerd',
          'Te controleren' en 'Goedgekeurd' voorkomen, NA is ook toegelaten."
        )
      }
    }

  #controle van specifieke velden die enkel bij 'fit' en 'afgeleidedata'
  #voorkomen
    if (Type %in% c("fit", "afgeleidedata")) {
      assert_that(
        has_name(Data, "nBomen"),
        msg = "De opgegeven dataframe heeft geen veld met naam nBomen"
      )
      if (!all.equal(Data$nBomen, as.integer(Data$nBomen),
                     check.attributes = FALSE)) {
        stop("De waarden in de kolom nBomen moeten gehele getallen zijn")
      }
      if (!all(Data$nBomen >= 0)) {
        stop("De waarden in de kolom nBomen mogen niet negatief zijn")
      }

      assert_that(has_name(Data, "nBomenInterval"),
                  msg = "De opgegeven dataframe heeft geen veld met naam
                  nBomenInterval")
      if (!all.equal(Data$nBomenInterval, as.integer(Data$nBomenInterval),
                     check.attributes = FALSE)) {
        stop(
          "De waarden in de kolom nBomenInterval moeten gehele getallen zijn"
        )
      }
      if (!all(Data$nBomenInterval >= 0)) {
        stop("De waarden in de kolom nBomenInterval mogen niet negatief zijn")
      }

      assert_that(has_name(Data, "nBomenOmtrek05"),
                  msg = "De opgegeven dataframe heeft geen veld met
                  naam nBomenOmtrek05")
      if (!all.equal(Data$nBomenOmtrek05, as.integer(Data$nBomenOmtrek05),
                     check.attributes = FALSE)) {
        stop(
          "De waarden in de kolom nBomenOmtrek05 moeten gehele getallen zijn"
        )
      }
      if (!all(Data$nBomenOmtrek05 >= 0)) {
        stop("De waarden in de kolom nBomenOmtrek05 mogen niet negatief zijn")
      }

      if (!all(Data$nBomen >= Data$nBomenInterval)) {
        stop("nBomen moet groter zijn dan nBomenInterval")
      }

      if (!all(Data$nBomenInterval >= Data$nBomenOmtrek05)) {
        stop("nBomenInterval moet groter zijn dan nBomenOmtrek05")
      }

      assert_that(
        has_name(Data, "Omtrek"),
        msg = "De opgegeven dataframe heeft geen veld met naam Omtrek"
      )
      assert_that(inherits(Data$Omtrek, "numeric"))
      if (!all(round(Data$Omtrek * 100) %in% seq(25, 235, 10))) {
        stop("Omtrek bevat waarden die geen geldige omtrekklassen zijn
             (geldige omtrekklassen zijn 0.25, 0.35, 0.45,... t.e.m. 2.35)")
      }

      if (Type == "fit") {
        assert_that(has_name(Data, "logOmtrek"),
                    msg = "De opgegeven dataframe heeft geen veld met naam
                    logOmtrek")
        assert_that(inherits(Data$logOmtrek, "numeric"))
        if (!all.equal(Data$logOmtrek, log(Data$Omtrek),
                       check.attributes = FALSE)) {
          stop("logOmtrek is niet overal correct berekend")
        }

        assert_that(has_name(Data, "logOmtrek2"),
                    msg = "De opgegeven dataframe heeft geen veld met naam
                    logOmtrek2")
        assert_that(inherits(Data$logOmtrek2, "numeric"))
        if (!all.equal(Data$logOmtrek2, Data$logOmtrek ^ 2,
                       check.attributes = FALSE)) {
          stop("logOmtrek2 is niet overal correct berekend")
        }
      }

      assert_that(has_name(Data, "Q5k"),
                  msg = "De opgegeven dataframe heeft geen veld met naam Q5k")
      assert_that(inherits(Data$Q5k, "numeric"))
      assert_that(has_name(Data, "Q95k"),
                  msg = "De opgegeven dataframe heeft geen veld met naam Q95k")
      assert_that(inherits(Data$Q95k, "numeric"))
    }

  #controle van veld model in 'basismodel' en 'lokaalmodel'
    if (grepl("model", Type)) {
      assert_that(has_name(Data, "Model"))
      assert_that(inherits(Data$Model, "list"))
    }

  }


  return(Data)

}
