context("test rmse")

wd <- getwd()

test_wd <- tempdir()

setwd(test_wd)

library(dplyr)

for (rmse in c(1, 3, 10)) {

describe(sprintf("rmse", rmse), {

  Data <- dataAfwijkendeCurve(nBomen = 10000, sd = rmse)

  Basisdata1 <- Data[["Basisdata"]]
  Lokaledata <- Data[["Lokaledata"]]


  it("De rmse wordt correct berekend voor domeinen van het Basismodel", {
    expect_equal(rmse.basis(Basisdata1, "Basis", unique(Basisdata1$BMS)) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
                   "nBomenIntervalOmtrek05", "Q5k", "Q95k", "rmseD", "rmseVL",
                   "maxResid"))
    expect_equal(rmse.basis(Basisdata1, "Basis", unique(Basisdata1$BMS)) %>%
                   filter(
                     DOMEIN_ID %in% c("HM", "LM")
                   ) %>%
                   transmute(DOMEIN_ID,
                             rmseD = as.numeric(rmseD)) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = c("HM", "LM"),
                            rmseD = rmse,
                            stringsAsFactors = FALSE),
                 tolerance = 0.1)
  })

  it("De rmse wordt correct berekend voor het lokaal model", {
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "HM"),
                            "Lokaal", unique(Lokaledata$BMS)) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
                   "nBomenIntervalOmtrek05", "Q5k", "Q95k", "rmseD", "maxResid")
                 )
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "HM"),
                            "Lokaal", unique(Lokaledata$BMS)) %>%
                   transmute(DOMEIN_ID,
                             rmseD = as.numeric(rmseD)) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "HM",
                            rmseD = rmse,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "LM"),
                            "Lokaal", unique(Lokaledata$BMS)) %>%
                   transmute(DOMEIN_ID,
                             rmseD = as.numeric(rmseD)) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "LM",
                            rmseD = rmse,
                            stringsAsFactors = FALSE),
                 tolerance = 0.1)
  })


  Data <-
    dataAfgeleidmodel(nBomenBasis = 10000, nBomenAfgeleid = 10000, sd = rmse,
                      Uitzonderingen =
                        data.frame(DOMEIN_ID = "Klein", BMS = "testboom",
                                   min_basis = 10001, min_afgeleid = NA,
                                   stringsAsFactors = FALSE))

  Basisdata2 <- Data[["Basisdata"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("De output van de functie is correct voor Vlaams model (Basismodel)", {
    expect_equal(rmse.basis(Basisdata2, "Basis", unique(Basisdata2$BMS)) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
                   "nBomenIntervalOmtrek05", "Q5k", "Q95k", "rmseD", "rmseVL",
                   "maxResid"))
    expect_is(
      rmse.basis(Basisdata2, "Basis", unique(Basisdata2$BMS))$rmseVL, "numeric"
    )
  })

  it(
    "De rsme wordt correct berekend voor de verschuiving v h Afgeleid model", {
      expect_equal(rmse.verschuiving(Afgeleidmodel[[1]]$Model[[1]],
                                     "testboom",
                                     "Klein") %>%
                     colnames(.),
                   c("BMS", "DOMEIN_ID", "nBomenModel", "RmseVerschuiving"))
      expect_equal(rmse.verschuiving(Afgeleidmodel[[1]]$Model[[1]],
                                     "testboom",
                                     "Klein") %>%
                     transmute(DOMEIN_ID,
                               RmseVerschuiving =
                                 as.numeric(RmseVerschuiving)) %>%
                     as.data.frame(., stringsAsFactors = FALSE),
                   data.frame(DOMEIN_ID = "Klein",
                              RmseVerschuiving = rmse,
                              stringsAsFactors = FALSE),
                   tolerance = 0.1)
  })

})

}

setwd(wd)
