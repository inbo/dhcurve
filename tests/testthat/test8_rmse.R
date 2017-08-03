context("test rmse")

test_wd <- tempdir()

setwd(test_wd)

library(dplyr)

Data <- dataAfwijkendeCurve()

Basisdata1 <- Data[["Basisdata"]]
Lokaledata <- Data[["Lokaledata"]]


test_that(
  "De rmse wordt correct berekend voor domeinen van het Basismodel", {
  expect_equal(rmse.basis(Basisdata1, "Basis") %>%
                 colnames(.),
               c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
                 "nBomenOmtrek05", "Q5k", "Q95k", "rmseD", "rmseVL",
                 "maxResid"))
  expect_equal(rmse.basis(Basisdata1, "Basis") %>%
                 filter(
                   DOMEIN_ID %in% c("HM", "LM")
                 ) %>%
                 transmute(DOMEIN_ID,
                           rmseD = as.numeric(rmseD)) %>%
                 as.data.frame(., stringsAsFactors = FALSE),
               data.frame(DOMEIN_ID = c("HM", "LM"),
                          rmseD = 3,
                          stringsAsFactors = FALSE),
               tolerance = 1)
})

test_that(
  "De rmse wordt correct berekend voor het lokaal model", {
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "HM"),
                            "Lokaal") %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
                   "nBomenOmtrek05", "Q5k", "Q95k", "rmseD", "maxResid"))
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "HM"),
                            "Lokaal") %>%
                   transmute(DOMEIN_ID,
                             rmseD = as.numeric(rmseD)) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "HM",
                            rmseD = 3,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
    expect_equal(rmse.basis(Lokaledata %>%
                              filter(DOMEIN_ID == "LM"),
                            "Lokaal") %>%
                   transmute(DOMEIN_ID,
                             rmseD = as.numeric(rmseD)) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "LM",
                            rmseD = 3,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
})


Data <- dataAfgeleidmodel()

Basisdata2 <- Data[["Basisdata"]]
Afgeleidmodel <- Data[["Afgeleidmodel"]]


# test_that(
#   "De rmse wordt correct berekend voor Vlaams model (Basismodel)", {
#     expect_equal(rmse.basis(Basisdata2, "Basis") %>%
#                    colnames(.),
#                  c("BMS", "DOMEIN_ID", "nBomen", "nBomenInterval",
#                    "nBomenOmtrek05", "Q5k", "Q95k", "rmseD", "rmseVL",
#                    "maxResid"))
#     expect_equal(rmse.basis(Basisdata2, "Basis") %>%
#                    transmute(DOMEIN_ID,
#                              rmseVL = as.numeric(rmseVL)) %>%
#                    distinct() %>%
#                    as.data.frame(., stringsAsFactors = FALSE),
#                  data.frame(DOMEIN_ID = ...,
#                             rmseD = 3,
#                             stringsAsFactors = FALSE),
#                  tolerance = 1)
# })

test_that(
  "De rsme wordt correct berekend voor de verschuiving v het Afgeleid model", {
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
                            RmseVerschuiving = 3,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
})
