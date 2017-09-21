context("test hoogteschatting")

describe("hoogteschatting", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]


  it("De hoogtes worden correct berekend voor domeinen van het Basismodel", {
    expect_equal(hoogteschatting.basis(Basismodel1$Model[[1]],
                                       Basismodel1$Model[[1]]$data,
                                       "Basis") %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "H_VL_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95"))
    expect_equal(hoogteschatting.basis(Basismodel1$Model[[1]],
                                       Basismodel1$Model[[1]]$data,
                                       "Basis") %>%
                   filter(
                     DOMEIN_ID %in% c("HM", "LM"),
                     Omtrek == 0.55
                   ) %>%
                   transmute(DOMEIN_ID, Omtrek,
                             H_D_finaal = as.numeric(H_D_finaal)) %>%
                   distinct() %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = c("HM", "LM"),
                            Omtrek = 0.55,
                            H_D_finaal =
                              c(20 + 7 * log(0.55) + 4 * log(0.55) ^ 2,
                                20 + 5 * log(0.55) - 4 * log(0.55) ^ 2),
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De hoogtes worden correct berekend voor het lokaal model", {
    expect_equal(hoogteschatting.basis(Lokaalmodel$Model[[1]],
                                       Lokaledata %>%
                                         filter(DOMEIN_ID == "HM"),
                                       "Lokaal") %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95"))
    expect_equal(hoogteschatting.basis(Lokaalmodel$Model[[1]],
                                       Lokaledata %>%
                                         filter(DOMEIN_ID == "HM"),
                                       "Lokaal") %>%
                   filter(
                     Omtrek == 0.55
                   ) %>%
                   transmute(DOMEIN_ID, Omtrek,
                             H_D_finaal = as.numeric(H_D_finaal)) %>%
                   distinct() %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "HM",
                            Omtrek = 0.55,
                            H_D_finaal = 20 + 7 * log(0.55) + 4 * log(0.55) ^ 2,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
    expect_equal(hoogteschatting.basis(Lokaalmodel$Model[[2]],
                                       Lokaledata %>%
                                         filter(DOMEIN_ID == "LM"),
                                       "Lokaal") %>%
                   filter(
                     Omtrek == 0.55
                   ) %>%
                   transmute(DOMEIN_ID, Omtrek,
                             H_D_finaal = as.numeric(H_D_finaal)) %>%
                   distinct() %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(DOMEIN_ID = "LM",
                            Omtrek = 0.55,
                            H_D_finaal = 20 + 5 * log(0.55) - 4 * log(0.55) ^ 2,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })


  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("De hoogtes worden correct berekend voor Vlaams model (Basismodel)", {
    expect_equal(hoogteschatting.basis(Basismodel2$Model[[1]],
                                       Basismodel2$Model[[1]]$data,
                                       "Basis") %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_D_finaal", "H_VL_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95"))
    expect_equal(hoogteschatting.basis(Basismodel2$Model[[1]],
                                       Basismodel2$Model[[1]]$data,
                                       "Basis") %>%
                   filter(
                     Omtrek == 0.55
                   ) %>%
                   transmute(BMS, Omtrek,
                             H_VL_finaal = as.numeric(H_VL_finaal)) %>%
                   distinct() %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            Omtrek = 0.55,
                            H_VL_finaal = 20 + 15 * log(0.55) + log(0.55) ^ 2,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De hoogtes worden correct berekend voor Afgeleid model", {
    expect_equal(hoogteschatting.afgeleid(Afgeleidmodel[[1]]$Model[[1]],
                                       Afgeleidmodel[[2]]) %>%
                   colnames(.),
                 c("BMS", "DOMEIN_ID", "BOS_BHI", "nBomenInterval",
                   "nBomenOmtrek05", "nBomen", "Q5k", "Q95k", "Omtrek",
                   "H_VL_finaal", "IDbms", "C13", "HOOGTE",
                   "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "Q5",
                   "Q95", "H_D_finaal"))
    expect_equal(hoogteschatting.afgeleid(Afgeleidmodel[[1]]$Model[[1]],
                                          Afgeleidmodel[[2]]) %>%
                   filter(
                     Omtrek == 1.05
                   ) %>%
                   transmute(BMS, Omtrek,
                             H_VL_finaal = as.numeric(H_VL_finaal),
                             H_D_finaal = as.numeric(H_D_finaal)) %>%
                   distinct() %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            Omtrek = 0.55,
                            H_VL_finaal = 20 + 15 * log(1.05) + log(1.05) ^ 2,
                            H_D_finaal = 15 + 15 * log(1.05) + log(1.05) ^ 2,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  setwd(wd)

})
