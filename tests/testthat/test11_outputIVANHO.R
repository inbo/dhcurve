context("test outputIVANHO")

describe("outputIVANHO", {

  set.seed(8434556868)

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]

  it("De hoogtes worden correct berekend voor domeinen van het Basismodel", {
    expect_equal(outputIVANHO(Basismodel1) %>%
                   filter(DOMEIN_ID %in% c("HM", "LM")) %>%
                   select(-OmtrekklassetypeID, -Omtrekklasse) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = rep(c("HM", "LM"), 22),
                            BOS_BHI = rep(c("HoogMinimum", "LaagMaximum"),
                                            22),
                            Omtrek = rep(seq(0.25, 2.35, 0.1), each = 2),
                            stringsAsFactors = FALSE) %>%
                   mutate(
                     Hoogte =
                       ifelse(DOMEIN_ID == "HM",
                              20 + 7 * log(Omtrek) + 4 * log(Omtrek) ^ 2,
                              20 + 5 * log(Omtrek) - 6 * log(Omtrek) ^ 2),
                     Modeltype = "basismodel"
                   ),
                 tolerance = 1)
  })

  it("De hoogtes worden correct berekend voor het Lokaal model", {
    expect_error(outputIVANHO(Lokaalmodel))
    expect_equal(outputIVANHO(Lokaalmodel = Lokaalmodel,
                              Data.lokaal = Lokaledata) %>%
                   select(-OmtrekklassetypeID, -Omtrekklasse) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "andereboom",
                            DOMEIN_ID = rep(c("HM", "LM"), each = 22),
                            BOS_BHI = rep(c("HoogMinimum", "LaagMaximum"),
                                          each = 22),
                            Omtrek = seq(0.25, 2.35, 0.1),
                            stringsAsFactors = FALSE) %>%
                   mutate(
                     Hoogte =
                       ifelse(DOMEIN_ID == "HM",
                              20 + 7 * log(Omtrek) + 4 * log(Omtrek) ^ 2,
                              20 + 5 * log(Omtrek) - 6 * log(Omtrek) ^ 2 ),
                     Modeltype = "lokaal model"
                   ),
                 tolerance = 1)
  })

  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("De hoogtes worden correct berekend voor Afgeleid model", {
    expect_error(outputIVANHO(Afgeleidmodel))
    expect_equal(outputIVANHO(Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
                   filter(Modeltype == "afgeleid model") %>%
                   select(-OmtrekklassetypeID, -Omtrekklasse) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = "Klein",
                            BOS_BHI = "DOMEIN_Klein",
                            Omtrek = seq(0.55, 2.35, 0.1),
                            stringsAsFactors = FALSE) %>%
                   mutate(
                     Hoogte = 15 + 15 * log(Omtrek) + log(Omtrek) ^ 2,
                     Modeltype = "afgeleid model"
                   ),
                 tolerance = 1)
  })

  it("De dataset wordt correct samengesteld", {
    expect_equal(outputIVANHO(Basismodel2, Afgeleidmodel,
                              Lokaalmodel, Lokaledata) %>%
                   select(BMS, DOMEIN_ID, Modeltype) %>%
                   distinct(),
                 tibble(BMS = c(rep("testboom", 7), rep("andereboom", 2)),
                        DOMEIN_ID = c(LETTERS[1:6], "Klein", "HM", "LM"),
                        Modeltype = c(rep("basismodel", 6), "afgeleid model",
                                      rep("lokaal model", 2))))
  })

  setwd(wd)

})
