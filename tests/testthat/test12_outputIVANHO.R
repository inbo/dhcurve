context("test outputIVANHO")

describe("outputIVANHO", {

  set.seed(84345568)

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]

  it("De uitgevoerde dataset heeft de juiste kolommen", {
    expect_equal(
      outputIVANHO(Basismodel = Basismodel1, Data.lokaal = Lokaledata,
                   Lokaalmodel = Lokaalmodel) %>%
        colnames(.),
      c("BMS", "IDbms", "DOMEIN_ID", "BOS_BHI", "Omtrek", "OmtrekklassetypeID",
        "Omtrekklasse", "Hoogte", "RMSE", "Modeltype")
    )
  })

  it("De hoogtes worden correct berekend voor domeinen van het Basismodel", {
    expect_equal(outputIVANHO(Basismodel1) %>%
                   filter(DOMEIN_ID %in% c("HM", "LM")) %>%
                   select(
                     -OmtrekklassetypeID, -Omtrekklasse, -RMSE
                   ) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            IDbms = 1,
                            DOMEIN_ID = c("HM", rep(c("HM", "LM"), 24)),
                            BOS_BHI =
                              c("HoogMinimum",
                                rep(c("HoogMinimum", "LaagMaximum"), 24)),
                            Omtrek =
                              c(0.15, rep(seq(0.25, 2.55, 0.1), each = 2)),
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
    expect_error(outputIVANHO(Lokaalmodel = Lokaalmodel))
    expect_equal(outputIVANHO(Lokaalmodel = Lokaalmodel,
                              Data.lokaal = Lokaledata) %>%
                   select(
                     -OmtrekklassetypeID, -Omtrekklasse, -RMSE
                   ) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(
                   BMS = "andereboom",
                   IDbms = 2,
                   DOMEIN_ID = c("HM", rep(c("HM", "LM"), each = 24)),
                   BOS_BHI =
                     c("HoogMinimum",
                       rep(c("HoogMinimum", "LaagMaximum"), each = 24)),
                   Omtrek = c(0.15, rep(seq(0.25, 2.55, 0.1), 2)),
                   stringsAsFactors = FALSE
                 ) %>%
                   mutate(
                     Hoogte =
                       ifelse(DOMEIN_ID == "HM",
                              20 + 7 * log(Omtrek) + 4 * log(Omtrek) ^ 2,
                              20 + 5 * log(Omtrek) - 6 * log(Omtrek) ^ 2),
                     Modeltype = "lokaal model"
                   ),
                 tolerance = 1)
  })

  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]

  it("functie outputIVANHO() geeft geen warnings", {
    expect_no_warning(
      outputIVANHO(Basismodel = Basismodel1, Data.lokaal = Lokaledata,
                   Lokaalmodel = Lokaalmodel))
    expect_no_warning(
      outputIVANHO(Basismodel = Basismodel2, Afgeleidmodel = Afgeleidmodel))
  })

  it("De uitgevoerde dataset heeft de juiste kolommen voor afgeleid model", {
    expect_equal(
      outputIVANHO(Basismodel = Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
        colnames(.),
      c("BMS", "IDbms", "DOMEIN_ID", "BOS_BHI", "Omtrek", "OmtrekklassetypeID",
        "Omtrekklasse", "Hoogte", "RMSE", "Modeltype")
    )
  })

  it("De hoogtes worden correct berekend voor Afgeleid model", {
    expect_error(outputIVANHO(Afgeleidmodel))
    expect_error(outputIVANHO(Afgeleidmodel = Afgeleidmodel))
    expect_equal(outputIVANHO(Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
                   filter(Modeltype == "afgeleid model") %>%
                   select(
                     -OmtrekklassetypeID, -Omtrekklasse, -RMSE
                   ) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            IDbms = 1,
                            DOMEIN_ID = "Klein",
                            BOS_BHI = "DOMEIN_Klein",
                            Omtrek = seq(0.45, 2.55, 0.1),
                            stringsAsFactors = FALSE) %>%
                   mutate(
                     Hoogte = 15 + 15 * log(Omtrek) + log(Omtrek) ^ 2,
                     Modeltype = "afgeleid model"
                   ),
                 tolerance = 1)
  })

  it("De dataset wordt correct samengesteld", {
    expect_message(outputIVANHO())
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
