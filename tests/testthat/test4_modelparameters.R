context("test modelparameters")

describe("modelparameters", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]


  it("De parameters worden correct berekend voor domeinen van het Basismodel", {
    expect_equal(modelparameters(Basismodel1) %>%
                   filter(DOMEIN_ID %in% c("HM", "LM")) %>%
                   select(-Avl, -Bvl, -Cvl) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = c("HM", "LM"),
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = 20,
                            Bd = c(7, 5),
                            Cd = c(4, -6),
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De parameters worden correct berekend voor het Lokaal model", {
    expect_error(modelparameters(Lokaalmodel))
    expect_equal(modelparameters(Lokaalmodel, Lokaledata) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "andereboom",
                            DOMEIN_ID = c("HM", "LM"),
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = 20,
                            Bd = c(7, 5),
                            Cd = c(4, -6),
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("De parameters worden correct berekend voor Vlaams model (Basismodel)", {
    expect_equal(modelparameters(Basismodel2) %>%
                   select(-Ad, -Bd, -Cd) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = LETTERS[1:6],
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Avl = 20,
                            Bvl = 15,
                            Cvl = 1,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De parameters worden correct berekend voor Afgeleid model", {
    expect_error(modelparameters(Afgeleidmodel))
    expect_equal(modelparameters(Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
                   as.data.frame(., stringsAsFactors = FALSE),
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = "Klein",
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = -5,
                            Avl = 20,
                            Bvl = 15,
                            Cvl = 1,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })
   setwd(wd)

})
