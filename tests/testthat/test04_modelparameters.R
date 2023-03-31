context("test modelparameters")

describe("modelparameters", {

  set.seed(1635434355)

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)

  Data <- dataAfwijkendeCurve()

  Basismodel1 <- Data[["Basismodel"]]
  Lokaledata <- Data[["Lokaledata"]]
  Lokaalmodel <- Data[["Lokaalmodel"]]


  it("De parameters worden correct berekend voor domeinen van het Basismodel", {
    resultaat <-
      modelparameters(Basismodel1) %>%
                   filter(DOMEIN_ID %in% c("HM", "LM")) %>%
                   select(-Avl, -Bvl, -Cvl) %>%
                   as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    attr(resultaat, "groups") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = c("HM", "LM"),
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = 30,
                            Bd = c(7, 5),
                            Cd = c(4, -6),
                            stringsAsFactors = FALSE),
                 tolerance = 0.5)
  })

  it("De parameters worden correct berekend voor het Lokaal model", {
    expect_error(modelparameters(Lokaalmodel))
    resultaat <-
      modelparameters(Lokaalmodel, Lokaledata) %>%
                   as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "andereboom",
                            DOMEIN_ID = c("HM", "LM"),
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = 30,
                            Bd = c(7, 5),
                            Cd = c(4, -6),
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  Data <- dataAfgeleidmodel()

  Basismodel2 <- Data[["Basismodel"]]
  Afgeleidmodel <- Data[["Afgeleidmodel"]]


  it("functie modelparameters() geeft geen warnings", {
    expect_no_warning(modelparameters(Basismodel1))
    expect_no_warning(
      modelparameters(Basismodel2, Afgeleidmodel = Afgeleidmodel))
    expect_no_warning(modelparameters(Lokaalmodel, Lokaledata))
  })

  it("De parameters worden correct berekend voor Vlaams model (Basismodel)", {
    resultaat <-
      modelparameters(Basismodel2) %>%
                   select(-Ad, -Bd, -Cd) %>%
                   as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    attr(resultaat, "groups") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = LETTERS[1:6],
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Avl = 30,
                            Bvl = 15,
                            Cvl = 1,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })

  it("De parameters worden correct berekend voor Afgeleid model", {
    expect_error(modelparameters(Afgeleidmodel))
    resultaat <-
      modelparameters(Basismodel2, Afgeleidmodel = Afgeleidmodel) %>%
                   as.data.frame(., stringsAsFactors = FALSE)
    attr(resultaat$Q5k, "names") <- NULL
    attr(resultaat$Q95k, "names") <- NULL
    expect_equal(resultaat,
                 data.frame(BMS = "testboom",
                            DOMEIN_ID = "Klein",
                            Q5k = 0.25,
                            Q95k = 2.35,
                            Ad = -5,
                            Avl = 30,
                            Bvl = 15,
                            Cvl = 1,
                            stringsAsFactors = FALSE),
                 tolerance = 1)
  })
   setwd(wd)

})
