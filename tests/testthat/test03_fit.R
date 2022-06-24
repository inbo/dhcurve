context("test fit")

describe("fit", {

  wd <- getwd()

  test_wd <- tempdir()

  setwd(test_wd)

  library(dplyr)
  library(tibble)

  Data <- testdataset() %>%
    bind_rows(testdataset(c(100, 100), BMS = "andereboom", IDbms = 2))

  Datalijst <- initiatie(Data)

  Data.basis <- Datalijst[["Basis"]]
  Data.afgeleid <- Datalijst[["Afgeleid"]]
  Data.lokaal <- Datalijst[["Lokaal"]]


  it("Output van fit.basis is correct (tibble met velden BMS en Model)", {
      expect_is(fit.basis(Data.basis), "data.frame")
      expect_equal(colnames(fit.basis(Data.basis)),
                   c("BMS", "Model"))
      expect_equal(fit.basis(Data.basis) %>%
                     select(BMS),
                   tibble(BMS = "testboom"))
      expect_is(fit.basis(Data.basis)$Model, "list")
      expect_s3_class(fit.basis(Data.basis)$Model[[1]], "lme")
  })

  Data_basis_fout <- Data.basis
  Data_basis_fout$HOOGTE[1] <- -1

  it("Foutcontrole in fit.basis gebeurt correct", {
    expect_error(fit.basis(Data.basis %>% select(-BMS)))
    expect_error(fit.basis(Data.basis %>% select(-DOMEIN_ID)))
    expect_error(fit.basis(Data.basis %>% select(-HOOGTE)))
    expect_error(fit.basis(Data.basis %>% mutate(HOOGTE = "foute invoer")))
    expect_error(fit.basis(Data_basis_fout))
    expect_error(fit.basis(Data.basis %>% select(-logOmtrek)))
    expect_error(fit.basis(Data.basis %>% mutate(logOmtrek = logOmtrek - 1)))
    expect_error(fit.basis(Data.basis %>% select(-logOmtrek2)))
    expect_error(fit.basis(Data.basis %>% mutate(logOmtrek2 = logOmtrek2 - 1)))

    expect_error(fit.basis(Data.basis %>% mutate(nBomenInterval = 101)))
    expect_error(fit.basis(Data.basis %>% mutate(nBomenInterval = 101)))
    expect_error(fit.basis(Data.basis %>%
                             mutate(Omtrek =
                                      ifelse(Omtrek == 0.35, 0.40, Omtrek))))
    expect_error(fit.basis(Data.basis %>% mutate(Status = "foute invoer")))
  })

  Basismodel <- fit.basis(Data.basis)
  Kolomnamen <-
    c("DOMEIN_ID", "BOS_BHI", "nBomenInterval", "nBomenOmtrek05", "nBomen",
      "Q5k", "Q95k", "Omtrek", "H_VL_finaal", "IDbms", "C13", "HOOGTE",
      "Status", "ID", "Rijnr", "logOmtrek", "logOmtrek2", "nBomenTotOmtrek05",
      "Q5", "Q95", "BMS")

  it("Output van fit.afgeleid is correct (list met 2 tibbles)", {
      expect_error(fit.afgeleid(Data.afgeleid),
                   "argument \"Basismodel\" is missing, with no default")
      expect_is(fit.afgeleid(Data.afgeleid, Basismodel), "list")
      expect_is(fit.afgeleid(Data.afgeleid, Basismodel)[[1]], "data.frame")
      expect_equal(colnames(fit.afgeleid(Data.afgeleid, Basismodel)[[1]]),
                   c("DOMEIN_ID", "BMS", "Model"))
      expect_equal(fit.afgeleid(Data.afgeleid, Basismodel)[[1]] %>%
                     select(BMS, DOMEIN_ID),
                   tibble(BMS = "testboom", DOMEIN_ID = c("G", "H")))
      expect_type(fit.afgeleid(Data.afgeleid, Basismodel)[[1]]$Model,
                  "list")
      expect_s3_class(fit.afgeleid(Data.afgeleid, Basismodel)[[1]]$Model[[1]],
                      "lm")
      expect_is(fit.afgeleid(Data.afgeleid, Basismodel)[[2]], "data.frame")
      expect_equal(fit.afgeleid(Data.afgeleid, Basismodel)[[2]] %>%
                     colnames(.),
                   Kolomnamen)
      expect_equal(fit.afgeleid(Data.afgeleid, Basismodel)[[2]] %>%
                     select(-H_VL_finaal, -BMS) %>%
                     filter(!is.na(C13)) %>%
                     arrange(C13, HOOGTE) %>%
                     as.data.frame(., stringsAsFactors = FALSE),
                   Data.afgeleid %>%
                     select(setdiff(Kolomnamen, "H_VL_finaal"), -BMS) %>%
                     arrange(C13, HOOGTE) %>%
                     as.data.frame(., stringsAsFactors = FALSE))
  })

  Data_afgeleid_fout <- Data.afgeleid
  Data_afgeleid_fout$HOOGTE[1] <- -1

  it("Foutcontrole in fit.afgeleid gebeurt correct", {
    expect_error(fit.afgeleid(Data.afgeleid %>% select(-BMS),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% select(-DOMEIN_ID),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% select(-HOOGTE),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% mutate(HOOGTE = "foute invoer"),
                              Basismodel))
    expect_error(fit.afgeleid(Data_afgeleid_fout,
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% select(-logOmtrek),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>%
                                mutate(logOmtrek = logOmtrek - 1),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% select(-logOmtrek2),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>%
                                mutate(logOmtrek2 = logOmtrek2 - 1),
                              Basismodel))

    expect_error(fit.afgeleid(Data.afgeleid %>% mutate(nBomenInterval = 21),
                              Basismodel))
    expect_error(fit.afgeleid(Data.afgeleid %>% mutate(nBomenInterval = 21),
                              Basismodel))
  })

  it(
  "Output v fit.lokaal is correct(tibble met velden BMS, DOMEIN_ID en Model)", {
      expect_is(fit.lokaal(Data.lokaal), "data.frame")
      expect_equal(colnames(fit.lokaal(Data.lokaal)),
                   c("BMS", "DOMEIN_ID", "Model"))
      expect_equal(fit.lokaal(Data.lokaal) %>%
                     select(BMS, DOMEIN_ID),
                   tibble(BMS = "andereboom", DOMEIN_ID = c("A", "B")))
      expect_type(fit.lokaal(Data.lokaal)$Model,
                  "list")
      expect_s3_class(fit.lokaal(Data.lokaal)$Model[[1]], "lm")
  })

  setwd(wd)

})
