context("test testdataset")

describe("testdataset", {

  library(dplyr)
  library(nlme)

  nBomenDomein <- c(100, 50, 200, 100, 150, 65, 80)
  minOmtrek <- 20
  maxOmtrek <- 239

  it("testdataset() geeft geen warnings", {
    expect_no_warning(testdataset(nBomenDomein))
  })

  it("Gegenereerde dataset heeft juiste aantal records", {
    expect_equal(nrow(testdataset(nBomenDomein)),
                 sum(nBomenDomein))
    expect_equal(nrow(testdataset(nBomenDomein) %>%
                        select(BMS) %>%
                        distinct()),
                 1)
    expect_equal(nrow(testdataset(nBomenDomein) %>%
                        select(DOMEIN_ID) %>%
                        distinct()),
                 length(nBomenDomein))
  })

  it("Gegenereerde dataset heeft gevraagde range voor omtrek (C13)", {
    expect_gte(min(testdataset(nBomenDomein)$C13),
              minOmtrek)
    expect_lte(max(testdataset(nBomenDomein)$C13),
               maxOmtrek)
  })

  Data <- testdataset(nBomenDomein) %>%
    mutate(
      Omtrek = floor(C13 / 10) / 10 + 0.05,
      Rijnr = seq_along(C13),
      logOmtrek = log(Omtrek),
      logOmtrek2 = logOmtrek ^ 2,
      Testgroep = (row_number(DOMEIN_ID) - 1) %% 6 + 1
    )

  Model <-
    lme(HOOGTE ~ logOmtrek + logOmtrek2,
    random = ~ (logOmtrek + logOmtrek2) | DOMEIN_ID,
    data = Data,
    control = lmeControl(opt = "optim", singular.ok = TRUE,
                         returnObject = TRUE))

  it("Gegenereerde dataset heeft gevraagde modelparameters", {
    expect_equal(fixef(Model)[[1]],
                 30, tolerance = 1)
    expect_equal(fixef(Model)[[2]],
                 15, tolerance = 1)
    expect_equal(fixef(Model)[[3]],
                 1, tolerance = 1)
  })


  Data_result <- data.frame(NULL)
  for (i in 1:6) {
    Data_test <- Data[Data$Testgroep == i, ]
    Data_model <- Data[Data$Testgroep != i, ]

    Modeli <- lme(HOOGTE ~ logOmtrek + logOmtrek2,
                  random = ~ (logOmtrek + logOmtrek2) | DOMEIN_ID,
                  data = Data_model,
                  control = lmeControl(opt = "optim", singular.ok = TRUE,
                                       returnObject = TRUE))

    Data_Boomsoort <- Data_test %>%
      mutate(
        H_model = predict(Modeli, newdata = .),
        Resid = HOOGTE - H_model,
        Resid2 = Resid ^ 2
      )

    Data_result <- Data_result %>%
      bind_rows(Data_Boomsoort)
  }

  Rmse <- Data_result %>%
    group_by(
      DOMEIN_ID
    ) %>%
    summarise(
      sse = sum(c(Resid2)),
      nBomen = n()
    ) %>%
    ungroup() %>%
    mutate(
      rmse = sqrt(sse / (nBomen - 2))
    )

  it("De rmse ligt voor elke domeincurve rond 2", {
    expect_equal(mean(Rmse$rmse),
                 2, tolerance = 1)
    expect_lt(max(Rmse$rmse), 3.5)
    expect_gt(min(Rmse$rmse), 0.5)
  })
})
