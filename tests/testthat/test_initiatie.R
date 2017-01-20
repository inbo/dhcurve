context("test initiatie")

#voor testfuncties: evt. een map 'helper' onder tests zetten en daarin functies zetten om een dataset te genereren vanuit een welbepaald model (met bepaalde rmse, outliers,...).  Deze runt hij voor de tests

Dataset <- data.frame(DOMEIN_ID = c(rep("Bos1",155),
                                    rep("Bos2",110),
                                    rep("Bos3",55),
                                    rep("Bos4",55),
                                    rep("Bos5",55),
                                    rep("Bos6",100),
                                    rep("BosKlein",15)),
                      BMS = c(rep("SoortModel",55),
                              rep("SoortExtra",55),
                              rep("SoortNiet",45),
                              rep("SoortModel",55),
                              rep("SoortExtra",55),
                              rep("SoortModel",55),
                              rep("SoortModel",55),
                              rep("SoortModel",55),
                              rep("SoortModel",55),
                              rep("SoortExtra",45),
                              rep("SoortModel",15)),
                      C13 = 100,
                      HOOGTE = 20,
                      stringsAsFactors = FALSE)

Resultaat1 <- data.frame(nBomenInterval = 55,
                     nBomenOmtrek05 = 55,
                     C13 = 100,
                     HOOGTE = 20,
                     Omtrek = 1.05)
Resultaat2 <- data.frame(logOmtrek = 0.048790164169,
                     logOmtrek2 = 0.00238048011968,
                     nBomen = 55,
                     Q5 = 0.95,
                     Q95 = 1.15)


test_that("Dataset wordt correct opgedeeld", {
  expect_equal(as.data.frame(initiatie(Dataset)[[1]]),
               data.frame(BMS = "SoortModel",
                          DOMEIN_ID = c(rep("Bos1",55),
                                        rep("Bos2",55),
                                        rep("Bos3",55),
                                        rep("Bos4",55),
                                        rep("Bos5",55),
                                        rep("Bos6",55)),
                          Resultaat1,
                          Rijnr = c(1:55,156:210,266:485),
                          Resultaat2,
                          stringsAsFactors = FALSE))
  expect_equal(as.data.frame(initiatie(Dataset)[[2]]),
               data.frame(BMS = "SoortModel",
                          DOMEIN_ID = c(rep("BosKlein",15)),
                          nBomenInterval = 15,
                          nBomenOmtrek05 = 15,
                          C13 = 100,
                          HOOGTE = 20,
                          Omtrek = 1.05,
                          Rijnr = c(531:545),
                          logOmtrek = 0.048790164169,
                          logOmtrek2 = 0.00238048011968,
                          nBomen = 15,
                          Q5 = 0.95,
                          Q95 = 1.15,
                          stringsAsFactors = FALSE))
  expect_equal(as.data.frame(initiatie(Dataset)[[3]]),
               data.frame(BMS = "SoortExtra",
                          DOMEIN_ID = c(rep("Bos1",55),
                                        rep("Bos2",55)),
                          Resultaat1,
                          Rijnr = c(56:110,211:265),
                          Resultaat2,
                          stringsAsFactors = FALSE))
})

