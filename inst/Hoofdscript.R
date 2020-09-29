library(RODBC)
library(readr)
library(dplyr)
library(tidyr)
library(dhcurve)

dbpath <- leesFile("dbpath")

wdpath <- leesFile("wdpath")

if (!is.null(wdpath)) {
  setwd(wdpath)
}

#ophalen gegevens ----
connectieGegs <- odbcConnectAccess2007(
  paste0(dbpath, "DiamHoogteMetingen.accdb")
)

#Deze queries zijn om de gedetailleerde groepen te analyseren,
#voor Ivanho moeten de queries aangepast worden!

queryBosdat <-
  "SELECT ID, DOMEIN_ID, BOS_BHI, IDbms_bosdat AS IDbms,
     BMS_bosdat AS BMS, TYPE_METING, C13,HOOGTE, JAAR, StaandLiggend, Status
   FROM tblBosdatMetingen"
TreesBosdat <-
  sqlQuery(connectieGegs, queryBosdat, stringsAsFactors = FALSE)

queryNieuweMetingen <-
  "SELECT ID, DOMEIN_ID, BOS_BHI, IDbms_bosdat AS IDbms, BMS_bosdat AS BMS,
   C13, HOOGTE, JAAR, StaandLiggend, Status
   FROM tblNieuweHoogtemetingen"
TreesNieuweMetingen <-
  sqlQuery(connectieGegs, queryNieuweMetingen, stringsAsFactors = FALSE)

odbcClose(connectieGegs)



Data <- TreesBosdat %>%
  bind_rows(TreesNieuweMetingen %>%
              mutate(TYPE_METING = "Nieuw")) %>%
  filter(
    StaandLiggend == "staand",
    !Status %in% c("Afgekeurd","Meetfout")
  )


#hier beginnen de functies ----

# initiatie ----
Datalijst <- initiatie(Data)

#voorbeeld als je de limiet voor gebruik van de gegevens van cultuurpopulier in
#de Palingbeek om het model te berekenen, wil optrekken van 50 naar 60 bomen
# Uitzonderingen <- data.frame(DOMEIN_ID = "DomWVL09172", BMS = "cultuurpopulier",  #nolint
#                              min_basis = 60, min_afgeleid = NA,
#                              stringsAsFactors = FALSE)
#
# Datalijst <- initiatie(Data, Uitzonderingen)   #nolint


Data.basis <- Datalijst[["Basis"]]
Data.afgeleid <- Datalijst[["Afgeleid"]]
Data.lokaal <- Datalijst[["Lokaal"]]
Data.rest <- Datalijst[["Rest"]]


# FIT - basis ----
Basismodel <- fit.basis(Data.basis)
AfwijkendeMetingen <- validatie.basis(Basismodel)
write.csv2(AfwijkendeMetingen, "AfwijkendeMetingenBasis.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

#methode om status aan te passen in de db

# 1) Bosdat 
updatequeryBosdat <-
  "UPDATE tblBosdatMetingen
  SET Status = 'Te controleren'
  WHERE Status = 'Niet gecontroleerd' AND rownames IN ('%s')"

IDafwijkendeMetingen <-
  paste(unique(AfwijkendeMetingen$ID, na.rm = TRUE), collapse = "','")
query <- sprintf(updatequeryBosdat, IDafwijkendeMetingen)

connectieGegs <- odbcConnectAccess2007(
  paste0(dbpath, "DiamHoogteMetingen.accdb")
)
Test <-
  sqlQuery(connectieGegs, query)
odbcClose(connectieGegs)


# 2) Nieuwe hoogtemetingen
updatequeryNieuweMetingen <-
  "UPDATE tblNieuweHoogtemetingen
  SET Status = 'Te controleren'
  WHERE Status = 'Niet gecontroleerd' AND ID IN ('%s')"

IDafwijkendeMetingen <-
  paste(unique(AfwijkendeMetingen$ID, na.rm = TRUE), collapse = "','")

query <- sprintf(updatequeryNieuweMetingen, IDafwijkendeMetingen)

connectieGegs <- odbcConnectAccess2007(
  paste0(dbpath, "DiamHoogteMetingen.accdb")
)

Test <-
  sqlQuery(connectieGegs, query)

odbcClose(connectieGegs)



# FIT - afgeleid ----
Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)
AfwijkendeMetingen2 <- validatie.afgeleid(Basismodel, Afgeleidmodel)
write.csv2(AfwijkendeMetingen2, "AfwijkendeMetingenAfgeleid.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!


# FIT - lokaal ----
#De volgende modellen zijn onafhankelijk van de voorgaande en kunnen dus
#berekend worden zonder dat de voorgaande berekend zijn
Lokaalmodel <- fit.lokaal(Data.lokaal)
AfwijkendeMetingen3 <- validatie.lokaal(Lokaalmodel, Data.lokaal)
write.csv2(AfwijkendeMetingen3, "AfwijkendeMetingenLokaal.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!


# IVANHO Output ----
OutputIvanho <-
  outputIVANHO(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal)
write.csv2(OutputIvanho, "IVANHO.csv")


# Modelparameters per domein-bms -----
#voor een lijst met de modelparameters en foutenmarge (RMSE):
Resultaat <-
  resultaat(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal, Data.rest)
write.csv2(Resultaat, "DHcurves.csv")
