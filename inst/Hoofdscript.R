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

#ophalen gegevens
connectieGegs <- odbcConnectAccess2007(
  paste0(dbpath, "DiamHoogteMetingen.accdb")
)

#Deze queries zijn om de gedetailleerde groepen te analyseren,
#voor Ivanho moeten de queries aangepast worden!

queryBosdat <-
  "SELECT rownames, DOMEIN_ID, BOS_BHI, IDbms_bosdat AS IDbms,
     BMS_bosdat AS BMS, TYPE_METING, C13,HOOGTE, JAAR, StaandLiggend, Status
   FROM tblBosdatMetingen"
TreesBosdat <-
  sqlQuery(connectieGegs, queryBosdat, stringsAsFactors = FALSE)

queryNieuweMetingen <-
  "SELECT DOMEIN_ID, BOS_BHI, IDbms_bosdat AS IDbms, BMS_bosdat AS BMS,
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
    Status != "Afgekeurd"
  )


#hier beginnen de functies
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

Basismodel <- fit.basis(Data.basis)
AfwijkendeMetingen <- validatie.basis(Basismodel)
write.csv2(AfwijkendeMetingen, "AfwijkendeMetingenBasis.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

#methode om status aan te passen in de db (enkel uitgewerkt voor 'Bosdat'-gegs):
updatequeryBosdat <-
  "UPDATE tblBosdatMetingen
  SET Status = 'Te controleren'
  WHERE Status = 'Niet gecontroleerd' AND rownames IN ('%s')"

IDafwijkendeMetingen <-
  paste(unique(AfwijkendeMetingen$rownames, na.rm = TRUE), collapse = "','")
query <- sprintf(updatequeryBosdat, IDafwijkendeMetingen)

connectieGegs <- odbcConnectAccess2007(
  paste0(dbpath, "DiamHoogteMetingen.accdb")
)
Test <-
  sqlQuery(connectieGegs, query)
odbcClose(connectieGegs)


Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)
AfwijkendeMetingen2 <- validatie.afgeleid(Basismodel, Afgeleidmodel)
write.csv2(AfwijkendeMetingen2, "AfwijkendeMetingenAfgeleid.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

#De volgende modellen zijn onafhankelijk van de voorgaande en kunnen dus
#berekend worden zonder dat de voorgaande berekend zijn
Lokaalmodel <- fit.lokaal(Data.lokaal)
AfwijkendeMetingen3 <- validatie.basis(Lokaalmodel, Data.lokaal)
write.csv2(AfwijkendeMetingen3, "AfwijkendeMetingenLokaal.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

OutputIvanho <-
  outputIVANHO(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal)
write.csv2(OutputIvanho, "IVANHO.csv")

#voor een lijst met de modelparameters en foutenmarge (RMSE):
Resultaat <-
  resultaat(Basismodel, Afgeleidmodel, Lokaalmodel, Data.lokaal, Data.rest)
write.csv2(Resultaat, "DHcurves.csv")
