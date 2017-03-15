library(RODBC)
library(readr)
library(dplyr)
library(tidyr)
library(dhcurve)

setwd("C:/R/GitRepositories/dhcurve")

#ophalen gegevens
connectieGegs2016bosniveau <- odbcConnectAccess2007("C:/R/gegevens/project_DHcurves/meetgegevens/GegevensGelinktOpBosniveauAanBHIvsJan2016.accdb")

query40_T4 <- "SELECT DOMEIN_ID, BOS_BHI, BOS_OPNAME_TYPE_OMSCH, BMS, TYPE_METING, C13, HOOGTE1, HOOGTE2, HOOGTE3, AANTAL, JAAR FROM tblqry40_T4_mergeBHI"
TreesT4_2016bos <- sqlQuery(connectieGegs2016bosniveau,query40_T4,stringsAsFactors = FALSE)

query42_T7 <- "SELECT DOMEIN_ID, BOS_BHI, BOS_OPNAME_TYPE_OMSCH, BMS, TYPE_METING, C13, HOOGTE, JAAR FROM tblqry42_T7_mergeBHI"
TreesT7_2016bos <- sqlQuery(connectieGegs2016bosniveau,query42_T7,stringsAsFactors = FALSE)

odbcClose(connectieGegs2016bosniveau)

connectieGegs2016bestandniveau <- odbcConnectAccess2007("C:/R/gegevens/project_DHcurves/meetgegevens/NieuweMeetgegevensGelinktOpBestandsniveauAanBHIvsJan2016.accdb")

query_nieuw <- "SELECT DOMEIN_ID, BOS_BHI, BMS, C13, HOOGTE, JAAR, GUID_BestandBHI FROM tblResultHoogtemetingen_all"
Trees_nieuw <- sqlQuery(connectieGegs2016bestandniveau,query_nieuw,stringsAsFactors = FALSE)

odbcClose(connectieGegs2016bestandniveau)

Data <- TreesT4_2016bos %>%
  gather(key = magweg, value = HOOGTE, -(1:6), -AANTAL, -JAAR) %>%
  filter(HOOGTE != 0) %>%
  transmute(DOMEIN_ID, BOS_BHI, BOS_OPNAME_TYPE_OMSCH, BMS, TYPE_METING, C13, HOOGTE, JAAR) %>%
  rbind(.,TreesT7_2016bos,
        data.frame(Trees_nieuw[,1:2],
                   BOS_OPNAME_TYPE_OMSCH = "VersneldeOpmeting",
                   BMS = Trees_nieuw$BMS,
                   TYPE_METING = "nieuw",
                   Trees_nieuw[,4:6])) %>%
  left_join(.,read_tsv("ANB/domeingegs.txt"),by = "DOMEIN_ID") %>%
  left_join(.,read_tsv("domeingegs_bodem.txt"),by = c("DOMEIN_ID", "BOS_BHI")) %>%
  mutate(
    BMS = ifelse(BMS == "Grove den (Gewone den)","GroveDen",BMS)
  ) %>%
  filter(
    HOOGTE < 60,
    !grepl(" (G)",BMS, fixed = TRUE)
  )


#hier beginnen de functies
Datalijst <- initiatie(Data)

#voorbeeld als je de limiet voor gebruik van de gegevens van cultuurpopulier in de Palingbeek om het model te berekenen, wil optrekken van 50 naar 60 bomen
# Uitzonderingen <- data.frame(DOMEIN_ID = "DomWVL09172", BMS = "cultuurpopulier",
#                              min_basis = 60, min_afgeleid = NA,
#                              stringsAsFactors = FALSE)
#
# Datalijst <- initiatie(Data, Uitzonderingen)


Data.basis <- Datalijst[["Basis"]]
Data.afgeleid <- Datalijst[["Afgeleid"]]
Data.extra <- Datalijst[["Domein"]]
Data.rest <- Datalijst[["Rest"]]

Basismodel <- fit.basis(Data.basis)
AfwijkendeMetingen <- validatie.basis(Basismodel)
write.csv2(AfwijkendeMetingen, "AfwijkendeMetingenBasis.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

Afgeleidmodel <- fit.afgeleid(Data.afgeleid, Basismodel)
AfwijkendeMetingen2 <- validatie.afgeleid(Basismodel, Afgeleidmodel)
write.csv2(AfwijkendeMetingen2, "AfwijkendeMetingenAfgeleid.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!

#De volgende modellen zijn onafhankelijk van de voorgaande en kunnen dus onafhankelijk berekend worden
Extramodellen <- fit.extra(Data.extra)
AfwijkendeMetingen3 <- validatie.basis(Extramodellen, Data.extra)
write.csv2(AfwijkendeMetingen3, "AfwijkendeMetingenExtra.csv")
#metingen nakijken en vlaggen in de databank vooraleer verder te gaan!


Resultaat <- resultaat(Basismodel, Afgeleidmodel, Extramodellen, Data.extra, Data.rest)
write.csv2(Resultaat, "DHcurves.csv")
