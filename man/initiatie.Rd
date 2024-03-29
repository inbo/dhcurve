% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initiatie.R
\name{initiatie}
\alias{initiatie}
\title{Initiatiestap met opsplitsen van dataset volgens modeltypen}
\usage{
initiatie(
  Data,
  Uitzonderingen = data.frame(DOMEIN_ID = "", BMS = "", min_basis = NA_integer_,
    min_afgeleid = NA_integer_, stringsAsFactors = FALSE),
  Bestandsnaam = "VerwijderdeGegevensInitiatie.html",
  verbose = TRUE,
  PathWD = getwd()
)
}
\arguments{
\item{Data}{Dataframe met metingen van bomen die als basis moeten dienen om
diameter-hoogtecurves op te stellen.  De dataframe moet de velden \code{DOMEIN_ID}
(identificatienummer voor domein), \code{BOS_BHI} (domeinnaam), \code{IDbms}
(identificatienummer voor boomsoort), \code{BMS} (boomsoort), \code{C13} (omtrek in
centimeter, gemeten op 1,3 m hoogte), \code{HOOGTE} (in meter) en \code{Status}
bevatten en mag eventueel velden \code{TYPE_METING} en \code{JAAR} bevatten (die worden
bij \code{rmse.basis()} als groeperende variabelen gebruikt).
\code{Status} mag enkel gegevens met status "Niet gecontroleerd", "Te controleren"
of "Goedgekeurd" bevatten,
dus gegevens met status "Afgekeurd" of "Negeren" moeten vooraf verwijderd
worden.}

\item{Uitzonderingen}{Lijst met boomsoort-domeincombinaties waarvoor
uitzonderingen gelden voor de limieten van minimum 50 en minimum 10 bomen.
De dataframe moet de velden \code{DOMEIN_ID}, \code{BMS}, \code{min_basis} (= vervangende
waarde voor 50) en \code{min_afgeleid} (= vervangende waarde voor 10) bevatten.
Daarnaast kan in een extra veld \code{min_uitbreiden_model} aangegeven worden
vanaf hoeveel metingen een uitbreiding naar hogere omtrekklassen opnieuw
bekeken moet worden (= vervangende waarde voor 10).
De waarde NA geeft telkens aan dat de default gebruikt mag worden.
De default voor dit argument is dat er geen uitzonderingen zijn
(wat sowieso het geval is als de curves nog niet gevalideerd zijn).}

\item{Bestandsnaam}{Een naam voor het validatierapport (\code{.html}-bestand) dat
gegenereerd wordt, bestaande uit een string die eindigt op \code{.html}}

\item{verbose}{Dit geeft de toestand van het systeem aan en zorgt ervoor dat
boodschappen niet onnodig gegeven worden.  (Defaultwaarde behouden.)}

\item{PathWD}{Het path van de working directory, dus het path waarin het
validatierapport opgeslagen moet worden (default wordt het in de op dat
moment actieve working directory opgeslagen).}
}
\value{
Als er gegevens verwijderd worden, genereert de functie een validatierapport
(\code{.html}-bestand) waarin een overzicht gegeven wordt van de verwijderde
gegevens, dit zijn gegevens met omtrek > 3 m en omtrek < 0.2 m.

De functie geeft een list van dataframes terug, met in elke dataframe
behalve de variabelen uit \code{Data} de berekende variabelen
\code{Omtrek} (= omtrekklasse), \code{logOmtrek}, \code{logOmtrek2}, \code{Q5k} en \code{Q95k} (de
grenzen van het bruikbaar interval), \code{nBomen} (= aantal metingen behalve de
verwijderde gegevens),
\code{nBomenOmtrek05} (aantal metingen met omtrek > 0.5 m en < 2.8 m),
\code{nBomenInterval} (= aantal metingen binnen het bruikbaar interval),
\code{nBomenIntervalOmtrek05} (aantal metingen binnen het bruikbaar interval
met omtrek > 0.5 m)
en \code{nExtra} (aantal metingen boven het bruikbaar interval op basis waarvan
een uitbreiding van de curve naar hogere omtrekklassen gevalideerd zou
kunnen worden)

De 4 dataframes die achtereenvolgens in de list zitten, zijn:
\itemize{
\item \verb{[["Basis"]]} gegevens van boomsoorten waarvoor meer dan 50 metingen
(binnen het bruikbaar interval met omtrek > 0.5 m) beschikbaar zijn in
minimum 6 domeinen, waarbij enkel gegevens worden opgenomen van de
domeinen waarvoor minimum 50 metingen beschikbaar zijn.  Op basis van
deze dataset kan een basismodel berekend worden, bestaande uit een
Vlaams model per boomsoort en domeinspecifieke modellen.
\item \verb{[["Afgeleid"]]} gegevens van domeinen met minder metingen (< 50
metingen binnen het bruikbaar interval en > 10 metingen boven 0.5 m)
van boomsoorten waarvoor een Vlaams model berekend kan worden
(dus boomsoorten die in dataset "Basis" voorkomen), op basis
waarvan een afgeleid model berekend kan worden.
\item \verb{[["Lokaal"]]} gegevens van domeinen met veel metingen voor een
boomsoort (> 50 metingen binnen het bruikbaar interval met
omtrek > 0.5 m) waarvan er te weinig domeinen (< 6) zijn
met voldoende metingen om een Vlaams model op te stellen. Voor deze
boomsoort-domeincombinaties kan een lokaal model berekend worden.
\item \verb{[["Rest"]]} metingen van de boomsoort-domeincombinaties die niet tot
de 3 voorgaande categorieën behoren en waar dus geen model voor
opgesteld kan worden.
}

Voor de eerste en derde dataframe worden metingen binnen het bruikbaar
interval gemarkeerd als \code{VoorModelFit} en ook metingen boven dit interval
tot een omtrek van 3 m worden bijgehouden voor een eventuele uitbreiding van
het model (tot maximaal 10 omtrekklassen);
voor het afgeleid model (2de dataframe) worden de metingen met omtrek tussen
0,5 m en 2,8 m bijgehouden.

In geval er gegevens verwijderd zijn, wordt aan de list een extra dataframe
\verb{[["VerwijderdeGegevens"]]} toegevoegd met de gegevens uit het
validatierapport.
}
\description{
De functie \code{initiatie()} maakt de dataset klaar voor de verdere stappen van
het opstellen van diameter-hoogtecurves: ze berekent de noodzakelijke
variabelen en splitst de dataset op basis van het aantal metingen per
boomsoort-domeincombinatie in 4 delen:
3 delen die als parameter meegegeven kunnen worden voor de
\code{fit.xxx()}-functies per modeltype (basismodel, afgeleid model en lokaal
model), en een vierde deel met de resterende gegevens.
}
