#' Leest tekst in file van package dhcurve
#'
#' Idee is om in het package zelf (in de map 'inst') 2 gebruikerspecifieke tekstbestandjes toe te voegen met het path waar de databank opgeslagen is (=path) en evt. met het path wat als working directory gebruikt zal worden (=wd).  Deze laatste is facultatief, als deze niet opgegeven wordt, zal de eerder opgegeven working directory gebruikt worden.  Deze functie kan in de hoofdfunctie of andere scripts buiten het package gebruikt worden om deze (vaste) locatie op te roepen, wat als voordeel biedt dat de link naar de databank bij verplaatsen van de databank niet in elk script aangepast moet worden (maar enkel in dat ene bestand).
#'
#' @param Bestandsnaam  Naam van het tekstbestandje dat in het package opgeslagen is, bv. "path"
#'
#' @return Inhoud van het opgegeven tekstbestandje
#'
#' @export
#'

leesFile <- function(Bestandsnaam) {

  if (file.exists(system.file(Bestandsnaam, package = "dhcurve"))) {
    inhoud <-
      tryCatch(inhoud <- scan(file = system.file(Bestandsnaam, package = "dhcurve"),
                            what = "character", quiet = TRUE),
               error = function(e) {
                 print(paste("Error: Geen bestand",Bestandsnaam,"gevonden, zie functie leesFile voor meer info"))
                 return(NULL)
               },
               warning = function(w) {
                 print(paste("Error: Geen bestand",Bestandsnaam,"gevonden, zie functie leesFile voor meer info"))
                 return(NULL)
               },
               message = FALSE)
  } else {
    inhoud <- NULL
  }

  return(inhoud)

}
