# dhcurve 0.2

* Bij afgeleide curves vervalt het bruikbaar interval, deze worden nu
  gemodelleerd op basis van alle beschikbare metingen tussen 0,5 en 2,8 m
  (zie [issue 53](https://github.com/inbo/dhcurve/issues/53#issuecomment-1109585295))
* De grenzen waarvoor modellen gebruikt kunnen worden en dus ook berekend
  worden, worden ruimer:
    * Voor elke curve worden deze grenzen uitgebreid tot 3 klassen onder en 2
      klassen boven het interval waarbinnen de metingen vallen op basis waarvan
      het model gefit is, met een minimum van 0.2 m omtrek en 2.5 m hoogte.
      Bij basismodellen en lokale modellen is dit ten opzichte van het bruikbaar
      interval (= interval waarvan de metingen gebruikt worden om het interval
      te fitten),
      bij afgeleide modellen ten opzichte van de beschikbare metingen tussen
      0,5 en 2,8 m.
    * voor domein-boomsoortcombinaties met veel metingen boven het bruikbaar
      interval kan het bruikbare interval naar boven toe uitgebreid worden tot
      3 m met de nieuwe functie `validatie.uitbreiding()`, die volgens hetzelfde
      principe werkt als de andere validatie-functies: als een uitbreiding niet
      goed is, de minimumgrens voor het aantal metingen optrekken en de dataset
      opnieuw opsplitsen met `initiatie()`, en fitten en valideren (enkel de
      uitbreiding).
      Een klein verschil met andere validaties is dat de uiteindelijke output
      van `validatie.uitbreiding()` toegevoegd moet worden aan argument
      `Uitbreiding` in `outputIVANHO()` om de uitbreiding op te nemen in de
      resulterende dataset.

# dhcurve 0.1

* Eerste afgewerkte versie van het package `dhcurve`.
