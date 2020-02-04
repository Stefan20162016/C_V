#!/bin/bash

#
# Aufgabe2.sh eine von vielen Loesungen
#


# in $# steht die Anzahl der Parameter, die genau gleich 2 sein sollen
if ! (( "$#" == 2 ))
 then
   echo $0 benoetigt zwei Parameter
   exit
fi

### optional kann man auch nochmal nachfragen ob die Endungen so korrekt sind:

### In den Positionsparameter $1 steht die alte und in $2 die neue Endung
#echo Willst du \*.$1 nach \*.$2 verschieben ? Dann druecke enter, sonst CTRL+C
### read liest zeilenweise von der Standardeingabe und mit ENTER gehts weiter
#read

# wie in Aufgabe1 nur jetzt mit den Positionsparametern $1 und $2
for i in *.$1
 do
  mv $i ${i%.*}.$2
  # oder wieder mit Befehlssubstitution und sed
done

exit 0
