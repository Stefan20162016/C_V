#!/bin/bash

#
# Aufgabe1.sh eine von vielen Loesungen
#

for i in *.doc

 do

  # 1.Moeglichkeit: ${%.*} entfernt die erste Endung
  mv $i ${i%.*}.txt

  # 2.Moeglichkeit mit sed: Dateiname wird mit echo ausgegeben und mit sed wird die alte Endung durch die neue ersetzt
  #mv $i $(echo $i|sed 's/doc/txt/')

done


exit 0
