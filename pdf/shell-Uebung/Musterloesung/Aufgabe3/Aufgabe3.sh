#!/bin/bash

#
# Aufgabe3.sh eine (umfangreiche) von vielen Loesungen
#
# man kann z.B. die Ueberpruefungen weglassen und einfach
# tar cfvz archiv/`date +%Y-%m-%d`.tgz $* 
# benutzen, dabei werden auch keine temporaeren Verzeichnisse
# angelegt
#


# in $DATE steht das heutige Datum in einem speziellen Format
DATE=$(date +%Y-%m-%d)

# Wenn weniger als ein Parameter angegeben wurde, exit
if  (( "$#" < 1 )) 
 then 
   echo $0 will als Parameter ein Muster welche Dateien archiviert werden sollen
   exit
fi

# Verzeichnis archiv erstellen, wenn es noch nicht existiert, bei Fehler exit
if [ ! -d archiv ]; then
 mkdir archiv || exit
fi
 
# zur Sicherheit einfach mal vorher loeschen
rm -rf archiv/$DATE
mkdir archiv/$DATE

# alle Dateien kopieren so wie die Argumente von der Shell interpretiert werden
cp -a "$*" archiv/$DATE
cd archiv 
tar cfvz $DATE.tgz $DATE

if [[ "$?" == 0 ]]
 then
  echo Archiv $DATE.tgz erfolgreich erstellt
  #temporaeres Backupverzeichnis wieder loeschen
  rm -rf $DATE
else
  echo Fehler beim Archivieren!!!
  exit 1
fi

exit 0
