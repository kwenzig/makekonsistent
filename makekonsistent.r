# makeconsistent.r
# Ein R-Skript zur Ueberpruefung von Metadaten in questions.csv und
# answers.csv auf einige Eigenschaften und ggfs. einige Regel basierte 
# Anpassungen.
# 2014  Knut Wenzig (kwenzig@diw.de)

# This program is free software; you can redistribute it and/or modify it under 
# the terms of the GNU General Public License as published by the Free Software 
# Foundation; either version 3 of the License, or (at your option) any later 
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT 
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with 
# this program; if not, see <http://www.gnu.org/licenses/>. 


# erstmal die CSV einlesen

#path <- "S:/METADATA/SOEP-IS/questionnaire-2011-xls-and-qlib/edit/"
#path <- "S:/MA/kwenzig/Dokumentation/QuestCSV2LaTeX/CSV/IS2012panel/"
#path <- "S:/MA/kwenzig/Dokumentation/QuestCSV2LaTeX/CSV/test20140328/"
path <- "S:/MA/kwenzig/Dokumentation/QuestCSV2LaTeX/CSV/IS2011/"

questions.csv <- read.csv(paste0(path,"questions.csv"), 
                          colClasses="character",encoding="UTF-8")

answers.csv <- read.csv(paste0(path, "answers.csv"), 
                        colClasses="character", encoding="UTF-8")

### Strukturprüfung questions.csv

questions.csv$view_sort_id <- 1:dim(questions.csv)[1]

## question-id nur einmal vergeben
questions.list <- rle(questions.csv$question)$values
# folgende questions sind doppelt vergeben
question.error <- questions.list[duplicated(questions.list)]
if(length(question.error)>0) {
  question.error
  warning("question in questions.csv kommt mehrfach an verschiedenen Stellen vor, sollte aber Frage eindeutig identifizieren.")
}

## nur eine rootquestion pro question
rootquestions.csv <- questions.csv[questions.csv$item=="",]
# folgende questions haben mehr als eine rootquestion
rq.error <- rootquestions.csv$question[duplicated(rootquestions.csv$question)]
if(length(rq.error)>0) {
  warning(rq.error)
  stop ("Für diese question gibt es mehr als ein leeres item, d.h. mehr als eine rootquestion. Irreparabel!")
}


## answer_list in answer.csv
answer.list.ac <- rle(answers.csv$answer_list)$values
al.ac.warn <- answer.list.ac[duplicated(answer.list.ac)]
# wahrscheinlich doppelte IDs für answer_list in answers.csv
if(length(al.ac.warn)>0) {
  al.ac.warn
  warning ("answer_list in answers.csv kommt mehrfach an verschiedenen Stellen vor, sollte aber Antwortoptionen eindeutig identifizieren.")
}


# leere answer_list in answer.csv
table(answer.list.ac=="")
stopifnot(answer.list.ac!="")

### answers_list und scale konsistent machen
answer.list.qc <- unique(questions.csv$answer_list)

## answer_list, die in answers.csv vorhanden  ist, 
## aber nicht in questions.csv verwendet wird
## answer.csv eingrenzen auf verwendete answer_lists
answer.list.ac.warn <- setdiff(answer.list.ac, answer.list.qc)
# answer.csv eingrenzen
if(length(answer.list.ac.warn)>0) {
  answer.list.ac.warn
  answers.csv <- answers.csv[!is.element(answers.csv$answer_list,
                                         answer.list.ac.warn),]
  answer.list.ac <- rle(answers.csv$answer_list)$values
  warning("answer.csv eingegrenzt auf verwendete answer_list")
}

## answer_list, die in questions.csv verwendet wird,
## aber nicht in answers.csv vorhanden ist:
## löschen und scale in chr umwandeln
answer.list.qc.warn <- setdiff(answer.list.qc, answer.list.ac)
answer.list.qc.warn <- setdiff(answer.list.qc.warn, "")
answer.list.qc.warn
# mit einem default überschrieben
if(length(answer.list.qc.warn)>0) {
  warning(paste0(answer.list.qc.warn,sep=", "))
  answer.list.qc.warn <- is.element(questions.csv$answer_list,
                                    setdiff(answer.list.qc.warn, ""))
  questions.csv$answer_list[answer.list.qc.warn] <- ""
  questions.csv$scale[answer.list.qc.warn] <- "chr"
  warning("Korrigiert in question.csv: ",
          "answer_list (gelöscht) und scale (nach chr), ",
          "wenn answer_list in question verwendet und ",
          "in answers.csv question nicht vorhanden")
}

## scale muss gültig sein
# txt Only display the text, no variables are generated. All filters and instructions still apply.
# chr Result is a character string.
# int Result is a integer.
# dec Result is a number with decimals.
# bin Result is either true, false (equals "null")
# cat Result is a pre-defined answer category.
scale.warn <- setdiff(unique(questions.csv$scale), c("txt", "chr", "int", "dec", "bin","cat"))
# mit default überschreiben
if(length(scale.warn)>0) {
  scale.warn
  questions.csv$scale[is.element(questions.csv$scale, scale.warn)] <- "chr"
  warning(paste0(scale.warn, sep=", "))
  warning("ungültige scales überschrieben mit chr")
}

## wenn answer_list leer, darf scale nicht cat sein
# scale wird dann default (chr)
scale.warn <- questions.csv$answer_list=="" & questions.csv$scale=="cat"
if(length(intersect(scale.warn, TRUE)) > 0) {
  warning("scale=cat ohne answer_list")
  warning(questions.csv[scale.warn,c("question","item")])
  questions.csv$scale[scale.warn] <- "chr"
  warning("scale nach chr korrigiert")
}
  
## wenn answer_list gefüllt, muss scale cat sein
# scale wird dann default (cat)
scale.warn <- questions.csv$answer_list!="" & questions.csv$scale!="cat"
if(length(intersect(scale.warn, TRUE)) > 0) {
  warning("scale!=cat bei anser_list")
  warning(questions.csv[scale.warn,c("question","item")])
  questions.csv$scale[scale.warn] <- "cat"
  warning("scale nach cat korrigiert")
}


