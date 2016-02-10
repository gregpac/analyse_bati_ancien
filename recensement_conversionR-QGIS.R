#DRAFT-2015-06-30
#conversion dees données fichiers csv originaux vers csv issu de QGIS
#Grégoire Paccoud 2015
#Craterre-ENSAG, labex AE&CC ()
#licence CC-by-sa
setwd ("/Users/gregoirepaccoud/R/CDcadastre2013/RELEVANT")

recensement<-read.csv(file="relevant_recensement_QGISconvert_140901.csv", header=TRUE)
#essai code 1
#rec_entrees<-length(recensement$groupID)
#for(i in seq_along(recensement$groupID)) {
#  ligne<-subset (recensement, groupID=i)
#  if (ligne[8]=="1") {ligne[21]<-1}
#  else if (recensement$colombage=="1") {recensement$TECHNIQUES<-3}
#  else if (recensement$torchis=="1") {recensement$TECHNIQUES<-8}
#  else if (recensement$brique=="1") {recensement$TECHNIQUES<-2}
#  else if (recensement$mod_contemp=="1") {recensement$TECHNIQUES<-7}
#  else if (recensement$machefer=="1") {recensement$TECHNIQUES<-9}
#  else if (recensement$pierre=="1") {recensement$TECHNIQUES<-5}
#  else if (recensement$bois=="1") {recensement$TECHNIQUES<-10}
#  else if (recensement$metal=="1") {recensement$TECHNIQUES<-11}
#  else if (recensement$demoli=="1") {recensement$TECHNIQUES<-6}
#}
#write.csv(recensement, file="recensement_convert.csv")

#essai code 2
for(i in seq_along(recensement$groupID)) {
  ligne<-subset (recensement, groupID==i)
  if (ligne[8]=="1") {ligne[21]<-1}
  else if (ligne[9]=="1") {ligne[21]<-3}
  else if (ligne[10]=="1") {ligne[21]<-8}
  else if (ligne[11]=="1") {ligne[21]<-2}
  else if (ligne[7]=="1") {ligne[21]<-7}
  else if (ligne[12]=="1") {ligne[21]<-9}
  else if (ligne[13]=="1") {ligne[21]<-5}
  else if (ligne[14]=="1") {ligne[21]<-10}
  else if (ligne[15]=="1") {ligne[21]<-11}
  else if (ligne[19]=="1") {ligne[21]<-6}
  l'i'<-ligne
}

write.csv(recensement, file="recensement_convert.csv")


#live test code 1-fonctionne
setwd ("/Users/gregoirepaccoud/R/CDcadastre2013/RELEVANT")
recensement<-read.csv(file="relevant_recensement_QGISconvert_140901.csv", header=TRUE)
ligne<-subset (recensement, groupID=="1")
if (!is.na(ligne$pise) & ligne$pise=="1") {ligne$TECHNIQUE<-1}
if (!is.na(ligne$colombage) & ligne$colombage=="1") {ligne$TECHNIQUE<-3}
if (!is.na(ligne$torchis) & ligne$torchis=="1") {ligne$TECHNIQUE<-8}
if (!is.na(ligne$brique) & ligne$brique=="1") {ligne$TECHNIQUE<-2}
if (!is.na(ligne$mod_contemp) & ligne$mod_contemp=="1") {ligne$TECHNIQUE<-7}
if (!is.na(ligne$machefer) & ligne$machefer=="1") {ligne$TECHNIQUE<-9}
if (!is.na(ligne$pierre) & ligne$pierre=="1") {ligne$TECHNIQUE<-5}
if (!is.na(ligne$bois) & ligne$bois=="1") {ligne$TECHNIQUE<-10}
if (!is.na(ligne$metal) & ligne$metal=="1") {ligne$TECHNIQUE<-11}
if (!is.na(ligne$demoli) & ligne$demoli=="1") {ligne$TECHNIQUE<-6}
l1<-ligne


#live test code 2.1 : iteration avec apply et fonction- marche pas.
#convertQGIS<-function () {
#  if (pise=="1") {TECHNIQUE<-1}
#  if (colombage=="1") {TECHNIQUE<-3}
#  if (torchis=="1") {TECHNIQUE<-8}
#  if (brique=="1") {TECHNIQUE<-2}
#  if (mod_contemp=="1") {TECHNIQUE<-7}
#  if (machefer=="1") {TECHNIQUE<-9}
#  if (!is.na(pierre) & pierre=="1") {TECHNIQUE<-5}
#  if (!is.na(bois) & bois=="1") {TECHNIQUE<-10}
#  if (!is.na(metal) & metal=="1") {TECHNIQUE<-11}
#  if (!is.na(demoli) & demoli=="1") {TECHNIQUE<-6}
#}

#recensementQGIS<-apply(recensement, 1,convertQGIS)


