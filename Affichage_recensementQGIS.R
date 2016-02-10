#DRAFT-2015-04-09
#représentation cartographique à partir des fichiers de recensement des techniques de construction du bati ancien 
#fichiers issus du formulaire QGIS pour le recensement par commune (ou CSV converti)
#Grégoire Paccoud 2015
#Craterre-ENSAG, labex AE&CC ()
#licence CC-by-sa

library("rgdal", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library(ggplot2)
library(reshape2)

#1. ___create a blank ggplot theme___
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="transparent", color='grey'),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=32)))

#2. ___read shapefile for background elements - replace "commune" by name of the shapefile, check spelling of layers!!___
setwd ("/Users/gregoirepaccoud/R/CDcadastre2013/RELEVANT")
commune.contours<-readOGR(".", "COMMUNE")
commune.bati<-readOGR(".","btiment")
#commune.voirie<-readOGR(".","VOIE_1")
commune.bati_f<-fortify(commune.bati)
commune.contours_f<-fortify(commune.contours)

#3. ___read survey file (csv from qgis)___
recensement_commune<-read.csv(file="DRAFT_relevant_recensement_QGIS_140907.csv", header=TRUE)

#4. ___extract data___
recensement_commune.df<-as.data.frame(recensement_commune)
recensement_pise<-subset(recensement_commune.df, TECHNIQUE=="1")
recensement_brique<-subset(recensement_commune.df, TECHNIQUE=="2")
recensement_colombage<-subset(recensement_commune.df, TECHNIQUE=="3")
recensement_torchis<-subset(recensement_commune.df, TECHNIQUE=="4")
recensement_moderne<-subset(recensement_commune.df, TECHNIQUE=="7")
recensement_bois<-subset(recensement_commune.df, TECHNIQUE=="10")
recensement_pierre<-subset(recensement_commune.df, TECHNIQUE=="5")
recensement_demoli<-subset(recensement_commune.df, TECHNIQUE=="6")
recensement_metal<-subset(recensement_commune.df, TECHNIQUE=="11")
recensement_machefer<-subset(recensement_commune.df, TECHNIQUE=="9")
recensement_autre<-subset(recensement_commune.df, TECHNIQUE=="8")
recensement_streetview<-subset(recensement_commune.df, COV_STVIEW=="1")
recensement_isole<-subset(recensement_commune.df, ZONE=="1")
recensement_dense<-subset(recensement_commune.df, ZONE=="2")
recensement_cadNap<-subset(recensement_commune.df, CAD_NAP=="1")
recensement_remarquable<-subset(recensement_commune.df, REMARQUABL=="1")

#5. ____global stats____
communes<-c("Relevant")

# total bati
total<-length(recensement_commune$groupID)

# % relevés (nbre renseignés) 
non_releves<-is.na(recensement_commune$TECHNIQUE)
releves<-length(recensement_commune$TECHNIQUE[!non_releves])
percent_releves<-(releves/total)*100
percent_manquant<-100-percent_releves

# % couverture streetview (nbre streetview)
couvert<-recensement_commune$COV_STVIEW==1
strtview<-length(recensement_commune$COV_STVIEW[couvert])
percent_strtview<-(strtview/total)*100

# % isolé <> urbain
isole<-recensement_commune$ZONE==1
nbre_isole<-length(recensement_commune$ZONE[isole])
dense<-recensement_commune$ZONE==2
nbre_dense<-length(recensement_commune$ZONE[dense])
percent_isole<-(nbre_isole/releves)*100
percent_dense<-(nbre_dense/releves)*100

# % moderne et contemporain <> ancien
mod_contemporain<-recensement_commune$TECHNIQUE==7
nbre_mod_contemporain<-length(recensement_commune$TECHNIQUE[mod_contemporain])
percent_mod_contemporain<-(nbre_mod_contemporain/total)*100
percent_ancien<-percent_releves - percent_mod_contemporain

# nbre bâti terre remarquables
remarquable<-recensement_commune$REMARQUABL==1
remarquable<-!is.na(remarquable)
nbre_remarquable<-length(recensement_commune$REMARQUABL[remarquable])

# (nbre dommages observés sur bâti terre)

#6. ____building materials stats____

# % pise
technique<-recensement_commune$TECHNIQUE[!non_releves]
pise<-technique==1
nbre_pise<-length(recensement_commune$TECHNIQUE[pise])
percent_pise<-(nbre_pise/releves)*100

# % brique
brique<-technique==2
nbre_brique<-length(recensement_commune$TECHNIQUE[brique])
percent_brique<-(nbre_brique/releves)*100

# % colombage
colombage<-technique==3
nbre_colombage<-length(recensement_commune$TECHNIQUE[colombage])
percent_colombage<-(nbre_colombage/releves)*100

# % torchis
torchis<-technique==8
nbre_torchis<-length(recensement_commune$TECHNIQUE[torchis])
percent_torchis<-(nbre_torchis/releves)*100

# % bois
bois<-technique==10
nbre_bois<-length(recensement_commune$TECHNIQUE[bois])
percent_bois<-(nbre_bois/releves)*100

# % pierre
pierre<-technique==5
nbre_pierre<-length(recensement_commune$TECHNIQUE[pierre])
percent_pierre<-(nbre_pierre/releves)*100

# % machefer
machefer<-technique==9
nbre_machefer<-length(recensement_commune$TECHNIQUE[machefer])
percent_machefer<-(nbre_machefer/releves)*100

# % métal
metal<-technique==11
nbre_metal<-length(recensement_commune$TECHNIQUE[metal])
percent_metal<-(nbre_metal/releves)*100

# % détruit
technique<-technique==6
nbre_detruit<-length(recensement_commune$TECHNIQUE[detruit])
percent_detruit<-(nbre_detruit/releves)*100

#7. ____history stats____

# nbre et % pre-cadNap
cadNap<-recensement_commune$CAD_NAP==1
cadNap<-!is.na(cadNap)
nbre_cadNap<-length(recensement_commune$CAD_NAP[cadNap])
percent_cadNap<-(nbre_cadNap/total)*100

# % logements avant 1949 (INSEE_ancien) <> % pisé

# % pise et moderne/contemporain dans cadNap
cadNap_compare<-subset(recensement_commune, CAD_NAP==1)

pise_cadNap<-cadNap_compare$TECHNIQUE==1
nbre_pise_cadNap<-length(cadNap_compare$TECHNIQUE[pise_cadNap])
percent_pise_cadNap<-(nbre_pise_cadNap/nbre_cadNap)*100

mod_contemp_cadNap<-cadNap_compare$TECHNIQUE==7
nbre_mod_contemp_cadNap<-length(cadNap_compare$TECHNIQUE[mod_contemp_cadNap])
percent_mod_contemp_cadNap<-(nbre_mod_contemp_cadNap/nbre_cadNap)*100

# % pise (brique colombage) dans urbain, isolé
isole_compare<-subset(recensement_commune, ZONE==1)

pise_isole<-isole_compare$TECHNIQUE==1
nbre_pise_isole<-length(isole_compare$TECHNIQUE[pise_isole])
percent_pise_isole<-(nbre_pise_isole/nbre_isole)*100

dense_compare<-subset(recensement_commune, ZONE==2)
pise_dense<-dense_compare$TECHNIQUE==1
nbre_pise_dense<-length(dense_compare$TECHNIQUE[pise_dense])
percent_pise_dense<-(nbre_pise_dense/nbre_dense)*100

#8. ___plot stat data : c(plot x, plot y, text size, variable to print)___

# encart pise
encart_nbre_pise<-c(850500,6558700,8,ceiling(nbre_pise))
encart_percent_pise<-c(850900,6558700,20,signif(percent_pise,3))
encart_pise_isole<-c(851350,6558700,8,signif(percent_pise_isole,3))
encart_pise_dense<-c(851650,6558700,8,signif(percent_pise_dense,3))
encart_pise<-rbind(encart_nbre_pise, encart_percent_pise, encart_pise_isole, encart_pise_dense)
dimnames(encart_pise)<-list(c("nombre", "part", "isole", "dense"),c("longMeans", "latMeans","textSize", "texte")) 
encart_pise.df<-as.data.frame(encart_pise)
encart_pise_f<-fortify(encart_pise.df)


# encart brique
encart_nbre_brique<-c(850500,6558400,8,ceiling(nbre_brique))
encart_percent_brique<-c(850900,6558400,8,signif(percent_brique,2))
encart_brique<-rbind(encart_nbre_brique, encart_percent_brique)
dimnames(encart_brique)<-list(c("nombre", "part"),c("longMeans", "latMeans", "textSize", "texte")) 
encart_brique.df<-as.data.frame(encart_brique)
encart_brique_f<-fortify(encart_brique.df)


# encart couverture
encart_total_bati<-c(850500,6559050,20,ceiling(total))
encart_percent_couvert<-c(850900,6558950,12,signif(percent_releves,3))
encart_percent_strtview<-c(850900,6559150,8,signif(percent_strtview,3))
encart_percent_isole<-c(851350,6559050,8,signif(percent_isole,3))
encart_percent_dense<-c(851650,6559050,8,signif(percent_dense,3))
encart_couverture<-rbind(encart_total_bati, encart_percent_couvert, encart_percent_strtview, encart_percent_isole, encart_percent_dense)
dimnames(encart_couverture)<-list(c("bati","couvTotale", "streetview", "isolé", "dense"),c("longMeans", "latMeans", "textSize", "texte")) 
encart_couverture.df<-as.data.frame(encart_couverture)
encart_couverture_f<-fortify(encart_couverture.df)


# encart histoire >>> disques surfaces/part 


#9. ___plot map. don't forget to change file name ("title") if needed___
commune.map<-ggplot() +
  geom_path(data=commune.contours_f, aes(long,lat, group=group)) +
  geom_point(data=recensement_pise, aes(x=longMeansID, y=latMeansID), size=4, alpha=0.5, colour='orange')+
  geom_point(data=recensement_brique, aes(x=longMeansID, y=latMeansID), size=4, alpha=0.4, colour='red')+
 # geom_point(data=recensement_moderne, aes(x=longMeansID, y=latMeansID), size=3, alpha=0.2, colour='gray')+
  geom_path(data=commune.bati_f, aes(long,lat, group=group), size=0.3, colour='grey') +
  geom_point(data=recensement_cadNap, aes(x=longMeansID, y=latMeansID), shape=8, size=3, colour='black')+
  geom_point(data=recensement_remarquable, aes(x=longMeansID, y=latMeansID), shape=1, size=5, colour='red')+
#  geom_text(data=recensement_commune, aes(x=longMeansID,y=latMeansID, label=groupID), size=2, colour='black') +
  geom_text(data=encart_pise_f, aes(x=longMeans,y=latMeans, label=texte,size=textSize), colour='orange') + scale_size(range=c(10,30), guide=FALSE) +
  geom_text(data=encart_brique_f, aes(x=longMeans,y=latMeans, label=texte,size=textSize), colour='red') + scale_size(range=c(10,30), guide=FALSE) +
  geom_text(data=encart_couverture_f, aes(x=longMeans,y=latMeans, label=texte,size=textSize), colour='black') + scale_size(range=c(10,30), guide=FALSE) +
  labs(title="Recensement du bâti ancien à Relevant") +
  coord_equal() +
  theme_opts
           
#10. create svg file. replace "commune" by name of the shapefile !
ggsave("QGISTESTrelevant140907.svg", plot=commune.map, width=40, height=40)