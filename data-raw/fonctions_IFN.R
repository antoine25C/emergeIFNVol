# #librairies------
# library(data.table)
# library(dplyr)
# library(readr)
# library(ggplot2)
# library(tidyr)
# library(sf)
# library(fasterize)
# library(elevatr) #permet d'acceder aux bases de donnÃ©es MNH
# library(raster)
# library(gstat)
# library(rlist)
# library(tcltk)
# library(colorspace)
# library(RColorBrewer)
# library(ggrepel)
# library(stringr)
# library(png)

# #fonctions -----
# import_IFN <- function(choix_dept = NULL, choix_ser = NULL, annees = NULL){
#   if (is.null(annees)){
#     warning("Attention vous n'avez pas spécifié d'année ou plage d'année sur
#             lesquelles vous souhaitiez téléchargé les données IFN,
#             auncune données n'ont donc été téléchargées")
#     return(NULL)
#   }
#   else if((annees < 2005) || (min(annees)<2005)){
#     warning("Attention l'année que vous avez spécifié ou la plus petite année de la plage
#             d'annéee spécifiée est antérieur à 2005 (première année pour lesquelles les données
#             IFN sont disponibles en libre accès sur internet)
#             Aucune donnée n'a donc été téléchargée")
#     return(NULL)
#   }
#   else if (class(annees)!= "integer"){
#     warning("l'année ou plage d'année spécifié n'est pas composé de nombre entier,
#             Aucune donnée n'a donc été téléchargée")
#     return(NULL)
#   }
#   else {
#     rep <- "https://inventaire-forestier.ign.fr/IMG/zip/" #lien permettant le téléchargemnt via R des données brutes de l'IFN
#     IFNarbres    <- data.table()
#     IFNplacettes <- data.table()
#     IFNecologie <- data.table()
#     options(warn = -1) #permet de supprimer les messages d'alerte
#     for (i in annees){
#       print(paste("les données de l'année",i,"sont en cours de téléchargement", sep = " "))
#       tempRep    <- tempdir()
#       temp       <- tempfile()
#       j <- 15 #à fixer à une valeur d'au moins 10 pour avoir les dernières données à jour de l'IFN (au 08/03/2019)
#       test <- 1
#       while (test != 0){ #boucle qui test le téléchargement à l'adresse par exempl pour 2010"https://inventaire-forestier.ign.fr/IMG/zip/2010-'valeur de j dans la boucle'.zip"
#         if (j !=0){
#           repTour    <- paste0(rep,i,"-",j,".zip")
#         } else {
#           repTour    <- paste0(rep,i,".zip")
#         }
#         test<- try(download.file(repTour, temp),silent = T)
#         j <- (j-1) #j diminue après chaque boucle pour trouver pas à pas le dernier fichier le plus à jour de la base de donnée
#       }
#
#       liste      <- unzip(temp, exdir=tempRep)
#       tabArbres     <- read_csv2(liste[grepl("arbres_foret", liste)])
#       tabPlacettes  <- read_csv2(liste[grepl("placettes_foret_2", liste)])
#       tabEcologie <- read.csv2(liste[grepl("ecologie_2", liste)])
#       tabPlacettes$Annee <- i
#       tabArbres$Annee <- i
#       tabEcologie$Annee <- i
#       IFNarbres <- rbindlist(list(IFNarbres, tabArbres), use.names=TRUE, fill=TRUE) #data.table
#       IFNplacettes <- rbindlist(list(IFNplacettes, tabPlacettes), use.names=TRUE, fill=TRUE)
#       IFNecologie <- rbindlist(list(IFNecologie, tabEcologie), use.names=TRUE, fill=TRUE)
#       unlink(temp); unlink(tempRep)
#     }
#     options(warn = 0)
#     IFN_data <- list()
#     IFN_data$IFNplacettes <- IFNplacettes
#     IFN_data$IFNarbres <- IFNarbres
#     IFN_data$IFNecologie <- IFNecologie
#
#   }
#
#   IFN_data <-select_don_IFN(IFN_data, choix_dept = choix_dept, choix_ser=choix_ser)
#
#   return(IFN_data)
# }
#
# select_don_IFN <- function(IFN_data, choix_dept = NULL, choix_ser=NULL){
#   ListPlacettes <- list()
#   ListArbres <- list()
#   ListEcologie <- list()
#
#
#   if (!is.null(choix_dept)){
#     IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
#       dplyr::select(idp,xl93,yl93,dep,ser,csa,dc,dist,Annee)%>%
#       mutate(dep = as.numeric(dep))%>%
#       filter(dep %in% choix_dept)
#     IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
#       left_join(IFN_data$IFNplacettes[,c('idp', 'dep')], by = 'idp')%>%
#       filter(dep %in% choix_dept) %>%
#       dplyr::select(idp, topo, pent2, expo, roche)
#     IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
#       left_join(IFN_data$IFNplacettes, by = c('idp','Annee'))%>%
#       left_join(IFN_data$IFNecologie, by = 'idp')%>%
#       dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche,decoupe)%>%
#       mutate(ir5 = as.numeric(ir5),
#              v = as.numeric(v))%>%
#       filter(dep %in% choix_dept)
#   }
#   if(!is.null(choix_ser) & is.null(choix_dept)){
#     IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
#       dplyr::select(idp,xl93,yl93,dep,ser,csa,dc,dist,Annee)%>%
#       mutate(dep = as.numeric(dep))%>%
#       filter(ser %in% choix_ser)
#     IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
#       left_join(IFN_data$IFNplacettes[,c('idp', 'ser')], by = 'idp')%>%
#       filter(ser %in% choix_ser) %>%
#       dplyr::select(idp, topo, pent2, expo, roche)
#     IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
#       left_join(IFN_data$IFNplacettes, by = c('idp','Annee'))%>%
#       left_join(IFN_data$IFNecologie, by = 'idp')%>%
#       dplyr::select(idp,a,simplif,espar,acci,ori,mortb,c13,ir5,age,htot,hdec,v,w,dep,ser,Annee,topo,pent2,expo,roche,decoupe)%>%
#       mutate(ir5 = as.numeric(ir5),
#              v = as.numeric(v))%>%
#       filter(ser %in% choix_ser)
#   }
#   if(!is.null(choix_ser) & !is.null(choix_dept)){
#     IFN_data$IFNplacettes <- IFN_data$IFNplacettes %>%
#       filter(ser %in% choix_ser)
#     IFN_data$IFNecologie <- IFN_data$IFNecologie %>%
#       left_join(IFN_data$IFNplacettes[,c('idp', 'ser')], by = 'idp')%>%
#       filter(ser %in% choix_ser)
#     IFN_data$IFNarbres <- IFN_data$IFNarbres %>%
#       filter(ser %in% choix_ser)
#   }
#
#   list_ser <- unique(IFN_data$IFNplacettes$ser)
#   for (Ser in list_ser){
#     IFNplacettes <- IFN_data$IFNplacettes %>%
#       filter(ser == Ser)
#     IFNarbres <- IFN_data$IFNarbres %>%
#       filter(ser == Ser)
#     IFNecologie <- IFN_data$IFNecologie %>%
#       filter(idp %in% unique(IFNplacettes$idp))
#     ListPlacettes <- list.append(ListPlacettes,IFNplacettes)
#     ListArbres <- list.append(ListArbres,IFNarbres)
#     ListEcologie <- list.append(ListEcologie,IFNecologie)
#   }
#   names(ListPlacettes) <-paste("IFNplacettes", list_ser, sep = "_")
#   names(ListArbres) <-paste("IFNarbres", list_ser, sep = "_")
#   names(ListEcologie) <-paste("IFNEcologie", list_ser, sep = "_")
#
#   IFN_data <- list()
#   IFN_data$IFNplacettes <- ListPlacettes
#   IFN_data$IFNarbres <- ListArbres
#   IFN_data$IFNecologie <- ListEcologie
#   return(IFN_data)
# }
#
# classif_cat_Diam <- function(IFN_data){
#   listeD <- seq(2.5,147.5, by = 5)
#   list_Cat <-append(rep("régé",2),c(rep("Perche",2),rep("PB",2),rep("BM",3), rep("GB",4),
#                                     rep("TGB",(length(listeD)-13))))
#   corres_cat <- data.table(listeD,list_Cat)
#
#   for (j in 1:length(names(IFN_data$IFNarbres))){
#     IFN_data$IFNarbres[[j]] <- IFN_data$IFNarbres[[j]]%>%
#       mutate(catD = NA,
#              grd_catD = NA)
#     for (i in corres_cat$listeD){
#       IFN_data$IFNarbres[[j]]$catD[IFN_data$IFNarbres[[j]]$c13<(i*pi) & IFN_data$IFNarbres[[j]]$c13>((i-5)*pi)] <- round(i-2.5, 0)
#       IFN_data$IFNarbres[[j]]$grd_catD[IFN_data$IFNarbres[[j]]$c13<(i*pi) & IFN_data$IFNarbres[[j]]$c13>((i-5)*pi)] <- corres_cat$list_Cat[corres_cat$listeD == i]
#     }
#   }
#   return(IFN_data)
# }
