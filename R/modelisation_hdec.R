#' Fonction unique interragissant avec l'utilisateur et réalisant l'ensemble des actions liées
#' à la modélisation de hauteur de décrochement
#' (de l'import des données brutes IFN à la calibration de modèles et sorties graphiques)
#'
#' Cette fonction permet d'effectuer en une seule étape l'ensemble des modélisations de
#' prédiction de la hauteur de décrochement. Cette fonction interagie avec l'utilisateur et fait appel
#' aux fonctions suivantes : IFNtraitement::import_IFN(...), IFNtraitement::classif_cat_Diam(...)
#' emergeIFNVol::BD_Vol(...), emergeIFNVol::Nb_arbre_hdec_bois_fort(...),
#' emergeIFNVol::modelisation(...), emergeIFNVol::graph_res_modelisation(...)
#'
#' @param dos_projet chemin vers le dossier où l'on enregistre toutes les fichiers issue de la fonction (par défaut vaut NULL)
#'
#' @return Cette fonction renvoie le chemin vers le dossier où sont enregistré les modéles qui ont
#' été calibrés ainsi que les graphiques d'analyse de ces modèles. Elle enregistre en fait sous
#' forme d'archive RDS le'on résultat de la fonction emergeIFNVol::modelisation(...)
#' @export
#'
#' @import tcltk dplyr stats rlist stringr sf ggplot2 png grid svDialogs
#'
#' @examples pas d'exemple disponible actuellement
modeles_hdec_IFN <- function(dos_projet = NULL){
  #-------------------choix du dossier de travail-----
  if (is.null(dos_projet)){
    dir <-try(choose.dir(caption = "choisir le dossier où seront enregistrer toutes les données"))
    name <- svDialogs::dlg_input(message = "entrez le nom du dossier qui sera creer pour stocker toutes les donnees de ce projet")
    name <- name$res
    dos_projet <- paste(dir, name, sep = "/")
  }
  if(!dir.exists(dos_projet)){
    dir.create(dos_projet, showWarnings = F)
  }
  if(!dir.exists(paste(dos_projet,"Rdata",sep = "/"))){
    dir.create(paste(dos_projet,"Rdata",sep = "/"), showWarnings = F)
  }

  if(!dir.exists(paste(dos_projet,"modeles",sep = "/"))){
    dir.create(paste(dos_projet,"modeles",sep = "/"), showWarnings = F)
  }
  #-------------------installation du package onfR-------------------
  if (!require("onfR")) install.packages(system.file("onfR","onfR_0.9.6.zip", package = "emergeIFNVol"),repos = NULL, type = "win.binary")
  library(onfR)
  if(!require("IFNtraitement")) devtools::install_github("antoine25C/IFNtraitement")
  library(IFNtraitement)

  #----------------chargement des Rdata nécessaires-----------
  code_ess <-emergeIFNVol::code_ess
  codeser <- emergeIFNVol::code_ser
  Ser_shape <- emergeIFNVol::Ser_shape
  dep <- emergeIFNVol::dep
  dep_choix <- tk_select.list(as.vector(dep[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les départements dont vous souhaiter extraire les données IFN")
  dep_choix <-left_join(data.frame(NomDep = dep_choix), dep[,c('NumDep','NomDep')], by = 'NomDep')[,'NumDep']
  ser <- emergeIFNVol::ser
  ser_choix <- tk_select.list(as.vector(ser[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les sylvo-éco-régions sur lesquelles vous souhaitez restreindre les données IFN")
  ser_choix <-as.character(left_join(data.frame(NomSER = ser_choix), ser, by = 'NomSER')[,'codeser'])
  an_debut <- tk_select.list(2005:2017, preselect = NULL, multiple = F,
                         title = "choix de la première année dont les données IFN seront considérées")
  an_fin <- tk_select.list(an_debut:2017, preselect = NULL, multiple = F,
                             title = "choix de la dernière année dont les données IFN seront considérées")
  IFN <- import_IFN(choix_dept = dep_choix, choix_ser = ser_choix, annees = an_debut:an_fin)
  IFN <- classif_cat_Diam(IFN)
  IFN_vol <- BD_vol(IFN, code_ess)
  resultat <-Nb_arbre_hdec_bois_fort(IFN_vol, code_ess, nb_arbre_min = 270)
  res_modelisation <- modelisation(IFN_vol, resultat$tbl_corresp)
  modele_dir <- str_c(dos_projet,"/modeles/NumDep_",str_c(dep_choix, collapse = "_"),"_NomSER_",str_c(ser_choix, collapse = "_"),collapse = "")
  dir.create(modele_dir)
  saveRDS(res_modelisation, str_c(modele_dir,"modele.rds",sep= "/"))
  dir.create(str_c(modele_dir,"graphs", sep ="/"))
  graph_res_modelisation(dsn = str_c(modele_dir,"graphs/",sep = "/",collapse = ""), res_modelisation)
  return(modele_dir)
}
