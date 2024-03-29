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
#' @import tcltk dplyr stats rlist stringr sf ggplot2 png grid svDialogs xlsx
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
  if(!dir.exists(paste(dos_projet,"IFNdon",sep = "/"))){
    dir.create(paste(dos_projet,"IFNdon",sep = "/"), showWarnings = F)
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
  choix_dept <- tk_select.list(as.vector(dep[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les départements dont vous souhaiter extraire les données IFN")
  choix_dept <-left_join(data.frame(NomDep = choix_dept), dep[,c('NumDep','NomDep')], by = 'NomDep')[,'NumDep']
  ser <- emergeIFNVol::ser
  choix_ser <- tk_select.list(as.vector(ser[,2]), preselect = NULL, multiple = T,
                              title = "Selectionner les sylvo-éco-régions sur lesquelles vous souhaitez restreindre les données IFN")
  choix_ser <-as.character(left_join(data.frame(NomSER = choix_ser), ser, by = 'NomSER')[,'codeser'])
  an_debut <- tk_select.list(2005:2017, preselect = NULL, multiple = F,
                         title = "choix de la première année dont les données IFN seront considérées")
  an_fin <- tk_select.list(an_debut:2017, preselect = NULL, multiple = F,
                             title = "choix de la dernière année dont les données IFN seront considérées")
  IFN <- import_IFN(choix_dept = choix_dept, choix_ser = choix_ser, annees = an_debut:an_fin, split = T, save_dsn = paste(dos_projet,"IFNdon/IFN_data.rds",sep = "/"))
  IFN <- classif_cat_Diam(IFN)
  IFN_vol <- BD_vol(IFN_data = IFN, code_ess = code_ess)
  resultat <-Nb_arbre_hdec_bois_fort(IFN_vol, code_ess, nb_arbre_min = 270)
  res_modelisation <- modelisation(IFN_vol, resultat$tbl_corresp)
  nom_fichier_modele <- svDialogs::dlg_input(message = "entrez le nom de l'archive rds contenant les modéles qui ont été calibrés")$res
  if(!dir.exists(paste(dos_projet,"modeles",nom_fichier_modele,sep = "/"))){
    dir.create(paste(dos_projet,"modeles",nom_fichier_modele,sep = "/"), showWarnings = F)
  }
  saveRDS(res_modelisation, str_c(dos_projet,"modeles",nom_fichier_modele,"modele.rds",sep= "/"))
  if(!dir.exists(paste(dos_projet,"modeles",nom_fichier_modele,"graphs",sep = "/"))){
    dir.create(paste(dos_projet,"modeles",nom_fichier_modele,"graphs",sep = "/"), showWarnings = F)
  }
  graph_res_modelisation(dsn = paste(dos_projet,"modeles",nom_fichier_modele,"graphs",sep = "/"), res_modelisation)
  coefs <- recap_coef_modeles(res_modelisation)
  xlsx::write.xlsx(coefs, paste(dos_projet,"modeles",nom_fichier_modele,"recap_coeffs.xlsx",sep = "/"))
  return(modele_dir)
}

