#'Calcul un volume à partir des tarifs Emerge en estimant la hauteur de décrochement
#'
#'Cette fonction permet de calculer un volume en utilisant les tarifs Emerge mais
#'en ne rensignant "que" l'essence, le diamètre à hauteur de poitrine
#'et la hauteur d'un ou plusieurs arbres. La hauteur de décrochement est
#'prédite en utilisant un modèle de prédiction établit à partir des
#'données des placettes de l'inventaire foretier national
#'
#' @param Id_arbre identifiant unique de l'arbre (numerique ou chaine de caractère)
#' @param ess code de l'essence selon code ONF (voir colonne Cod_ess de la base de données emergeIFNVol::Code_ess_Emerge)
#' @param d13 diamètre à hauteur de poitrine de l'arbre
#' @param htot hauteur total de l'arbre
#' @param X coordonnée X de l'arbre (permet de lui affecter une sylvo-éco-région d'appartenance, ATTENTION au système de coordonnée de référence = crs)
#' @param Y coordonnée Y de l'arbre (permet de lui affecter une sylvo-éco-région d'appartenance, ATTENTION au système de coordonnée de référence = crs)
#' @param crs système de coordonnée de référence rattaché aux coordonnées X et Y renseigné précédement (par défaut : Lambert 93 code EPSG = 2154)
#'
#' @return
#' @export
#' @import sf dplyr stats
#'
#' @examples pas d'exemple disponible actuellement
Calc_Vol_Emerge <- function(Id_arbre,ess,d13, htot, X=NULL, Y=NULL, crs = NA ){
  #-------------------test présence données nécessaire -----------
  if (is.null(X) || is.null(Y)){
    warning("vous n'avez pas précisé de coordonnées X/Y complètes permettant de positionner les arbres\net leur attribuer une sylvo-éco-région (ser) de référence,\npar défaut seul les modèles tout ser confondus seront appliqués")
  }
  if (is.na(crs) & !is.null(coords)){
    crs <- 2154
    warning("Le système de coordonées de référence des points n'ayant pas été renseigné,\n
            par défaut le code EPSG '2154' du Lambert 93 à été utilisé")
  }
  if(!is.numeric(crs)){
    warning("Le système de coordonées de réference renseigné n'est pas numerique!\n
            Pour le champs crs un code EPSG numérique est attendu.")
    stop()
  }
  #-------------------choix du dossier de travail-----
  dir <-choose.dir(caption = "choisir le dossier où seront enregistrer toutes les données")
  name <- svDialogs::dlg_input(message = "entrez le nom du dossier qui sera creer pour stocker toutes les donnees de ce projet")$res
  dos_projet <- paste(dir, name, sep = "/")
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
  #----------------chargement des Rdata nécessaires-----------
  code_ess <-emergeIFNVol::code_ess
  codeser <- emergeIFNVol::code_ser
  Ser_shape <- emergeIFNVol::Ser_shape
  modele <- try(readRDS(choose.files(default = paste(dos_projet,"modeles",sep = "/"), caption = "Veuillez selectionner l'archive .rds contenant les données des modèles de prédiction de la hauteur de décrochement",
                                 multi = F, filters = ".rds")), silent = T)
  modelis <- "no"
  while((class(modele) == "try-error") & (modelis == "no")){
    modelis <- svDialogs::dlg_message(message = "Aucune archives Rdata selectionnee, voulez-vous effectuer les modelisations ?", type = "yesno")$res
    if(modelis == "yes"){
      modele_dir <- modeles_hdec_IFN(dos_projet = dos_projet)
      modele <- readRDS(paste(modele_dir,"modele.rds",sep= "/"))
    } else {
      modele <- try(readRDS(choose.files(default = paste(dos_projet,"modeles",sep = "/"), caption = "Veuillez selectionner l'archive .rds contenant les données des modèles de prédiction de la hauteur de décrochement",
                                         multi = F, filters = ".rds")), silent = T)
    }
  }
  #base de donnée initial
  BD <- data.frame(ida = Id_arbre, code = ess, d13=d13, c13 = d13*pi, htot = htot, X = X, Y = Y)
  BD$X[is.na(BD$X)] <- 0
  BD$Y[is.na(BD$Y)] <- 0
  BD <- st_as_sf(BD, coords = c('X','Y'), crs = crs)
  if (crs != 2154){
    BD <- st_transform(BD, crs = 2154)
  }
  Ser_shape <- st_crop(Ser_shape, BD)
  intersect <- sf::st_intersects(BD,Ser_shape)
  BD$row.id <-as.numeric(rownames(BD))
  intersect <-as.data.frame(intersect)
  BD <- BD %>%
    left_join(intersect , by = 'row.id') %>%
    dplyr::select(-row.id)
  colnames(BD)[ncol(BD)-1] <- "Row_ser"
  Ser_shape <- as.data.frame(Ser_shape)
  BD$ser <- Ser_shape[BD$Row_ser,1]
  BD <- as.data.frame(BD) %>%
    dplyr::select(-"Row_ser", - "geometry") %>%
    mutate(code = as.character(code))

  BD <- BD %>%
    left_join(code_ess[,c('code','espar','fam')], by = 'code') %>%
    mutate(decroi = (c13/pi)/(htot),
           ser = as.character(ser),
           fam = as.character(fam))
  if(length(BD$fam[is.na(BD$fam)])>0){
    BD$fam[is.na(BD$fam)]<-left_join(data.frame(code = BD[is.na(BD$fam),colnames(BD) == "code"]),
                                     code_ess_Emerge[colnames(code_ess_Emerge) %in% c("Cod_ess","fam")],
                                     by = c('code'='Cod_ess'))[,2]
  }

  BD$ser[!(BD$ser %in% unique(names(modele)))] <- "tt_ser"
  BD <- BD %>%
    filter(!is.na(code))
  BD <- predict_hdec(modele, BD)
  BD$hdec[!is.na(BD$hdec) & BD$hdec > BD$htot] <- BD$htot[!is.na(BD$hdec) & BD$hdec > BD$htot]
  BD <- BD %>%
    left_join(code_ess_Emerge[colnames(code_ess_Emerge)%in% c("Cod_ess","nomess")], by= c('code'='Cod_ess'))
  BD$htot[BD$htot<0] <-0
  BD$hdec[BD$hdec<0] <-0
  BD$v <- NA
  Fun_TarEmerge <- function(x){
    res <- NA
    if(!is.na(x['espar'])){
      res <- onfR::TarEmerge(c130 = as.numeric(x['c13']), htot =  as.numeric(x['htot']), hdec =  as.numeric(x['hdec']), dec =7, espar =  x['espar'], typevol = "total")
    }
    if (is.na(res) & !is.na(x['code'])){
      res <- onfR::TarEmerge(c130 = as.numeric(x['c13']), htot =  as.numeric(x['htot']), hdec =  as.numeric(x['hdec']), dec =7, ess =  x['code'], typevol = "total")
    }
    if (is.na(res) & !is.na(x['nomess'])){
      res <- onfR::TarEmerge(c130 = as.numeric(x['c13']), htot =  as.numeric(x['htot']), hdec =  as.numeric(x['hdec']), dec =7, nomess =  x['nomess'], typevol = "total")
    }
    return(res)
  }
  BD$v<- apply(BD,1, FUN = Fun_TarEmerge)
  BD <- BD %>%
    dplyr::select(-c13, -espar, -nomess, -decroi)
  return(BD)
}


