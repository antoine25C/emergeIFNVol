#' Prépare une base de données de complétes d'arbres avec une hauteur de décrochement
#' à partir des données brutes de l'IFN
#'
#' Cette fonction prépare une base de donnée issue des données de l'inventaire forestier
#' qui permetra de calibrer un modèle de prédiction de la hauteur de décrochement à partir
#' de l'essence, le diamètre, la hauteur total des arbres et leur sylvo-éco-région d'appartenance.
#' Elle filtre donc la base de donnée initial pour avoir une base de données complète
#' (i.e. avec pour chaque arbres les 5 données citées précédement)
#'
#' @param IFN_data Base de données de l'inventaire forestier national : liste
#' comprenant les base de données : arbres (IFNarbres), placettes (IFNplacettes)
#' et écologie (IFNecologie). Correspond à la base de donnée issue des données brute IFN
#' et que l'on peut obtenir à partir de la fonction import_IFN du package IFNtraitement
#' @param code_ess base de donnée de correspondance entre les codes essences de l'IFN, de l'ONF
#' et une classification entre familles d'essence (F : feuillus, R : résineux)
#'
#' @return retourne la base de données INF_data rentrée initialement filtrée pour que
#' la base de donnée arbre (IFNarbres) ne contiennent que des arbres dont on connait à la fois :
#' l'essence, le diamètre à 1,30m, la hauteur totale, la hauteur de décrochement et la sylvo-ecor-région d'appartenence
#' @export
#'
#' @import dplyr rlist
#'
#' @examples pas d'exemple disponible actuellement
BD_vol <- function(IFN_data, code_ess){
  IFNarbres <- list()
  IFNarbres_ser <- data.frame()
  for (i in 1:length(IFN_data$IFNarbres)){
    IFNarbres_ser <-IFN_data$IFNarbres[[i]]
    IFNarbres_ser <- IFNarbres_ser %>%
      left_join(code_ess[,c("espar","code","fam")], by='espar') %>%
      dplyr::filter(!is.na(htot),
             !is.na(hdec),
             grd_catD != "Perche",
             Annee != 2014,
             hdec <= htot,
             !is.na(fam),
             !is.na(code))%>%
      mutate(ida = paste(idp, a, sep = "_"),
             htot = htot/10,
             hdec = hdec/10,
             rapport_H = hdec/htot,
             decroi = (c13/pi)/(htot),
             diff_h_hdec = htot-hdec,
             code = as.factor(code),
             grd_catD  = as.factor(grd_catD),
             fam = as.factor(fam),
             decoupe = as.factor(decoupe))%>%
      dplyr::select(ida, code, espar, hdec, c13,htot, decroi, grd_catD,fam,decoupe, ser,v)
    IFNarbres <- list.append(IFNarbres, IFNarbres_ser)
    names(IFNarbres)[[i]] <- names(IFN_data$IFNarbres)[[i]]
  }
  IFN_vol <- list(IFN_data$IFNplacettes, IFNarbres, IFN_data$IFNecologie)
  names(IFN_vol) <- names(IFN_data)[1:3]
  return(IFN_vol)
}

#' Détermine par couple essence/sylvo-éco-région si une modélisation spécifique au couple est possible
#' (nombre de données disponible > seuil minimal d'abres)
#'
#' A partir de la base de données de l'inventaire forestier national filtrée par la fonction
#' emergeIFNVol::BD_Vol(...), cette fonction prépare la modélisation de la hauteur de décrochement
#' en retournant un tableau de correspondance définissant si pour une essence et une
#' sylvo-éco-région ( = SER) donnée on possède un nombre suffisant de données (d'arbres) pour effectuer
#' une modélisation spécifique à ce couple essence/SER. Si ce n'est pas le cas on spécifie si on effectuera
#' une modélisation spécifique à la SER en distinguant feuillus et résineux ou si on effectuera une
#' modélisation tout SER confondu en distinguant feuillus et résineux
#'
#' @param IFN_vol Base de données de l'inventaire forestier national préalablement filtrée et
#' issue de la fonction emergeIFNVol::BD_Vol(...) (voir l'aide spécifique à cette fonction pour plus d'informations)
#' @param code_ess base de donnée de correspondance entre les codes essences de l'IFN, de l'ONF
#' et une classification entre familles d'essence (F : feuillus, R : résineux)
#' @param nb_arbre_min nombre d'arbre minimum nécessaire à une modélisation de la hauteur de décrochement
#' spécifique à un couple essence/sylvo-éco-région
#'
#' @return tableau de correspondance définissant si pour une essence et une
#' sylvo-éco-région ( = SER) donnée on possède un nombre suffisant de données (d'arbres) pour effectuer
#' une modélisation spécifique à ce couple essence/SER. ou à défaut si on effectue une modélisation différenciant
#' feuillus et résineux par sylvo-éco-région (=SER) ou toutes SER confondues
#' @export
#'
#' @import dplyr rlist
#'
#' @examples pas d'exemple disponible actuellement
Nb_arbre_hdec_bois_fort <- function(IFN_vol, code_ess, nb_arbre_min = 200){
  ess <- character()
  list_prop <- list()
  for (i in 1:length(IFN_vol$IFNarbres)){
    IFN_vol$IFNarbres[[i]]$code <- as.character(IFN_vol$IFNarbres[[i]]$code)
    IFN_vol$IFNarbres[[i]]$code[IFN_vol$IFNarbres[[i]]$code %in% c("CHP", "CHS", "CHX")] <- "CHE"
    IFN_vol$IFNarbres[[i]]$code <- as.factor(IFN_vol$IFNarbres[[i]]$code)
    decoupe_0_2 <- IFN_vol$IFNarbres[[i]] %>%
      dplyr::filter(decoupe %in% c(0,2)) %>%
      group_by(code) %>%
      summarise(nb_bois_fort = n())
    decoupe_0_2$code <- as.character(decoupe_0_2$code)
    tt_arbres<- IFN_vol$IFNarbres[[i]] %>%
      group_by(code) %>%
      summarise(nb_arbre = n())%>%
      mutate(code = as.character(code)) %>%
      left_join(decoupe_0_2, by = 'code')
    tt_arbres$nb_arbre[is.na(tt_arbres$nb_arbre)]<-0
    tt_arbres$nb_bois_fort[is.na(tt_arbres$nb_bois_fort)] <- 0
    tt_arbres$prop <- 0
    tt_arbres$prop[tt_arbres$nb_arbre > 0] =(tt_arbres$nb_bois_fort/tt_arbres$nb_arbre)*100
    ess <- append(ess, as.character(unique(IFN_vol$IFNarbres[[i]]$code)))
    colnames(tt_arbres)[2:4] <- paste(str_remove(names(IFN_vol$IFNarbres)[[i]], pattern="IFNarbres_"),colnames(tt_arbres)[2:4],sep="_")
    list_prop <- list.append(list_prop, tt_arbres)
    names(list_prop)[[i]] <- names(IFN_vol$IFNarbres)[[i]]
  }
  ess <- unique(ess)
  res_ess <- data.frame(code = ess)
  res_ess$code <- as.character(res_ess$code)
  for (i in 1: length(list_prop)){
    res_ess <- res_ess %>%
      mutate(code = as.character(code)) %>%
      left_join(list_prop[[i]], by = 'code')
  }
  for (col in 2:dim(res_ess)[2]){
    res_ess[,col][is.na(res_ess[,col])] <- 0
  }
  res_ess$nb_arbre_tot <- rowSums(as.data.frame(res_ess[,str_detect(colnames(res_ess), pattern = "nb_arbre")]))
  res_ess$nb_bois_fort_tot <- rowSums(as.data.frame(res_ess[,str_detect(colnames(res_ess), pattern = "nb_bois_fort")]))
  res_ess$prop_tot <- (res_ess$nb_bois_fort_tot/res_ess$nb_arbre_tot)*100

  list_prop <-list.append(list_prop, res_ess[,c(1, (dim(res_ess)[2]-2):dim(res_ess)[2])])
  names(list_prop)[length(list_prop)]<-"tt_ser"
  res_rgrpt <- list()
  tbl_corresp <- data.frame(code = ess)%>%
    mutate(code = as.character(code)) %>%
    left_join(code_ess[,c('code', 'fam')], by ='code')
  tbl_corresp <- unique(tbl_corresp)
  tbl_corresp_ser <-data.frame()
  for (i in 1: length(list_prop)){
    res <-list_prop[[i]] %>%
      mutate(code = as.character(code)) %>%
      left_join(code_ess[, c('code','fam')], by='code')
    nom_ser <- str_remove(names(list_prop)[[i]], pattern= "IFNarbres_")
    res$code_rgrpt <- ""
    res$code_rgrpt[res[,3]>=nb_arbre_min] <-res$code[res[,3]>=nb_arbre_min]
    res$code_rgrpt[res[,3]<nb_arbre_min] <-paste("A",res$fam[res[,3]< nb_arbre_min], sep="_")
    res$code_rgrpt[res[,1] == "A.R"] <- "A_R"
    res$code_rgrpt[res[,1] == "A.F"] <- "A_F"
    tbl_corresp_ser <- data.frame(code = res$code, code_model = res$code_rgrpt)
    tbl_corresp_ser$code_model <-as.character(tbl_corresp_ser$code_model)
    res <- res %>%
      dplyr::select(-c(code,fam)) %>%
      group_by(code_rgrpt) %>%
      summarise_all(sum)
    res[,4] <- (res[,3]/res[,2])*100

    if("A_F" %in% unique(res$code_rgrpt) & (res[res$code_rgrpt == "A_F",3] < nb_arbre_min)) {
      tbl_corresp_ser$code_model[tbl_corresp_ser$code_model == "A_F"] <-"F"
    }
    if("A_R" %in% unique(res$code_rgrpt) & (res[res$code_rgrpt == "A_R",3] < nb_arbre_min)) {
      tbl_corresp_ser$code_model[tbl_corresp_ser$code_model == "A_R"] <- "R"
    }
    colnames(res)[1] <- "code"

    res_fam <-list_prop[[i]] %>%
      mutate(code = as.character(code)) %>%
      left_join(code_ess[, c('code','fam')], by='code')%>%
      dplyr::select(-code) %>%
      group_by(fam) %>%
      summarise_all(sum)
    res[,4] <- (res[,3]/res[,2])*100
    if(res_fam[res_fam$fam == "F",3]< nb_arbre_min) {
      tbl_corresp_ser$code_model[tbl_corresp_ser$code_model == "F"] <- "tt_ser"
    }
    if(res_fam[res_fam$fam == "R",3] < nb_arbre_min) {
      tbl_corresp_ser$code_model[tbl_corresp_ser$code_model == "R"] <- "tt_ser"
    }
    colnames(res_fam)[1] <- "code"
    res <- rbind(res, res_fam)
    res_rgrpt <- list.append(res_rgrpt, res)
    names(res_rgrpt)[[i]] <- nom_ser
    tbl_corresp_ser$code <- as.character(tbl_corresp_ser$code)

    tbl_corresp <-tbl_corresp %>%
      mutate(code = as.character(code)) %>%
      left_join(tbl_corresp_ser, by = 'code')
    tbl_corresp[,i+2][is.na(tbl_corresp[,i+2])] <- "tt_ser"
    tbl_corresp <- unique(tbl_corresp)
    colnames(tbl_corresp)[dim(tbl_corresp)[2]] <- nom_ser
  }

  resultat  <- list(res_ess,res_rgrpt, tbl_corresp)
  names(resultat) <- c("res_ess","res_rgrpt", "tbl_corresp")
  return(resultat)
}

#' Calibration de modèles de prédiction de la hauteur de décrochement d'arbres à partir de
#' la base de donnée de l'inventaire forestier nationale (=IFN)
#'
#' @param IFN_data Base de données de l'inventaire forestier national préalablement filtrée et
#' issue de la fonction emergeIFNVol::BD_Vol(...) (voir l'aide spécifique à cette fonction pour plus d'informations)
#' @param tbl_corresp_model tableau de correspondance définissant si pour une essence et une
#' sylvo-éco-région (=SER) donnée on effectuera une modélisation spécifique à ce couple essence/SER
#' tableau de correspondance issue de la fonction emergeIFNVol::Nb_arbre_hdec_bois_fort(...) (voir aide spécifique à cette fonction pour plus d'informations)
#'
#' @return renvoie une liste comportant une sous-liste par sylvo-écorégion (SER)
#' et une sous-liste toutes SER confondues. ces sous listes comportant autant de sous-liste que
#' de modèles spécifique à une essence + deux sous listes correspondant
#' aux  modèles résineux et feuillus. Les listes par modèles comportent elles :
#' la base de données arbres ayant servis à la modélisation, la partie de cette base de données ayant servie
#' à la calbration, celles ayant servie à la validation du modèle, le modèle en lui-même et
#' les graphiques d'analyse du modèle
#' @export
#'
#' @import dplyr rlist stats ggplot2
#'
#' @examples pas d'exemple disponible actuellement
modelisation <- function(IFN_data, tbl_corresp_model){
  list_data <- list()
  IFN_data$IFNarbres$tot <- concatenation_BD_arbres(IFN_data)
  IFN_data$IFNarbres$tot$ser <-"tt_ser"
  for (arbres in IFN_data$IFNarbres){
    nom_ser <- unique(arbres$ser)
    arbres <- arbres %>%
      mutate(code = as.character(code)) %>%
      left_join(tbl_corresp_model[,c('code', nom_ser)], by = 'code')
    arbres[,nom_ser][is.na(arbres[,nom_ser])] <-"tt_ser"
    list_data_ser <- list()
    for (code_model in unique(arbres[,nom_ser])[unique(arbres[,nom_ser]) != "tt_ser"]){
      if(!(code_model %in% c('F', 'R'))){
        BD <- arbres[arbres[, nom_ser] == code_model,] %>%
          dplyr::filter(decoupe %in% c(0,2)) %>%
          dplyr::select(ida,hdec, htot, c13, decroi, espar)
      } else {
        BD <- arbres[arbres[, 'fam'] == code_model,] %>%
          dplyr::filter(decoupe %in% c(0,2)) %>%
          dplyr::select(ida,hdec, htot, c13, decroi, espar)
      }

      BD_calib <-sample_frac(BD, 0.75)
      BD_valid <-anti_join(BD, BD_calib, by = 'ida')

      modele_lin <- step(lm(hdec ~., data = BD_calib[,2:5]), direction = "both")
      print(str_c(c("modèle linéaire pour la sylvo-éco-région",nom_ser, "et l'/les essence(s) modélisée(s) ", code_model),collapse =" "))
      print(summary(modele_lin))

      BD_calib$hdec_pred <- predict(modele_lin,BD_calib)
      BD_calib$err<- BD_calib$hdec - BD_calib$hdec_pred

      BD_valid$hdec_pred <- predict(modele_lin,BD_valid)
      BD_valid$err<- BD_valid$hdec - BD_valid$hdec_pred

      calc_Vols_Emerge <- function(BD){
        BD$v <- 0
        BD$v_pred <- 0
        for (i in 1:nrow(BD)){
          BD[i,]$v <- onfR::TarEmerge(c130 = BD[i,]$c13, htot = BD[i,]$htot, hdec = BD[i,]$hdec, dec =7, espar = BD[i,]$espar, typevol = "total")
          BD[i,]$v_pred <- onfR::TarEmerge(c130= BD[i,]$c13, htot = BD[i,]$htot,hdec = BD[i,]$hdec_pred, dec =7, espar = BD[i,]$espar, typevol = "total")
        }
        return(BD)
      }
      BD_calib <- calc_Vols_Emerge(BD_calib)
      BD_valid <- calc_Vols_Emerge(BD_valid)
      RMSE = function(ref, predict){
        df = data.frame(ref = ref,predict =predict)
        df <- df %>%
          dplyr::filter(!is.na(ref),
                 !is.na(predict))
        return(sqrt(mean((df$predict - df$ref)^2)))
      }
      rmse_calib <- round(RMSE(BD_calib$v, BD_calib$v_pred),2)
      rmse_valid <- round(RMSE(BD_valid$v, BD_valid$v_pred),2)


      plot_result_pred_calid =ggplot(BD_calib, aes(x = hdec, y = hdec_pred))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color = 'red')+
        scale_x_continuous(limits=c(0,50))+
        scale_y_continuous(limits=c(0,50))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("hdec prédit par régression linéaire pour les données de calibration\n du modèle en fonction du hdec déterminé sur le terrain par l'IFN", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))
      plot_result_pred_valid =ggplot(BD_valid, aes(x = hdec, y = hdec_pred))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color = 'red')+
        scale_x_continuous(limits=c(0,50))+
        scale_y_continuous(limits=c(0,50))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("hdec prédit par régression linéaire pour les données de validation\n du modèle en fonction du hdec déterminé sur le terrain par l'IFN", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))

      plot_err_pred_calib = ggplot(BD_calib, aes(x = htot, y = err))+
        geom_point()+
        geom_abline(intercept = 0, slope = 0, color = 'red')+
        scale_x_continuous(limits=c(0,50))+
        scale_y_continuous(limits=c(-30,30))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("erreurs de prédiction  pour les données d'entrainement du modèle", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))
      plot_err_pred_valid = ggplot(BD_valid, aes(x = htot, y = err))+
        geom_point()+
        geom_abline(intercept = 0, slope = 0, color = 'red')+
        scale_x_continuous(limits=c(0,50))+
        scale_y_continuous(limits=c(-30,30))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("erreurs de prédiction pour les données de validation du modèle", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))

      plot_Vol_Emerge_calid =ggplot(BD_calib, aes(x = v, y = v_pred))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color = 'red')+
        # scale_x_continuous(limits=c(0,15))+
        # scale_y_continuous(limits=c(0,15))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("Volume calculé à partir du hdec prédit par régression linéaire\n pour les données de calibration du modèle en fonction\n du volume calculé à partir du hdec déterminé sur le terrain par l'IFN", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model,"     RMSE = ", rmse_calib),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))
      plot_Vol_Emerge_valid =ggplot(BD_valid, aes(x = v, y = v_pred))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color = 'red')+
        #scale_x_continuous(limits=c(0,15))+
        #scale_y_continuous(limits=c(0,15))+
        geom_smooth(method = "lm", color = 'green')+
        ggtitle("Volume calculé à partir du hdec prédit par régression linéaire\n pour les données de calibration du modèle en fonction\n du volume calculé à partir du hdec déterminé sur le terrain par l'IFN", subtitle = str_c(c("sylvo-éco-région :",nom_ser, "essence(s) modélisé(s) :", code_model,"     RMSE = ", rmse_valid),collapse =" "))+
        theme(plot.title=element_text(hjust=0.5))

      BD <- arbres[arbres[, nom_ser] == code_model,]
      BD$hdec_pred <- predict(modele_lin,BD)
      BD$err<- BD$hdec - BD$hdec_pred
      plots <- list(plot_result_pred_calid,plot_result_pred_valid, plot_err_pred_calib,plot_err_pred_valid,plot_Vol_Emerge_calid, plot_Vol_Emerge_valid)
      names(plots) <- c('graph_result_pred_calid','graph_result_pred_valid', 'graph_err_pred_calib','graph_err_pred_valid', 'graph_Vol_Emerge_calib','graph_Vol_Emerge_valid')
      list_model <- list(BD, BD_calib, BD_valid, modele_lin, plots)
      names(list_model) <- c('BD', 'BD_calib', 'BD_valid', 'modele_lin', 'graphs')
      list_data_ser <- list.append(list_data_ser, list_model)
      names(list_data_ser)[length(list_data_ser)] <- code_model
    }
    list_data <- list.append(list_data,list_data_ser)
    names(list_data)[length(list_data)] <- nom_ser
  }
  return(list_data)
}


#' Prédiction de la hauteur de décrochement d'arbres selon leur essence, diamètre, hauteur total et sylvo-éco-région
#'
#' Cette fonction à partir d'une base de données arbres pour lesquels sont spécifié l'essence,
#' le diamètre à 1,30m, la hauteur total et la sylvo-éco-région d'appartenance prédit
#' la hauteur de décrocement de ces arbres à partir des modèles de prédictions issue de la fonction emergeIFNVol::modelisation(...)
#'
#' @param res_modelisation liste de résultat issue de la fonction emergeIFNVol::modelisation(...) (voir aide spécifique de cette fonction)
#' @param df data.frame des données par arbres et pour lesquels on veut prédir
#' la hauteur de décrochement. Ce data.frame doit comporter pour chaque arbre
#' les données suivantes :ida, ser, code, fam, htot, c13
#'
#' @return renvoir le data.frame df initial auquel s'ajoute la colonne hdec correspondant à a hauteur de décrochement prédite
#' @export
#'
#' @import dplyr stats
#'
#' @examples pas d'exemple disponible actuellement
predict_hdec <- function(res_modelisation, df){
  if (!all(c('ida','ser','code','fam','htot','c13')%in% colnames(df))){
    warning("le data.frame df donné en entré de la fonction ne comporte pas l'ensemble des données suivantes :
            ida, ser, code, fam, htot, c13
            Ou bien le nom de ces colonnes n'est pas écrit commme le demande la fonction")
    stop()
  }
  df$code_ini <-df$code
  df$code[df$code %in% c("CHP", "CHS", "CHX")] <- "CHE"
  df$hdec <- NA
  df$decroi <- (df$c13/pi)/(df$htot)
  SER <- names(res_modelisation)[names(res_modelisation) != "tt_ser"]
  for (ser in SER){
    Ess <- names(res_modelisation[[ser]])[!(names(res_modelisation[[ser]]) %in% c("A_F","A_R", "F", "R"))]
    for (ess in Ess){
      df$hdec[(df$ser == ser & df$code == ess)] <- predict(res_modelisation[[ser]][[ess]]$modele_lin,
                                                           df[df$ser == ser & df$code == ess,])
    }
    if("A_F" %in% names(res_modelisation[[ser]])){
      df$hdec[df$ser == ser & !(df$code %in% Ess) & df$fam == "F"] <- predict(res_modelisation[[ser]][["A_F"]]$modele_lin,
                                                                              df[df$ser == ser & !(df$code %in% Ess) & df$fam == "F",])
    } else if ("F" %in% names(res_modelisation[[ser]])){
      df$hdec[df$ser == ser & !(df$code %in% Ess) & df$fam == "F"] <- predict(res_modelisation[[ser]][["F"]]$modele_lin,
                                                                              df[df$ser == ser & !(df$code %in% Ess) & df$fam == "F",])
    }
    if("A_R" %in% names(res_modelisation[[ser]])){
      df$hdec[df$ser == ser & !(df$code %in% Ess) & df$fam == "R"] <- predict(res_modelisation[[ser]][["A_R"]]$modele_lin,
                                                                              df[df$ser == ser & !(df$code %in% Ess) & df$fam == "R",])
    } else if ("R" %in% names(res_modelisation[[ser]])){
      df$hdec[df$ser == ser & !(df$code %in% Ess) & df$fam == "R"] <- predict(res_modelisation[[ser]][["R"]]$modele_lin,
                                                                              df[df$ser == ser & !(df$code %in% Ess) & df$fam == "R",])
    }
  }
  Ess <- names(res_modelisation[["tt_ser"]])[!(names(res_modelisation[["tt_ser"]]) %in% c("A_F","A_R", "F", "R"))]
  for (ess in Ess){
    #if(!is.null(df[is.na(df$hdec) & df$ser %in% SER & df$code == ess,]))
    df$hdec[is.na(df$hdec) & df$code == ess] <- predict(res_modelisation[["tt_ser"]][[ess]]$modele_lin,
                                                                          df[is.na(df$hdec) & df$code == ess,])
  }
  if("A_F" %in% names(res_modelisation[["tt_ser"]])){
    df$hdec[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "F"] <- predict(res_modelisation[["tt_ser"]][["A_F"]]$modele_lin,
                                                                                               df[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "F",])
  } else if ("F" %in% names(res_modelisation[["tt_ser"]])){
    df$hdec[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "F"] <- predict(res_modelisation[["tt_ser"]][["F"]]$modele_lin,
                                                                                               df[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "F",])
  }
  if("A_R" %in% names(res_modelisation[["tt_ser"]])){
    df$hdec[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "R"] <- predict(res_modelisation[["tt_ser"]][["A_R"]]$modele_lin,
                                                                                               df[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "R",])
  } else if ("R" %in% names(res_modelisation[["tt_ser"]])){
    df$hdec[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "R"] <- predict(res_modelisation[["tt_ser"]][["R"]]$modele_lin,
                                                                                               df[is.na(df$hdec) & !(df$code %in% Ess) & df$fam == "R",])
  }
  df <-df %>%
    mutate(code = code_ini) %>%
    dplyr::select(-code_ini)
  return(df)
}
