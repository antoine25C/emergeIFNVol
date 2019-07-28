#' plot_don_Vol réalise des graphiques d'analyse de la base de données arbre de l'IFN
#'
#' Cette fonction renvoie des graphiques d'analyse de la base de données arbre de
#' l'inventaire forestier national à partir des variables x et y spécifiées en tant
#' que paramètres d'entrée de la fonction
#'
#' @param IFNarbres Base de données arbre de l'IFN issue des données brute IFN
#' et que l'on peut obtenir à partir de la fonction import_IFN du package IFNtraitement
#' @param file_path chemin complet de l'image qui sera enregistrer en sortie de la fonction
#' (ex : "c:/dos1/dos2/graph1.png")
#' @param x nom de la colonne d'IFNarbres qui sera représentée en abscisse des graphiques
#' @param y nom de la colonne d'IFNarbres qui sera représentée en ordonnée des graphiques
#' @param color couleur des points des graphiques
#' @param shape formes des "points" des graphiques
#'
#' @return Cette fonction ne retourne rien mais enregistre des fichiers images
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples pas d'exemple disponible actuellement
plot_don_Vol <- function (IFNarbres, file_path ="", x = NULL, y = NULL, color = NULL, shape = NULL){
  IFNarbres$grd_catD <- factor(IFNarbres$grd_catD, levels = c("PB", "BM", "GB", "TGB"))

  IFNarbres_EPC <- IFNarbres %>%
    filter(code_rgrp == "EPC")

  IFNarbres_SAP <- IFNarbres %>%
    filter(code_rgrp == "SAP")

  IFNarbres_HET <- IFNarbres %>%
    filter(code_rgrp == "HET")

  IFNarbres_FRE <- IFNarbres %>%
    filter(code_rgrp == "FRE")

  IFNarbres_ERA <- IFNarbres %>%
    filter(code_rgrp == "ERA")

  IFNarbres_A_F <- IFNarbres %>%
    filter(!code_rgrp %in% c("HET", "FRE", "ERA"),
           fam == "F")

  IFNarbres_A_R <- IFNarbres %>%
    filter(!code_rgrp %in% c("EPC", "SAP"),
           fam == "R")
  IFNarbres_res <- IFNarbres %>%
    filter(fam == "R")

  IFNarbres_feu <- IFNarbres %>%
    filter(fam == "F")


  plot1 = ggplot(IFNarbres, aes(x = x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "toutes essences confondues")


  plot2 = ggplot(IFNarbres_res, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour les essences résineuses")



  plot3 = ggplot(IFNarbres_feu, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour les essences feuillus")



  plot4 = ggplot(IFNarbres_EPC, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour l'épicéa")



  plot5 = ggplot(IFNarbres_SAP, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour le sapin")



  plot6 = ggplot(IFNarbres_A_R, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour les essences résineuses (hors épicéa et sapin)")



  plot7 = ggplot(IFNarbres_HET, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour le hêtre")



  plot8 = ggplot(IFNarbres_FRE, aes(x =  x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour le frêne")



  plot9 = ggplot(IFNarbres_ERA, aes(x =  x, y = y, color =color))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour les érables")



  plot10 = ggplot(IFNarbres_A_F, aes(x = x, y = y,  color=color, shape = shape))+
    geom_point()+
    xlab("hauteur totale de l'arbre")+
    scale_x_continuous(breaks = seq(0,50,5),limits = c(0,50), labels = paste(seq(0,50,5),"m"))+
    ylab("hauteur de décrochement ")+
    scale_y_continuous(breaks = seq(0,50,5),limits = c(0,50),labels = paste(seq(0,50,5),"m"))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position ="bottom")+
    ggtitle("hauteur de décrochement en fonction de la hauteur totale", subtitle = "pour les essences feuillus (hors hêtre, frêne et érables")


  plots <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10)

  multiplot(plots,
            width=2200,
            height=1200,
            file=file_path,
            layout=matrix(c(1,0,0,0,0,2,4,5,6,0,3,7,8,9,10), nrow=3, byrow=TRUE))

}

#' graphiques d'analyse des modélisation de la hauteur de décrochement
#'
#' Cette fonction enregistre, pour chaque modèles issues de la fonction emergeIFNVol::modelisation(...),
#' les graphiques d'analyse de ces modèles avec une image par modèle
#'
#'
#' @param dsn chemin complet vers le dossier du projet comportant les sous-dossiers
#' "modeles/NumDep_..._NomSER_.../graphs"
#' @param res_modelisation liste de résultat issue de la fonction emergeIFNVol::modelisation(...) (voir aide spécifique de cette fonction)
#'
#' @return Cette fonction ne retourne rien mais enregistre des fichiers images
#' @export
#'
#' @import png grid
#'
#' @examples pas d'exemple disponible actuellement
graph_res_modelisation <- function(dsn = getwd(), res_modelisation){
  for (ser in names(res_modelisation)){
    for(code_model in names(res_modelisation[[ser]])){
      multiplot(res_modelisation[[ser]][[code_model]]$graphs,
                width=1200,
                height=1200,
                file=str_c(c(dsn,"/graphs_modelisation_",ser,"_",code_model), collapse =""),
                layout=matrix(c(1,2,3,4,5,6), nrow=3, byrow=TRUE))
    }
  }
}

#' fonction assurant la mise en page d'une liste de graphique à enregister sous une unique image
#'
#' @param plots liste des graphiques à enregistrer dans une seule image
#' @param file chemin complet de l'image qui sera enregistrer en sortie
#' de la fonction (ex : "c:/dos1/dos2/graph1.png")
#' @param width largeur de l'image (par défaut = 1000)
#' @param height hauteur de l'image (par défaut = 1000)
#' @param cols nombre de colonnes selon lesquels les graphiques devront être
#' réparties dans l'image final (par défaut vaut 1, donnée non prise en compte
#' si l'argument layout est renseigné)
#' @param layout matrice permettant de spécifier l'agencement des différents graphiques
#' (ex : si on 3 graphiques spécifié dans plots et layout = matrix(c(1,1,3,2), nrow=2, byrow=TRUE)
#' alors l'image final sera composée d'une première ligne avec le premier graphique de plots
#' puis d'une seconde ligne divisée en 2 parties avec à gauche le 3ieme graphique de plots
#' et à droite le 2nd graphiques de plots)
#'
#' @return
#' @export
#'
#' @import png grid
#'
#' @examples pas d'exemple disponible actuellement
multiplot <- function(plots, file=".",width=1000,height=1000, cols=1, layout=NULL) {
  numPlots = length(plots)
  print(numPlots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    png(file = paste0(file,".png"), bg = "transparent",width=width,height=height)

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      if(class(plots[[i]])=="character"){
        # image png
        plots[[i]] <- readPNG(paste0(plots[[i]],".png"))

        grid.raster(plots[[i]],vp=viewport(layout.pos.row = matchidx$row,
                                           layout.pos.col = matchidx$col))

      }else{
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
    dev.off()
  }
}

