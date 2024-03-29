% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emergeIFNVol_plot.R
\name{plot_don_Vol}
\alias{plot_don_Vol}
\title{plot_don_Vol réalise des graphiques d'analyse de la base de données arbre de l'IFN}
\usage{
plot_don_Vol(IFNarbres, file_path = "", x = NULL, y = NULL,
  color = NULL, shape = NULL)
}
\arguments{
\item{IFNarbres}{Base de données arbre de l'IFN issue des données brute IFN
et que l'on peut obtenir à partir de la fonction import_IFN du package IFNtraitement}

\item{file_path}{chemin complet de l'image qui sera enregistrer en sortie de la fonction
(ex : "c:/dos1/dos2/graph1.png")}

\item{x}{nom de la colonne d'IFNarbres qui sera représentée en abscisse des graphiques}

\item{y}{nom de la colonne d'IFNarbres qui sera représentée en ordonnée des graphiques}

\item{color}{couleur des points des graphiques}

\item{shape}{formes des "points" des graphiques}
}
\value{
Cette fonction ne retourne rien mais enregistre des fichiers images
}
\description{
Cette fonction renvoie des graphiques d'analyse de la base de données arbre de
l'inventaire forestier national à partir des variables x et y spécifiées en tant
que paramètres d'entrée de la fonction
}
