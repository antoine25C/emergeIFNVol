#' Cette fonction renvoie un tableau récapitulant pour chaque modèles les coefficient de la régression linéaire
#'
#' @param modeles list des modèles calibrés issue de la fonction emergeIFNVol::modelisation
#'
#' @return data.frame avec la sylvo-éco-région et l'esence ou groupe d'essence auxquels le modèle s'applique et les
#' coefficients du modèles : intercept, coefficients multiplicateurs de lahauteur, du diamètre et de la décroissance métrique
#' + renvoie le R² de chaque modèle
#' @export
#'
#' @import rlist
recap_coef_modeles <- function (modeles){
  ser_list = character()
  ess_list = character()
  intercept_list = numeric()
  htot_list = numeric()
  diam_list = numeric()
  decroi_list = numeric()
  r2_list = numeric()
  for (ser in names(modeles)){
    for (ess in names(modeles[[ser]])){
      ser_list <- list.append(ser_list, ser)
      ess_list <- list.append(ess_list, ess)
      r2_list <- list.append(r2_list, summary(modeles[[ser]][[ess]]$modele_lin)$r.squared)
      intercept <- try(modeles[[ser]][[ess]]$modele_lin$coefficients[["(Intercept)"]], silent = T)
      if(class(intercept)=="try-error") {intercept <- 0}
      intercept_list <- list.append(intercept_list, intercept)

      htot <- try(modeles[[ser]][[ess]]$modele_lin$coefficients[["htot"]], silent = T)
      if(class(htot)=="try-error") {htot <- 0}
      htot_list <- list.append(htot_list, htot)

      diam <-try(modeles[[ser]][[ess]]$modele_lin$coefficients[["c13"]], silent = T)
      if(class(diam)=="try-error") {diam <- 0}
      diam_list <- list.append(diam_list, diam)

      decroi <- try(modeles[[ser]][[ess]]$modele_lin$coefficients[["decroi"]], silent = T)
      if(class(decroi)=="try-error") {decroi <- 0}
      decroi_list <- list.append(decroi_list, decroi)
    }
  }
  coefs <- data.frame(ser = ser_list, ess = ess_list, intercept = intercept_list,
                      htot = htot_list, diam = diam_list, decroi = decroi_list,r2 = r2_list)
  return(coefs)
}

