res_calc_vol<- Calc_Vol_Emerge(Id_arbre = arbre$Id_arbre, ess=arbre$Cod_ess, d13 = arbre$Diam,
                        htot = arbre$H_def, X = arbre$Xarbre_recale, Y = arbre$Yarbre_recale, crs = 2154)
res_calc_vol$ida <- as.character(res_calc_vol$ida)
arbre <- arbre %>%
  left_join(res_calc_vol[colnames(res_calc_vol) %in% c("ida","hdec","v")], by = c('Id_arbre'='ida')) %>%
  mutate(Vol = v) %>%
  dplyr::select(-v)


system.time(test <- Calc_Vol_Emerge(Id_arbre = arbre$Id_arbre, ess=arbre$Cod_ess, d13 = arbre$Diam,
                        htot = arbre$H_def, X = arbre$Xarbre_recale, Y = arbre$Yarbre_recale, crs = 2154))
Id_arbre = arbre$Id_arbre
ess=arbre$Cod_ess
d13 = arbre$Diam
htot = arbre$H_def
X = arbre$Xarbre_recale
Y = arbre$Yarbre_recale
crs = 2154


system.time(test <- Calc_Vol_Emerge(IFNarbres$ida, IFNarbres$code, IFNarbres$d13, IFNarbres$htot, X= IFNarbres$xl93,Y =IFNarbres$yl93, crs = 2154))
IFNarbres <- IFNarbres %>%
  left_join(test[,c('ida','v')], by = 'ida')

ggplot(IFNarbres, aes(x=v.x, y= v.y, color = code))+
  geom_point()+
  geom_abline(slope = 1, intercept= 0, color ='red')
