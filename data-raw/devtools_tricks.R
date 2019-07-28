usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("png")
usethis::use_package("rlist")
usethis::use_package("stringr")
usethis::use_package("stats")
usethis::use_package("sf")
usethis::use_package("grid")
usethis::use_package("tcltk")
usethis::use_package("utils")
usethis::use_package("data.table")
#usethis::use_package("raster")
usethis::use_package("svDialogs")

dir <- "F:/1_Stage_EAM_Lidar_Acct/06_carto_volume/res"
code_ess <-readRDS(paste(dir,"Rdata/codes_ess.rds", sep = "/"))
code_ess_Emerge <-readRDS(paste(dir,"Rdata/code_ess_Emerge.rds", sep = "/"))
codeser <- readRDS(paste(dir,"Rdata/code_ser.rds", sep = "/"))
Ser_shape <- readRDS(paste(dir,"Rdata/ser_shape.rds", sep = "/")) %>%
  dplyr::select(codeser)
sf::st_crs(Ser_shape) <- 2154
dep <- readRDS(paste(dir,"Rdata/dep.rds", sep = "/"))%>%
  mutate(NumDep = as.character(NumDep),
         NomDep = paste(NomDep, NumDep,"    "))
ser <- readRDS(paste(dir,"Rdata/Nom_Ser.rds", sep = "/")) %>%
  mutate(NomSER = paste(NomSER, codeser,"    "))

usethis::use_data(code_ess, internal = T, overwrite = T)
usethis::use_data(code_ess_Emerge, internal = T, overwrite = T)
usethis::use_data(Ser_shape, internal = T, overwrite = T)
usethis::use_data(dep, internal = T, overwrite = T)
usethis::use_data(ser, internal = T, overwrite = T)

usethis::use_data(code_ess, internal = F, overwrite = T)
usethis::use_data(code_ess_Emerge, internal = F, overwrite = T)
usethis::use_data(Ser_shape, internal = F, overwrite = T)
usethis::use_data(dep, internal = F, overwrite = T)
usethis::use_data(ser, internal = F, overwrite = T)
F:\1_Stage_EAM_Lidar_Acct\06_carto_volume\res\package_onfR
