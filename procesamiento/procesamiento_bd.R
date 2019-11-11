
rm(list = ls())

#setwd("D:/Geografia/eramo01.github.io")
setwd("C:/Users/RAÚL/Documents/Raul/Inegi/eramo01.github.io")


# Cambios en establecimientos1.csv
#establecimientos1 <- read.csv("procesamiento/municipal_2_SPSS_All/establecimientos1.csv", encoding = "UTF-8")
#establecimientos1 <- establecimientos1[-151,-c(14:16,28:31,33,36:40)]
#names(establecimientos1) <- c("llave","folio_sist","tipo","id_denue","latitud","longitud","prec",
#"altitud","hora_cg","planeado","nvo_establec","saludo","acepta","consumo","mercado","residuos_vc","residuos_vp","residuos_ac","residuos_ap",
#"residuos_ot","ot_residuos_e","despedida","obs_1","obs_2","obs_3","obs_4","obs_5")
# Ubica los registros a los que se agregaron coordenadas a mano
# subset(munic[,c(1,3:6)], munic$latitud > 0 & is.na(munic$prec == TRUE) & munic$tipo == 2)
# Ubica establecimiento con error en coordenadas
# subset(munic[,c(1,3:6)], munic$id_denue == 8974)
# Se filtran los casos de entrevistas válidas
#establecimientos1 <- subset(establecimientos1, establecimientos1$acepta == 1 & establecimientos1$tipo == 2, na.rm = T)
# Se eliminan los registros rechazados por el supervisor


# Cambios en interview_diagnostics.csv
interview_diagnostics <- read.csv("procesamiento/municipal_2_SPSS_All/interview_diagnostics.csv", encoding = "UTF-8")
interview_diagnostics <- interview_diagnostics[,-c(3,5:9)]
names(interview_diagnostics) <- c("llave","folio_sist","entrev","durac")


# Cambios en interview_comments.csv
interview_comments <- read.csv("procesamiento/municipal_2_SPSS_All/interview_comments.csv", encoding = "UTF-8")
interview_comments <- interview_comments[,-c(3:5,7,11,13:15)]
names(interview_comments) <- c("llave","folio_sist","estatus","fecha_com","hora_com","autor_com","comentario")


# Lee tabla lista_residuos.csv
lista_residuos <- read.csv("procesamiento/municipal_2_SPSS_All/lista_residuos.csv", encoding = "UTF-8")
names(lista_residuos) <- c("llave","folio_sist","tipo_resid","num_resid")

# Cambios en lista_tiporesiduos.csv
lista_tiporesiduos <- read.csv("procesamiento/municipal_2_SPSS_All/lista_tiporesiduos.csv", encoding = "UTF-8")
lista_tiporesiduos <- lista_tiporesiduos[,-c(6,7,17,18,22)]
lista_tiporesiduos$V4 <- lista_tiporesiduos$V4+1
names(lista_tiporesiduos) <- c("llave","folio_sist","tipo_resid","consec_residuo",
  "nombre_residuo","cantidad_resid","unidad_resid",
  "otra_unidad","cant_r_no_comes","unid_r_no_comes","otra_ur_no_comes",
  "t_colecta","otro_t_colecta","causa_desecho","destino","cant_banco",
  "unidad_banco","otro_destino","autoriza_imag","imagen_resid")

# Se lee tabla de muestra
#muestra <- read.csv("procesamiento/bd_explot/muestra.csv")
# Se adicionan a la tabla establecimientos2 los campos provenientes de la muestra
#library(sqldf)
#establecimientos1a <- sqldf("select establecimientos1.*, muestra.nombre_establecimiento, muestra.codigo_scian, 
#muestra.personas_ocupadas, muestra.municipio, muestra.tipo_unidad_economica, muestra.sector_scian, 
#muestra.tipo_actividad from establecimientos1 left join muestra on establecimientos1.id_denue == muestra.id_denue")
#write.csv(establecimientos1a, "procesamiento/bd_explot/establecimientos1a.csv")

# Lee tabla modificada de establecimientos
establecimientos2 <- read.csv("procesamiento/bd_explot/establecimientos1a.csv")
# Esta tabla ya se encuentra revisada y con registros válidos

#write.csv(interview_diagnostics, "procesamiento/bd_explot/interview_diagnostics.csv")
#write.csv(lista_residuos, "procesamiento/bd_explot/lista_residuos.csv")
#write.csv(lista_tiporesiduos, "procesamiento/bd_explot/lista_tiporesiduos.csv")
#write.csv(lista_tiporesiduos, "procesamiento/bd_explot/lista_tiporesiduos.csv")


### Ahora se obtienen las tablas restantes depuradas y complementadas (interview_comments no se modifica):
library(sqldf)
interview_diagnostics2 <- sqldf("select interview_diagnostics.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica
                                from establecimientos2 join interview_diagnostics
                                 on interview_diagnostics.llave = establecimientos2.llave")

lista_residuos2 <- sqldf("select lista_residuos.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica,
                                establecimientos2.sector_scian
                                from lista_residuos join establecimientos2 
                                 on lista_residuos.llave = establecimientos2.llave")

lista_tiporesiduos2 <- sqldf("select lista_tiporesiduos.*, establecimientos2.id_denue, 
                      establecimientos2.nombre_establecimiento, establecimientos2.personas_ocupadas,
                                establecimientos2.tipo_actividad, establecimientos2.tipo_unidad_economica,
                                establecimientos2.sector_scian
                                from lista_tiporesiduos join establecimientos2 
                                 on lista_tiporesiduos.llave = establecimientos2.llave")


