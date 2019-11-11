rm(list = ls())

setwd("D:/xampp/htdocs/pagina_sai/eramo01.github.io")
#setwd("C:/Users/RAÚL/Documents/Raul/Inegi/eramo01.github.io")


# Lee tablas
establecimientos2 <- read.csv("procesamiento/bd_explot/establecimientos2.csv")
interview_diagnostics2 <- read.csv("procesamiento/bd_explot/interview_diagnostics2.csv")
lista_residuos2 <- read.csv("procesamiento/bd_explot/lista_residuos2.csv")
lista_tiporesiduos2 <- read.csv("procesamiento/bd_explot/lista_tiporesiduos2.csv")





##### Tipo de residuos
barplot(c(sum(establecimientos2$residuos_vc,na.rm = T),
          sum(establecimientos2$residuos_vp,na.rm = T),
          sum(establecimientos2$residuos_ac,na.rm = T),  
          sum(establecimientos2$residuos_ap,na.rm = T),
          sum(establecimientos2$residuos_ot,na.rm = T)), 
        beside=T, 
        main='Tipos de residuos declarados',
        legend.text= c('Vegetal crudo','Vegetal procesado', 'Animal crudo',
                       'Animal procesado', 'Otro tipo'),
        args.legend = list(x = "topright", bty="n", cex = 0.9, ncol = 2),
        ylab='Frecuencia', 
        ylim= c(0,80),
        col=c('green','darkgreen','gold2','orange','cyan'))


library(tidyverse)
library(tidytext)

# Descripción del residuo 1
texto5a <- read.csv("procesamiento/bd_explot/descrip_residuos1.csv", head= F)
df5a <- texto5a %>% count(V1, sort = TRUE)
df5a
names(df5a) <- c("Palabra (vegetal crudo)", "Frecuencia")
# Descripción del residuo 2
texto5b <- read.csv("procesamiento/bd_explot/descrip_residuos2.csv", head= F)
df5b <- texto5b %>% count(V1, sort = TRUE)
df5b
names(df5b) <- c("Palabra (vegetal procesado)", "Frecuencia")
# Descripción del residuo 3
texto5c <- read.csv("procesamiento/bd_explot/descrip_residuos3.csv", head= F)
df5c <- texto5c %>% count(V1, sort = TRUE)
df5c
names(df5c) <- c("Palabra (animal crudo)", "Frecuencia")
# Descripción del residuo 4
texto5d <- read.csv("procesamiento/bd_explot/descrip_residuos4.csv", head= F)
df5d <- texto5d %>% count(V1, sort = TRUE)
df5d
names(df5d) <- c("Palabra (animal procesado)", "Frecuencia")

df5 <- cbind(df5a[1:15,],df5b[1:15,],df5c[1:15,],df5d[1:15,])
write.csv(df5, "procesamiento/bd_explot/descrip_residuos_texto.csv")



# Texto Otros residuos 
texto1 <- read.csv("procesamiento/bd_explot/otros_residuos.csv", head= F)
df1 <- texto1 %>% count(V1, sort = TRUE)
df1
names(df1) <- c("Palabra", "Frecuencia")
write.csv(df1, "procesamiento/bd_explot/otros_residuos_texto.csv")


## Cantidad de residuos

lista_tiporesiduos2$kilos <- rep(0,191)
lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$unidad_resid==1)] <- lista_tiporesiduos2$cantidad_resid[which(lista_tiporesiduos2$unidad_resid==1)]/1000
lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$unidad_resid==2)] <- lista_tiporesiduos2$cantidad_resid[which(lista_tiporesiduos2$unidad_resid==2)]
lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$unidad_resid==3)] <- lista_tiporesiduos2$cantidad_resid[which(lista_tiporesiduos2$unidad_resid==3)]*1000

lista_tiporesiduos2$kilos_sem <- rep(0,191)
lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$t_colecta==1)] <- lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$t_colecta==1)]*7
lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$t_colecta==2)] <- lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$t_colecta==2)]
lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$t_colecta==3)] <- lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$t_colecta==3)]/2
lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$t_colecta==4)] <- lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$t_colecta==4)]/4
lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$t_colecta==5 & lista_tiporesiduos2$otro_t_colecta == "anual")] <- lista_tiporesiduos2$kilos[which(lista_tiporesiduos2$t_colecta==5 & lista_tiporesiduos2$otro_t_colecta == "anual")]/52


lista_tiporesiduos2$kilos_nc <- rep(0,191)
lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$unid_r_no_comes==1)] <- lista_tiporesiduos2$cant_res_no_comes[which(lista_tiporesiduos2$unid_r_no_comes==1)]/1000
lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$unid_r_no_comes==2)] <- lista_tiporesiduos2$cant_res_no_comes[which(lista_tiporesiduos2$unid_r_no_comes==2)]
lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$unid_r_no_comes==3)] <- lista_tiporesiduos2$cant_res_no_comes[which(lista_tiporesiduos2$unid_r_no_comes==3)]*1000

lista_tiporesiduos2$kilos_sem_nc <- rep(0,191)
lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$t_colecta==1)] <- lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$t_colecta==1)]*7
lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$t_colecta==2)] <- lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$t_colecta==2)]
lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$t_colecta==3)] <- lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$t_colecta==3)]/2
lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$t_colecta==4)] <- lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$t_colecta==4)]/4
lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$t_colecta==5 & lista_tiporesiduos2$otro_t_colecta == "anual")] <- lista_tiporesiduos2$kilos_nc[which(lista_tiporesiduos2$t_colecta==5 & lista_tiporesiduos2$otro_t_colecta == "anual")]/52


## Promedios
mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0)])
mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0)])
sum(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" & 
                                          (lista_tiporesiduos2$sector_scian == "Secundario" | lista_tiporesiduos2$sector_scian == "Terciario"))])
sum(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & (lista_tiporesiduos2$nombre_establecimiento == "LA HUERTA" |
                                          lista_tiporesiduos2$sector_scian == "Primario"))])


mean(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0)])
mean(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])
median(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0)])
sum(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" & 
                                          (lista_tiporesiduos2$sector_scian == "Secundario" | lista_tiporesiduos2$sector_scian == "Terciario"))])
sum(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & (lista_tiporesiduos2$nombre_establecimiento == "LA HUERTA" |
                                                                               lista_tiporesiduos2$sector_scian == "Primario"))])





##  Graficas residuos totales y no comestibles
plot(sort(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")]), 
     col="darkgreen", pch=18,
     main = "Cantidad de residuos semanales declarados en la muestra", xlab = "", ylab="kg")


plot(sort(lista_tiporesiduos2$kilos_sem_nc[which(lista_tiporesiduos2$kilos_sem_nc > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")]), 
     col="red", pch=18,
     main = "Cantidad de residuos semanales no comestibles declarados en la muestra", xlab = "", ylab="kg")


##  Graficas residuos por tipo
plot(sort(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                                lista_tiporesiduos2$tipo_resid == 1)]), 
     col="darkblue", pch=18,
     main = "Cantidad de residuos de vegetal crudo semanales", xlab = "", ylab="kg")

plot(sort(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                                lista_tiporesiduos2$tipo_resid == 2)]), 
     col="blue", pch=18,
     main = "Cantidad de residuos de vegetal procesado semanales", xlab = "", ylab="kg")


plot(sort(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                                lista_tiporesiduos2$tipo_resid == 3)]), 
     col="blueviolet", pch=18,
     main = "Cantidad de residuos de animal crudo semanales", xlab = "", ylab="kg")

plot(sort(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                                lista_tiporesiduos2$tipo_resid == 4)]), 
     col="purple", pch=18,
     main = "Cantidad de residuos de animal procesado semanales", xlab = "", ylab="kg")


## Promedios residuos por tipo
mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                           lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                           lista_tiporesiduos2$tipo_resid == 1)])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 &
                                                lista_tiporesiduos2$tipo_resid == 1)])


mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                           lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                           lista_tiporesiduos2$tipo_resid == 2)])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 &
                                                lista_tiporesiduos2$tipo_resid == 2)])


mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                           lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                           lista_tiporesiduos2$tipo_resid == 3)])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 &
                                                lista_tiporesiduos2$tipo_resid == 3)])


mean(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                           lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                           lista_tiporesiduos2$tipo_resid == 4)])
median(lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 &
                                                lista_tiporesiduos2$tipo_resid == 4)])



                                                      
## Residuos por tamaño del establecimiento                                                                                                              
levels(lista_tiporesiduos2$personas_ocupadas) <- c("0 a 5 pers", "6 a 10 pers", "11 a 30 pers", "31 a 50 pers", 
                                                   "51 a 100 pers", "101 a 250 pers", "251 y más pers")  
or <- order(lista_tiporesiduos2$personas_ocupadas)

ggplot(lista_tiporesiduos2[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA"),], 
       aes(x=lista_tiporesiduos2$personas_ocupadas[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                         lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")], 
           y=lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch
    notch=F,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3) +
  xlab('Personas Ocupadas') + ylab('kg') + 
  ggtitle('Cantidad de residuos declarados por tamaño del establecimiento')



## Residuos por tipo de actividad   
ggplot(lista_tiporesiduos2[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA" &
                                   lista_tiporesiduos2$tipo_actividad != ''),], 
       aes(x=lista_tiporesiduos2$tipo_actividad[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                           lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA"&
                                                           lista_tiporesiduos2$tipo_actividad != '')], 
           y=lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA"&
                                                   lista_tiporesiduos2$tipo_actividad != '')])) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch
    notch=F,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3) +
  xlab('') + ylab('kg') + 
  ggtitle('Cantidad de residuos declarados por tipo de actividad') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## Residuos por sector de actividad 
ggplot(lista_tiporesiduos2[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA"),], 
       aes(x=lista_tiporesiduos2$sector_scian[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                        lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")], 
           y=lista_tiporesiduos2$kilos_sem[which(lista_tiporesiduos2$kilos_sem > 0 & 
                                                   lista_tiporesiduos2$nombre_establecimiento != "LA HUERTA")])) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch
    notch=F,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3) +
  xlab('') + ylab('kg') + 
  ggtitle('Cantidad de residuos declarados por sector de actividad') 





## Destinos
barplot(c(length(which(lista_tiporesiduos2$destino == 1)), 
          length(which(lista_tiporesiduos2$destino == 2)),
          length(which(lista_tiporesiduos2$destino == 4)),
          length(which(lista_tiporesiduos2$destino == 5)),
          length(which(lista_tiporesiduos2$destino == 7)),
          length(which(lista_tiporesiduos2$destino == 8))),
        beside=T, legend.text= c('Alcantarillado','Alimentación animal', 'Basurero / desperdicio / desecho',
                                 'Composteo / procesos aeróbicos', 
'Banco de alimentos / comedor público', 'Otro destino'),
        main='Destinos de los residuos declarados',
        args.legend = list(x = "topleft", bty="n", cex = 0.8, ncol = 2),
        ylab='Frecuencia', 
        ylim= c(0,80),
        col=c('blue','darkgreen','gold2','orange','cyan', 'purple'))


# Otros destinos
texto3 <- read.csv("procesamiento/bd_explot/otro_destino.csv", head= F)
df3 <- texto3 %>% count(V1, sort = TRUE)
df3
names(df3) <- c("Palabra", "Frecuencia")
write.csv(df3, "procesamiento/bd_explot/otro_destino_texto.csv")



# Causas del desecho
texto4 <- read.csv("procesamiento/bd_explot/causas_desecho.csv", head= F)
df4 <- texto4 %>% count(V1, sort = TRUE)
df4
names(df4) <- c("Palabra", "Frecuencia")
write.csv(df4, "procesamiento/bd_explot/causas_desecho_texto.csv")


# Observaciones
texto2 <- read.csv("procesamiento/bd_explot/obs.csv", head= F)
df2 <- texto2 %>% count(V1, sort = TRUE)
df2
names(df2) <- c("Palabra", "Frecuencia")
write.csv(df2, "procesamiento/bd_explot/observaciones_texto.csv")




# Duración de entrevista en minutos
interview_diagnostics2$duracion <- as.numeric(substring(as.character(interview_diagnostics2$durac, ':'),5,5))*60 +
  as.numeric(substring(as.character(interview_diagnostics2$durac, ':'),7,8)) + 
  as.numeric(substring(as.character(interview_diagnostics2$durac, ':'),10,11))/60

### Análisis de tiempos de duración

library(MASS)
truehist(interview_diagnostics2$duracion[which(interview_diagnostics2$duracion > 0)], 
     col="deepskyblue2", nbins = 50,
     main = "Histograma de la duración de las entrevistas", xlab = "minutos", ylab="Frec. relativa")


library(ggplot2)

interview_diagnostics2$personas_ocupadas2 <- factor(interview_diagnostics2$personas_ocupadas, 
                                                    levels = c("0 a 5 pers", "6 a 10 pers",
                                                               "11 a 30 pers", "31 a 50 pers",
                                                               "51 a 100 pers", "101 a 250 pers",
                                                               "251 y más pers"))
ggplot(interview_diagnostics2[-85,], 
       aes(x=interview_diagnostics2$personas_ocupadas2[-85], y=interview_diagnostics2$duracion[-85])) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch
    notch=F,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3) +
  xlab('Personas Ocupadas') + ylab('Minutos') + 
  ggtitle('Duración de la entrevista por tamaño del establecimiento')

ggplot(interview_diagnostics2[interview_diagnostics2$tipo_actividad != '',], 
       aes(x=interview_diagnostics2$tipo_actividad[interview_diagnostics2$tipo_actividad != ''],
           y=interview_diagnostics2$duracion[interview_diagnostics2$tipo_actividad != ''])) + 
  geom_boxplot(
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    # Notch
    notch=F,
    notchwidth = 0.8,
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3) +
  xlab('Tipo de actividad') + ylab('Minutos') + 
  ggtitle('Duración de la entrevista por tipo de actividad') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

mean(interview_diagnostics2$duracion[-85])
median(interview_diagnostics2$duracion[-85])




