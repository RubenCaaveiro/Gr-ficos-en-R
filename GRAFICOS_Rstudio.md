# Gr-ficos-en-R
Diversos gráficos y funciones de gestión y filtrado de BBDD en R

### PRACTICA 2: Manejo de tablas y gráficos en R  ####
#  AUTOR: Ruben Caaveiro Barro
#  FECHA FIN: 4/3/2022
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _.

#_____EJERCICIO 1______#


#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#1a) G. BARRAS para frecuencia de accidentes por mes enn 2021

#se importa la tabla de Accidentalidad
library(readxl)
Accident<-read_excel('2021_Accidentalidad.xlsx')

AC1<-data.frame(c(substr(Accident$fecha, start = 6, stop = 7)))
cbind(AC1,Accident)->Accidnt_mes
Accbymes<-print(aggregate(Accidnt_mes$c.substr.Accident.fecha..start...6..stop...7.., 
                          by = list(Accidnt_mes$c.substr.Accident.fecha..start...6..stop...7..), FUN = length))
colnames(Accbymes)[1]<-c('Data')
colnames(Accbymes)[2]<-c('Accidentes_mensuales')

print(class(Accbymes))

#generar el gráfico con un límite de y en 5000 ya que no se acumulan más de 4795 accidentes en ningun mes
windows(width = 30, height = 25)
install.packages('cowplot')
library(cowplot)
library(ggplot2)

a <- ggplot(Accbymes, aes(x=Data, y=Accidentes_mensuales)) + 
  labs(x="Meses de 2021", y="Frecuencia de accidentes por mes")+ geom_bar(stat="identity", 
                                                                          position="dodge", 
                                                                          width = 0.5,
                                                                          color = (1:12), 
                                                                          fill=(1:12),
                                                                          alpha = 0.6)+
  scale_x_discrete(name = "Meses", 
                   labels = c('enero','febrero','marzo','abril','mayo','junio','julio',
                              'agosto','septiempre','octubre','noviembre','diciembre')) +
  coord_cartesian(xlim = c(1, 12),
                  ylim = c(0, 5000)) +
  theme(axis.text.x = element_text(angle = 70,
                                   size = 9,
                                   hjust = 1,
                                   vjust = 1))


#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#1b) gráfico circular de accidentes que afectan según grandes grupos de edad

cnt_edades<-print(aggregate(Accident$rango_edad, by = data.frame(Accident$rango_edad), FUN = length))
colnames(cnt_edades)=c('Rango de edad','Accidentes')
#Muestra cantidad de accidentes para cada grupo de edad

install.packages('dplyr')
library('dplyr')

#filtrar filas correspondientes para cada rango de edad:
jovenes<-cnt_edades[c(1,2,12,18),]
adultos<-cnt_edades[c((3:11),13),]
ancianos<-cnt_edades[c(14,15,17),]
desconocidos<-cnt_edades[16,]
#sumar los rangos de edad y crear un data frame:
joven<-print(sum(jovenes$Accidentes))
adulta<-print(sum(adultos$Accidentes))
anciana<-print(sum(ancianos$Accidentes))
desconocido<-print(sum(desconocidos$Accidentes))

edades<-data.frame(categoria = c("joven","adulta","anciana","desconocido"), cantidad = c(1462,33475,2169,4677))
#meto todos los datos en un df


#Sectores o grafico  circular
g <- ggplot(edades, aes(x="", y=cantidad, fill=categoria))+ 
            geom_bar(width = 1, stat = "identity") + 
            geom_col(color = 'darkgrey') +
            geom_label(aes(label = cantidad),  position = position_stack(vjust = 0.5), show.legend = FALSE) +
            coord_polar(theta = "y")



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#1c)Exportar los gráficos

#se unen los graficos en paralelo
z <- plot_grid(a, g, labels=c("A", "B"), ncol = 2, nrow = 1)
#se exportan a formato imagen
save_plot("ggplots.png",z, base_width=18, base_height =7)



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#_____EJERCICIO 2______#



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#2a)Gráfico de líneas para evolución del % de uso del tren entre 1990 2020: Austria, España, Suecia y Suiza.

#se importa la hoja 1 para tren con rango conveniente:
library(readxl)
TREN<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 1", range = 'A9:BI47')

#Selección de Austria, España, Suecia y Suiza
pais<-TREN[c(12,33,30,23),(seq(0,61, by=2))]
pais_tren<-cbind(PaisEU = c("España",'Suiza','Suecia','Austria'),pais)#añadir columna de nombres de país 

#Transpone todas las columnas menos la primeraa
tren_plot <- data.frame(t(pais_tren[-1]))
#Añadimos los nombres de las columnas
colnames(tren_plot) <- pais_tren[, 1]

fin<-cbind(Anio = c(1990:2019),tren_plot)#data.frame final


#Gráfico de líneas múltiplee para los 4 países


windows(width = 30, height = 25)
#Guardamos los parámetros gráficos actuales
opar <- par(no.readonly = TRUE)
#estas DOS PRIMERAS LÍNEAS:Extraen el gráfico en nueva ventana, si no se quiere
#basta con prescindir de ellas.

#Cambiar los márgenes del gráfico 
par(mar = c(6, 5, 4, 3))

plot(x=fin$Anio, y=fin$Suecia, #las dos variables principales
     xlab = "Evolución anual",ylab = "% de uso del tren", 
     type='l',col= 6, ylim=c(0,25))
lines(x=fin$Anio, y=fin$España, type='l', col= 3)
lines(x=fin$Anio, y=fin$Suiza, type='l', col= 1)
lines(x=fin$Anio, y=fin$Austria, type='l', col= 4)
#añadimos las tres variables lineales que faltan

title(main = "Evolución de uso del tren",#título
      col.main = "green4",
      sub = "Fuente: EUROSTAT",
      col.sub = "green4",
      col.lab = "blue", font.lab = 3)

legend(x='top', legend = c('Suecia','España','Suiza','Austria' ),
       title = "Países",   # Título de leyenda
       title.adj = 0.5,    # Ajuste horizontal
       title.col = "green4",
       col = c(6,3,1,4), lty = 1,cex=0.7, horiz = TRUE)#se dispone en horizontal
      #y con letra más pequeña 



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#2b) Exportar csv por comas para distribución modal de transporte en 2019: Portugal,
    #Francia, Polonia, Grecia y Luxemburgo.

library(readxl)
Train<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 1", range = 'A9:BI47')
Car<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 2", range = 'A9:BI47')
Bus<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 3", range = 'A9:BI47')

Train_<-Train[c(25,13,24,11,19),('2019')]
Car_<-Car[c(25,13,24,11,19),('2019')]
Bus_<-Bus[c(25,13,24,11,19),('2019')]

Transport<-data.frame(Train_,Car_,Bus_)
Transporte<-cbind(Paises = c('Portugal', 'Francia', 'Polonia', 'Grecia','Luxemburgo'),Transport)
colnames(Transporte)<-c('País','Ferrocarril', 'Automóvil', 'Autocares, autobuses y trolebuses')
#se exporta en csv por comas
write.csv(Transporte, "Transport_mods.csv")
#muestra resultado en consola
read.csv('Transport_mods.csv')



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#2c) Representar un box-plot múltiple con la distribución modal de coche y bus en el año 2019.
#Exportar los resultados de 2a y 2c en una única composición.

library(readxl)
Car<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 2", range = 'A9:BI47')
Bus<-read_excel("t2020_rk310_spreadsheet.xlsx", sheet = "Sheet 3", range = 'A9:BI47')

Coches<-Car[ ,'2019']
Buses<-Bus[ ,'2019']

Coche_Bus<-data.frame(Coches,Buses)
colnames(Coche_Bus)<-c('Coche','Bus')


windows(width = 30, height = 25)
#Guardamos los parámetros gráficos actuales

boxplot(Coche_Bus, xlab='Modo de transporte', ylab='Porcentaje de demanda',
        main = 'Distribucion modal del trasporte', col=c(2,5),notch = TRUE)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de línea
     col = "gray", # Color
     lwd = 2) 



#Exportar los graficos a png en una sola imagen

png('Ejercicio2_Grafics.png', width=6000, height = 3000, pointsize = 30)
par(mfrow=c(1,2))#permite incluir os dos gráficos
plot(x=fin$Anio, y=fin$Suecia, 
     xlab = "Evolución anual",ylab = "% de uso del tren", 
     type='l',col= 6, ylim=c(0,25))
lines(x=fin$Anio, y=fin$España, type='l', col= 3)
lines(x=fin$Anio, y=fin$Suiza, type='l', col= 1)
lines(x=fin$Anio, y=fin$Austria, type='l', col= 4)


title(main = "Evolución de uso del tren",
      col.main = "green4",
      sub = "Fuente: EUROSTAT",
      col.sub = "green4",
      col.lab = "blue", font.lab = 3)

legend(x='top', legend = c('Suecia','España','Suiza','Austria' ),
       title = "Países",   
       title.adj = 0.5,    
       title.col = "green4",
       col = c(6,3,1,4), lty = 1,cex=0.7, horiz = TRUE)


boxplot(Coche_Bus, xlab='Modo de transporte', ylab='Porcentaje de demanda',
        main = 'Distribucion modal del trasporte', col=c(2,5),notch = TRUE)
grid(nx = NULL, ny = NULL,
     lty = 2,      
     col = "gray", 
     lwd = 2) 

dev.off()





#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#_____EJERCICIO 3______#



#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#3a) Calcular la Temperatura media diaria en una nueva columna entendiéndola como el 
    #promedio de la Temperatura máxima y mínima.

Tudela_dat<-read.table('Tudela_2020.txt', header = T, sep = "\t")
Bera_dat<-read.table('Bera_2020.txt', header = T, sep = "\t")

Bera_dat[c(0:366),]->Bera_dat2 #no hay datos a partir de la fila 366.


#promedio de los valores entre columnas de tº max y min:
maxminTU <- data.frame(rowMeans (Tudela_dat [, c (2,3)], 
                                 na.rm = TRUE ))#'na.rm' elimina valores perdidos, si los hay, en la variable.
print(mean(maxminTU$rowMeans.Tudela_dat...c.2..3....na.rm...TRUE.))->PM_Tudela

maxminBE <- data.frame(rowMeans (Bera_dat2[, c (2,3)], 
                                 na.rm = TRUE ))
print(mean(maxminBE$rowMeans.Bera_dat2...c.2..3....na.rm...TRUE.))->PM_Bera

Tudela_Tº<-cbind(Tudela_dat,maxminTU)
          colnames(Tudela_Tº)[10]<-c('Promedio Tª')
          
Bera_Tº<-cbind(Bera_dat2,maxminBE)
          colnames(Bera_Tº)[13]<-c('Promedio Tª')


      
          
          
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#3b)Crear archivo txt separada por tabuladores que indique el % de vientos correspondientes 
    #(respecto al periodo anual) a cada sector y por municipio
          
#se agrupa una tabla con el recuento de valores para cada dirección de viento
WinDIRT<-aggregate(Tudela_Tº$Dirección.viento.2.m..MODA..sector, 
          by = list(Tudela_Tº$Dirección.viento.2.m..MODA..sector), FUN = length)
round(prop.table(WinDIRT$x), 4)*100

WinDIRB<-aggregate(Bera_Tº$Dirección.viento.10.m..MODA..sector, 
          by = list(Bera_Tº$Dirección.viento.10.m..MODA..sector), FUN = length)
round(prop.table(WinDIRB$x), 4)*100


#Se genera el df
Sector<-c('N','NE','E','SE','S','SO','O','NO')
  WTS<-cbind(Sector,WinDIRT)
       colnames(WTS)=c('Sectores','Direcciones','Frecuencia Tudela')
    
Sector<-c('N','NE','S','SO','O','NO','Desconocido')   
  WBS<-cbind(Sector,WinDIRB)
       colnames(WBS)=c('Sectores','Direcciones','Frecuencia Bera')
       rename(WBS,'Periodos mensuales' = 'c.substr.Bera_Tº.Fecha.hora..start...4..stop...5..',
              'Precipitación Acumulada en Bera'='precip_acum')
       round(prop.table(WBS$Frecuencia), 4)*100
            
#este 'full inner join' con 'merge' permitefusionar dos data frames en uno sin importar número de filas
merge(x = WTS, y = WBS, by = c("Sectores", "Direcciones"), all = TRUE) -> Vientos
Vientos$`Frecuencia Tudela`/366*100#----------------uuuuuuuuaaaaaaaaaaaaaaaaaaaaaaaaaa

#exportación de la tabla txt.
write.table(Vientos, file = 'Vientos_Tudela_Bera.txt', sep = "\t", 
            row.names = FALSE, col.names = TRUE)


#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

#3c)Climograma DE precipitación mensual con variable Precipitación acumulada l/m²*mes
    #como barras y, la temperatura media mensual
    

CLM_DATT<-data.frame(c(substr(Tudela_Tº$Fecha.hora, start = 4, stop = 5)))
         cbind(CLM_DATT,Tudela_Tº)->TUDE_M
TUDE_MES<-print(aggregate(TUDE_M$`Precipitación.acumulada.l.m²`, 
                          by = list(TUDE_M$c.substr.Tudela_Tº.Fecha.hora..start...4..stop...5..), FUN = length))
          colnames(TUDE_MES)[1]<-c('Data.Mensual')
          colnames(TUDE_MES)[2]<-c('Count')
  
CLM_DATB<-data.frame(c(substr(Bera_Tº$Fecha.hora, start = 4, stop = 5)))
          cbind(CLM_DATB,Bera_Tº)->BERA_M
BERA_MES<-print(aggregate(BERA_M$`Precipitación.acumulada.l.m²`, 
                                    by = list(BERA_M$c.substr.Bera_Tº.Fecha.hora..start...4..stop...5..), FUN = length))
          colnames(BERA_MES)[1]<-c('Data.Mensual')
          colnames(BERA_MES)[2]<-c('Count')   
          
          

  
library("dplyr") #se filtra y suma por meses la precipitación
          
      Precip_acc_TUD <- print(TUDE_M %>%
            group_by(c.substr.Tudela_Tº.Fecha.hora..start...4..stop...5..) %>%
                     summarize(precip_acum=sum(`Precipitación.acumulada.l.m²`)))
          colnames(Precip_acc_TUD)[1]<-c('Periodos mensuales')
          colnames(Precip_acc_TUD)[2]<-c('Precipitación acumulada en Tudela')
          
      PREC_T_transp <- data.frame(t(Precip_acc_TUD[-1]))
          
          colnames(PREC_T_transp)[1:12] <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio',
                                             'Agosto','Septiempre','Octubre','Noviembre','Diciembre')
        
          
          
      Precip_acc_BER <- print(BERA_M %>%
            group_by(c.substr.Bera_Tº.Fecha.hora..start...4..stop...5..) %>%
                     summarize(precip_acum=sum(`Precipitación.acumulada.l.m²`)))
          
          rename(Precip_acc_BER, 'Periodos mensuales' = 'c.substr.Bera_Tº.Fecha.hora..start...4..stop...5..',
                 'Precipitación Acumulada en Bera'='precip_acum')
          
      PREC_B_transp <- data.frame(t(Precip_acc_BER[-1]))
          
          colnames(PREC_B_transp)[1:12] <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio',
                                             'Agosto','Septiempre','Octubre','Noviembre','Diciembre')
          
      
#TEMPERATURA MEDIA MENSUAL

TEMP_TUDELA <- TUDE_M %>%
            group_by(c.substr.Tudela_Tº.Fecha.hora..start...4..stop...5..)%>%
                    summarize(Temperatura_media = mean(`Promedio Tª`))
            colnames(TEMP_TUDELA)[1]<-c('Periodos mensuales')

TEMP_BERA <- BERA_M %>%
            group_by(c.substr.Bera_Tº.Fecha.hora..start...4..stop...5..)%>%
                    summarize(Temperatura_media = mean(`Promedio Tª`))
            colnames(TEMP_BERA)[1]<-c('Periodos mensuales')
  
#hallamos la precipitación y temperatumra media totales para los dos pueblos:
  #precipitacion anual acumulada
   total_prc_T<-print(sum(PREC_T_transp))
   total_prc_B<-print(sum(PREC_B_transp))
  #temperatura anual media
   temp_anual_T<-print(mean(TEMP_TUDELA$Temperatura_media))
   temp_anual_B<-print(mean(TEMP_BERA$Temperatura_media))
          
#GRAFICO COMBINADO
            #se crean dos escalas de color con degradado para las barras
            col1 <- rgb(0.4, 0.3, 0.3, 0.6)
            col2 <- rgb(0.1, 0.6, 0.3, 0.2)
            
# precipitacion en Tudela         
barplot(as.matrix(PREC_T_transp),
        col = col1,
        main= 'El clima en Tudela y Bera', 
        sub = 'Temperatura media anual: Tudela > 15,11 ºC  Bera > 14,13 ºC        Precipitación acumulada anual: Tudela > 416.37 l/m^2 Bera > 1661.46 l/m^2', 
        cex.sub = 0.7,
        xlab = 'Rango temporal en meses', ylim = c(0,366), xlim = c(0,12),
        width = 0.78, space=0.25, las = 3, cex.names = 0.75, cex.lab = 0.75)
        
# tamaño de salida de gráfico (marcos de salida grafica)
par(mar=c(6,4,3,4))

# precipitacion en Bera
barplot(as.matrix(PREC_B_transp), col= col2,
        width = 0.78, 
        space=0.25,
        add = T, las = 3, cex.names = 0.75, cex.lab = 0.75)

par(new = T)
#temperatura de Tudela
plot(TEMP_TUDELA, 
     col = '#FF0000', 
     type = 'o', 
     lty = '72', 
     add = T, ann = F, axes = F, 
     ylim = c(0,35))

par(new = T)
#temperatura de Bera
plot(TEMP_BERA, col = 'darkorchid4', 
     type = 'o', 
     lty = '72', 
     add = T, ann = F, axes = F,
     ylim = c(0,35))
#parámetrs para la combinación de 2 gráficos en 1
axis(4, ylim = c(0,35))
mtext('Temperatura ºC', side = 4,  line = 2,  cex = 1)
mtext('Precipitación acumulada', side = 2, cex = 1)

legend('topleft', legend =c('Temperatura Tuedela', 'Temperatura Bera', 'Precipitacion Tudela', 'Precipitacin Bera'),
       col = c('#FF0000', 'darkorchid4', '#8B8970', '#D1EEEE'),
       lty = 1, cex = 0.8, 
       pch = c(1,1,18,18))
      
