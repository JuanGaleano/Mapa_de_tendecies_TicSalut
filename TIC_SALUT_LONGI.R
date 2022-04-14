
#### LECTURA DE LIBRERIAS ####
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(scales)
library(tidyr)

#### LECTURA DE DATOS #####
X2016 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2016.xlsx")
X2016<-X2016[!X2016$P_001=="Arep",]
X2016$year <- 2016

X2015 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2015.xlsx")
X2015$year <- 2015

X2014 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2014.xlsx")
X2014$year <- 2014

X2013 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2013.xlsx")
X2013$year <- 2013

X2012 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2012.xlsx")
X2012$year <- 2012

X2011 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2011.xlsx")
X2011$year <- 2011

X2010 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2010.xlsx")
X2010$year <- 2010

X2009 <- read_excel("C:/Users/jgaleano/Desktop/TICSALUT/DATOS/2009.xlsx")
X2009$year <- 2009





####################
#### BLOQUE 1 #####
###################
##### P_149_2 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_149_2_1', 
            'P_149_2_2',
            'P_149_2_3',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_149_2_1', 
                                'P_149_2_2',
                                'P_149_2_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Avui real",
                             "No planificat",
                             "Planificat")


dffinal2$categoria<-as.factor(dffinal2$categoria)
levels(dffinal2$categoria)
levels(dffinal2$categoria)<-c("Infraestructures\nhardware",
                             "Utilització d’ aplicacions\n estàndards des del núvol",
                             "Migració d’alguna aplicació\n de negoci a plataformes específiques")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), 
                      paste("(", dffinal2$abs,")", 
                            sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_149_2.csv")

ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 1)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("01_P_149_2.png", scale = 1, dpi = 300, height =12, width = 12)

##### P_150_2 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_150_2_1','P_150_2_2',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_150_2_1','P_150_2_2'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Avui real",
                             "No planificat",
                             "Planificat")


dffinal2$categoria<-as.factor(dffinal2$categoria)
levels(dffinal2$categoria)
levels(dffinal2$categoria)<-c("Núvol privat",
                              "Núvol públic")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), 
                      paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_150_2.csv")

ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 1)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("01_P_150_2.png", scale = 1, dpi = 300, height =8, width = 12)


##### P_136_2 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_136_2_1',
            'P_136_2_2',
            'P_136_2_3',
            'P_136_2_4',
            'P_136_2_5','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_136_2_1',
                                'P_136_2_2',
                                'P_136_2_3',
                                'P_136_2_4',
                                'P_136_2_5'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="Sí",]
  
})

dffinal <- do.call("rbind", dfList2)

##### P_136_2(15,16) #####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_136_2_6','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_136_2_6'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="Sí",]
  
})

dffinal354 <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal,dffinal354)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Correu (Gmail, Exchange,...)", 
                              "Ofimàtica (Googleapps, Office365,...)",
                              "Aplicacions de negoci",
                              "Còpies de seguretat",
                              "Aplicacions de salut amb clau",
                              "Altres")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_136_2.csv")


DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = rel,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,75))+
  guides(colour  = guide_legend(nrow = 6))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")

# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("01_P_136_2_rel.png", scale = 1, dpi = 300, height =6, width = 6)



##### P_028 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_028','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_028'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("telefonia IP")


write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_028.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = abs,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  guides(colour  = guide_legend(nrow = 6))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("01_P_028_abs.png", scale = 1, dpi = 300, height =6, width = 6)


##### P_30 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  
  df<-df[,c('P_030','year')]
  
  DFP030 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_030'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP030<-DFP030[complete.cases(DFP030),] # QUITAMOS LOS CASOS DE NO RESPOSTA
  DFP030<-DFP030[!DFP030$resposta=='No aplica', ]   

POP <- DFP030 %>% 
  group_by(year, categoria, resposta) %>%  
  summarise(valor = n()) %>%
  mutate(eti = round((valor / sum(valor)*100),2))

})

dffinal <- do.call("rbind", dfList2)


dffinal<-rbind(dffinal)


dffinal$resposta <- as.factor(dffinal$resposta)



levels(dffinal$resposta)<-c("No hi ha connexió", 
                        "<= 2 Mb",
                        "2 - 4 Mb",
                        "4 - 8 Mb",
                        "10 - 50 Mb",
                        "50 - 100Mb", 
                        "> 100Mb" )

colfunc <- colorRampPalette(rev(c("#34408D", "#34A8DF"))) 


dffinal$eti<-dffinal$eti/100

write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_30.csv")

ggplot(data=dffinal, aes(x=(resposta), y=eti, fill=resposta,label=paste(round(eti*100,0), sep="")))+
  #geom_path()+
  geom_bar(stat="identity")+
  scale_y_continuous(breaks=c(.25) ,labels=percent)+
  coord_flip()+
  expand_limits(y=c(0,.5))+
  facet_grid(~ year)+
  
  geom_text(
    position = position_stack(vjust = 0.5),
    size=4.5)+
  labs(x="")+
  scale_fill_manual(values=(c(colfunc(length(unique(dffinal$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))


setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_30.png", scale = 1, dpi = 300, height =4, width = 12)


##### P_31 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  
  df<-df[,c('P_031','year')]
  
  DFP030 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_031'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP030<-DFP030[complete.cases(DFP030),] # QUITAMOS LOS CASOS DE NO RESPOSTA
  DFP030<-DFP030[!DFP030$resposta=='No aplica', ]   
  
  POP <- DFP030 %>% 
    group_by(year, categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2))
  
})

dffinal <- do.call("rbind", dfList2)


dffinal<-rbind(dffinal)


dffinal$resposta <- as.factor(dffinal$resposta)



levels(dffinal$resposta)<-c("No hi ha connexió", 
                            "<= 2 Mb",
                            "2 - 4 Mb",
                            "4 - 8 Mb",
                            "10 - 50 Mb",
                            "50 - 100Mb", 
                            "> 100Mb" )

colfunc <- colorRampPalette(rev(c("#34408D", "#34A8DF"))) 


dffinal$eti<-dffinal$eti/100

write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B1_P_31.csv")


ggplot(data=dffinal, aes(x=(resposta), y=eti, fill=resposta,label=paste(round(eti*100,0), sep="")))+
  #geom_path()+
  geom_bar(stat="identity")+
  scale_y_continuous(breaks=c(.25) ,labels=percent)+
  coord_flip()+
  expand_limits(y=c(0,.5))+
  facet_grid(~ year)+
  
  geom_text(
    position = position_stack(vjust = 0.5),
    size=4.5)+
  labs(x="")+
  scale_fill_manual(values=(c(colfunc(length(unique(dffinal$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))


setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_31.png", scale = 1, dpi = 300, height =4, width = 12)



##################
#### BLOQUE 2 ####
##################
##### P_034_0_2 ####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_034_0_2_1',
            'P_034_0_2_2',
            'P_034_0_2_3',
            'P_034_0_2_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_034_0_2_1',
                                'P_034_0_2_2',
                                'P_034_0_2_3',
                                'P_034_0_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-with(dffinal2, ifelse(resposta=="No/ No Aplica", "No aplica", resposta))
dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Anatomia patològica",
                              "Documentació clínica",
                              "Farmàcia",
                              "Laboratori")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_034_0_2.csv")


ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_034_0_2.png", scale = 1, dpi = 300, height =8, width = 12)

##### P_034_0_3 #####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_034_0_3_1',
            'P_034_0_3_2',
            'P_034_0_3_3',
            'P_034_0_3_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_034_0_3_1',
                                'P_034_0_3_2',
                                'P_034_0_3_3',
                                'P_034_0_3_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-with(dffinal2, ifelse(resposta=="Si parcialment (bàsicament consulta de resultats)", "Sí parcialment (bàsicament consulta de resultats)",
                                         ifelse(resposta=="Si totalment (petició i consulta de resultats)", "Sí totalment (petició i consulta de resultats)",resposta)))
dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Anatomia patològica",
                              "Documentació clínica",
                              "Farmàcia",
                              "Laboratori")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_034_0_3.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "bottom",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_034_0_3.png", scale = 1, dpi = 300, height =8, width = 12)


##### P_N_18 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_18_1',
            'P_N_18_2',
            'P_N_18_3', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_18_1',
                                'P_N_18_2',
                                'P_N_18_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c("3M", 
                            'ASSHO',
                            'IASIST',
                            'No disposo',
                            'Altres')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_18.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_18.png", scale = 1, dpi = 300, height =12, width = 12)




##### P_N_15 ####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_015_1',
            'P_N_015_2',
            'P_N_015_3', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_015_1',
                                'P_N_015_2',
                                'P_N_015_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c("Roche (Omega)",
                            'No disposo',
                            'Altres')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_15.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_15.png", scale = 1, dpi = 300, height =12, width = 12)



##### P_N_17 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_17_1',
            'P_N_17_2',
            'P_N_17_3',
            'P_N_17_4',
            'P_N_17_5',
             'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_17_1',
                                'P_N_17_2',
                                'P_N_17_3',
                                'P_N_17_4',
                                'P_N_17_5'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c("AGFA",
                            'DICOM',
                            'Siemens',
                            "UDIAT (Raïm)",
                            "Centricity")

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_17.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_17.png", scale = 1, dpi = 300, height =12, width = 12)

##### P_N_11 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_11_1',
            'P_N_11_2',
            'P_N_11_3',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_11_1',
                                'P_N_11_2',
                                'P_N_11_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)
levels(POP1$categoria) <- c("iSoft (PatWin)",
                            'VitroSP(NovoPath)',
                            'Desenvolupament propi')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_11.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_11.png", scale = 1, dpi = 300, height =12, width = 12)


##### P_N_12 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_12_1',
            'P_N_12_2',
            'P_N_12_3',
            'P_N_12_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_12_1',
                                'P_N_12_2',
                                'P_N_12_3',
                                'P_N_12_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)
levels(POP1$categoria) <- c("Dimensió Informàtica",
                            'Grifols',
                            'SAVAC',
                            "SAP")

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_12.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_12.png", scale = 1, dpi = 300, height =12, width = 12)


##### P_N_13 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_13_1',
            'P_N_13_2',
            'P_N_13_3',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_13_1',
                                'P_N_13_2',
                                'P_N_13_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)
levels(POP1$categoria) <- c("DadeBerhing (Servolab),",
                            'Izasa ( Starlims)',
                            'No disposo')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_13.csv")


ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 1)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  labs(y="Centres\n")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_N_13.png", scale = 1, dpi = 300, height =12, width = 12)

##### P_034_2  #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_034_2_1',
            'P_034_2_2',
            'P_034_2_3',
            'P_034_2_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_034_2_1',
                                'P_034_2_2',
                                'P_034_2_3',
                                'P_034_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)

POP4$categoria<-as.factor(POP4$categoria)
levels(POP4$categoria)
levels(POP4$categoria)<-c("Anatomia patològica",
                          "Documentació clínica",
                          "Farmàcia",
                          "Laboratori")

write.csv(POP4, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_034_2.csv")

ggplot(data=POP4, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("02_P_034_3.png", scale = 1, dpi = 300, height =8, width = 12)








##################
#### BLOQUE 3 ####
##################
### P_300 ####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_300',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_300'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("MitjanÇant el visor d'HC3", 
                             "Tinc alguna informació d'HC3 integrada a la ETC",
                             "Les dues respostes anteriors són correctes", 
                             "No tinc accés")



dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B3_P_300.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("03_P_300.png", scale = 1, dpi = 300, height =6, width = 12)



### P_372_2 ####
DATOS<-list(X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_372_2_1', 'P_372_2_2', 'P_372_2_3', 'P_372_2_4', 'P_372_2_5',
            'P_372_2_6', 'P_372_2_7',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_372_2_1', 'P_372_2_2', 'P_372_2_3', 'P_372_2_4', 'P_372_2_5',
                                'P_372_2_6', 'P_372_2_7'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)


dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("Sí",
                                                          "No",
                                                          "No aplica")))



dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")


levels(dffinal2$categoria) <- c("Internament\nHospitalització", 
                           "No internament\nEAIA",
                           "No internament\nHospital de dia",
                           "Equips de suport\nPADES",
                           "Equips de suport\nUFISS",
                           "Domicilis",
                           "CAPS")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B3_P_372_2.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("03_P_372_2.png", scale = 1, dpi = 300, height =8, width = 12)



### P_338 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_338','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_338'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("PCC (Pacient Crònic Complex)\nMACA (Model d'Atenció Crònica Avançada)")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B3_P_338.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]
# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = abs,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("03_P_338_abs.png", scale = 1, dpi = 300, height =6, width = 6)



### P_386_2 ####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_386_2_1', 'P_386_2_2','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_386_2_1', 'P_386_2_2'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="Sí",]
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Capacitat per comunicar-se\nmitjançant Web-Service",
                              "Sistema té una visió de\ntot el procés assistencial")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B4_P_386_2.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]
# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = DF4_long,aes(y = pop, x = year, colour=groupz)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(categoria ~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("03_P_386_2.png", scale = 1, dpi = 300, height =8, width = 12)



##################
####  BLOQUE 4 ###
##################
##### P_350 #####

P350<-X2014[,c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4', 'P_350_2_5',
               'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1', 'P_350_3_2',	
               'P_350_3_3','P_350_3_4',	  'P_350_3_5',	'P_350_3_6', 'P_350_3_7',	
               'P_350_3_8',	'P_350_4_1',  'P_350_4_2',	'P_350_4_3', 'P_350_4_4',	
               'P_350_4_5',	'P_350_4_6',  'P_350_4_7',  'P_350_4_8', 'P_350_5_1',	
               'P_350_5_2',	'P_350_5_3',	'P_350_5_4',  'P_350_5_5', 'P_350_5_6',	
               'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2', 'P_350_6_3',	
               'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7', 'P_350_6_8',	
               'P_350_7_1',	'P_350_7_2',	'P_350_7_3',	'P_350_7_4', 'P_350_7_5',	
               'P_350_7_6',	'P_350_7_7',	'P_350_7_8',	'P_350_8_1', 'P_350_8_2',	
               'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	'P_350_8_6', 'P_350_8_7',	
               'P_350_8_8')]

DFP350<- melt(P350,
              # ID variables - all the variables to keep but not split apart on
              # id.vars=c("COM2","YEAR", "secciones", "Total_ext", "Totalpop"),
              # The source columns
              measure.vars=c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4',	'P_350_2_5',
                             'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1',	'P_350_3_2',	'P_350_3_3',	
                             'P_350_3_4',	'P_350_3_5',	'P_350_3_6',	'P_350_3_7',	'P_350_3_8',	'P_350_4_1',	
                             'P_350_4_2',	'P_350_4_3',	'P_350_4_4',	'P_350_4_5',	'P_350_4_6',	'P_350_4_7',	
                             'P_350_4_8',	'P_350_5_1',	'P_350_5_2',	'P_350_5_3',	'P_350_5_4',	'P_350_5_5',	
                             'P_350_5_6',	'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2',	'P_350_6_3',	
                             'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7',	'P_350_6_8',	'P_350_7_1',	
                             'P_350_7_2',	'P_350_7_3',	'P_350_7_4',	'P_350_7_5',	'P_350_7_6',	'P_350_7_7',	
                             'P_350_7_8',	'P_350_8_1',	'P_350_8_2',	'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	
                             'P_350_8_6',	'P_350_8_7',	'P_350_8_8'),
              # Name of the destination column that will identify the original
              # column that the measurement came from
              variable.name="categoria",
              value.name="resposta"
)

POP <- DFP350 %>% group_by(categoria, resposta) %>%  summarise(valor = n())

POPS1<-POP[POP$resposta=="Si",]
POPS1<-POPS1[complete.cases(POPS1),]

POPS1$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 7)

POPS1$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST'), each=8)

#length(POPS1$aver) # Nombre de Centres que responen

POPS2<-POP[POP$resposta=="No",]
POPS2<-POPS2[complete.cases(POPS2),]

POPS2$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 7)


POPS1$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST'), each=8)


POPS1$Valorno<-POPS2$valor
POPS1$Valorprop<- with(POPS1, valor/(valor+Valorno)*100)


POPS1$aver2 <- factor(POPS1$aver2, 
                      levels=rev(c('Atenció Primaria',
                                   'Atenció Hospitalaria',
                                   'Atenció Sanitaria', 
                                   'Atenció a la Salut Mental', 
                                   'PPI externs', 
                                   'Proveïdors de Subministraments',
                                   'SST')))

POPS1$aver <- factor(POPS1$aver, 
                     levels=c('Informes',
                              'Resultats laboratori',
                              'Dades administratives',
                              'Resultats cardiologia',
                              'Altre documentació',
                              'Dades citacions',
                              'Gestió', 
                              'Serveis Socials'))
POPS1$year <-2014

POP2014<-POPS1

POP2014$cat<-with(POP2014, paste(aver2,aver,sep="-"))
levels(POPS1$aver2)


P350<-X2015[,c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4', 'P_350_2_5',
               'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1', 'P_350_3_2',	
               'P_350_3_3','P_350_3_4',	  'P_350_3_5',	'P_350_3_6', 'P_350_3_7',	
               'P_350_3_8',	'P_350_4_1',  'P_350_4_2',	'P_350_4_3', 'P_350_4_4',	
               'P_350_4_5',	'P_350_4_6',  'P_350_4_7',  'P_350_4_8', 'P_350_5_1',	
               'P_350_5_2',	'P_350_5_3',	'P_350_5_4',  'P_350_5_5', 'P_350_5_6',	
               'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2', 'P_350_6_3',	
               'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7', 'P_350_6_8',	
               'P_350_7_1',	'P_350_7_2',	'P_350_7_3',	'P_350_7_4', 'P_350_7_5',	
               'P_350_7_6',	'P_350_7_7',	'P_350_7_8',	'P_350_8_1', 'P_350_8_2',	
               'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	'P_350_8_6', 'P_350_8_7',	
               'P_350_8_8', 'P_350_9_1',	'P_350_9_2',	'P_350_9_3', 'P_350_9_4',	
               'P_350_9_5',	'P_350_9_6',	'P_350_9_7',	'P_350_9_8')]

DFP350<- melt(P350,
              # ID variables - all the variables to keep but not split apart on
              # id.vars=c("COM2","YEAR", "secciones", "Total_ext", "Totalpop"),
              # The source columns
              measure.vars=c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4',	'P_350_2_5',
                             'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1',	'P_350_3_2',	'P_350_3_3',	
                             'P_350_3_4',	'P_350_3_5',	'P_350_3_6',	'P_350_3_7',	'P_350_3_8',	'P_350_4_1',	
                             'P_350_4_2',	'P_350_4_3',	'P_350_4_4',	'P_350_4_5',	'P_350_4_6',	'P_350_4_7',	
                             'P_350_4_8',	'P_350_5_1',	'P_350_5_2',	'P_350_5_3',	'P_350_5_4',	'P_350_5_5',	
                             'P_350_5_6',	'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2',	'P_350_6_3',	
                             'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7',	'P_350_6_8',	'P_350_7_1',	
                             'P_350_7_2',	'P_350_7_3',	'P_350_7_4',	'P_350_7_5',	'P_350_7_6',	'P_350_7_7',	
                             'P_350_7_8',	'P_350_8_1',	'P_350_8_2',	'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	
                             'P_350_8_6',	'P_350_8_7',	'P_350_8_8','P_350_9_1',	'P_350_9_2',	'P_350_9_3', 'P_350_9_4',	
                             'P_350_9_5',	'P_350_9_6',	'P_350_9_7',	'P_350_9_8'),
              # Name of the destination column that will identify the original
              # column that the measurement came from
              variable.name="categoria",
              value.name="resposta"
)

POP <- DFP350 %>% group_by(categoria, resposta) %>%  summarise(valor = n())

POPS1<-POP[POP$resposta=="Sí",]
POPS1<-POPS1[complete.cases(POPS1),]

POPS1$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 8)

POPS1$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST',
                   'Tercer Sector'), each=8)

#length(POPS1$aver) # Nombre de Centres que responen

POPS2<-POP[POP$resposta=="No",]
POPS2<-POPS2[complete.cases(POPS2),]

POPS2$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 8)


POPS2$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST',
                   'Tercer Sector'), each=8)


POPS1$Valorno<-POPS2$valor
POPS1$Valorprop<- with(POPS1, valor/(valor+Valorno)*100)


POPS1$aver2 <- factor(POPS1$aver2, 
                      levels=rev(c('Atenció Primaria',
                                   'Atenció Hospitalaria',
                                   'Atenció Sanitaria', 
                                   'Atenció a la Salut Mental', 
                                   'PPI externs', 
                                   'Proveïdors de Subministraments',
                                   'SST',
                                   'Tercer Sector')))

POPS1$aver <- factor(POPS1$aver, 
                     levels=c('Informes',
                              'Resultats laboratori',
                              'Dades administratives',
                              'Resultats cardiologia',
                              'Altre documentació',
                              'Dades citacions',
                              'Gestió', 
                              'Serveis Socials'))

POPS1$year <-2015

POP2015<-POPS1

POP2015$cat<-with(POP2015, paste(aver2,aver,sep="-"))

levels(POPS1$aver2)

P350<-X2016[,c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4', 'P_350_2_5',
               'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1', 'P_350_3_2',	
               'P_350_3_3','P_350_3_4',	  'P_350_3_5',	'P_350_3_6', 'P_350_3_7',	
               'P_350_3_8',	'P_350_4_1',  'P_350_4_2',	'P_350_4_3', 'P_350_4_4',	
               'P_350_4_5',	'P_350_4_6',  'P_350_4_7',  'P_350_4_8', 'P_350_5_1',	
               'P_350_5_2',	'P_350_5_3',	'P_350_5_4',  'P_350_5_5', 'P_350_5_6',	
               'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2', 'P_350_6_3',	
               'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7', 'P_350_6_8',	
               'P_350_7_1',	'P_350_7_2',	'P_350_7_3',	'P_350_7_4', 'P_350_7_5',	
               'P_350_7_6',	'P_350_7_7',	'P_350_7_8',	'P_350_8_1', 'P_350_8_2',	
               'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	'P_350_8_6', 'P_350_8_7',	
               'P_350_8_8','P_350_9_1',	'P_350_9_2',	'P_350_9_3', 'P_350_9_4',	
               'P_350_9_5',	'P_350_9_6',	'P_350_9_7',	'P_350_9_8')]

DFP350<- melt(P350,
              # ID variables - all the variables to keep but not split apart on
              # id.vars=c("COM2","YEAR", "secciones", "Total_ext", "Totalpop"),
              # The source columns
              measure.vars=c('P_350_2_1',	'P_350_2_2',	'P_350_2_3',	'P_350_2_4',	'P_350_2_5',
                             'P_350_2_6',	'P_350_2_7',	'P_350_2_8',	'P_350_3_1',	'P_350_3_2',	'P_350_3_3',	
                             'P_350_3_4',	'P_350_3_5',	'P_350_3_6',	'P_350_3_7',	'P_350_3_8',	'P_350_4_1',	
                             'P_350_4_2',	'P_350_4_3',	'P_350_4_4',	'P_350_4_5',	'P_350_4_6',	'P_350_4_7',	
                             'P_350_4_8',	'P_350_5_1',	'P_350_5_2',	'P_350_5_3',	'P_350_5_4',	'P_350_5_5',	
                             'P_350_5_6',	'P_350_5_7',	'P_350_5_8',	'P_350_6_1',	'P_350_6_2',	'P_350_6_3',	
                             'P_350_6_4',	'P_350_6_5',	'P_350_6_6',	'P_350_6_7',	'P_350_6_8',	'P_350_7_1',	
                             'P_350_7_2',	'P_350_7_3',	'P_350_7_4',	'P_350_7_5',	'P_350_7_6',	'P_350_7_7',	
                             'P_350_7_8',	'P_350_8_1',	'P_350_8_2',	'P_350_8_3',	'P_350_8_4',	'P_350_8_5',	
                             'P_350_8_6',	'P_350_8_7',	'P_350_8_8','P_350_9_1',	'P_350_9_2',	'P_350_9_3', 'P_350_9_4',	
                             'P_350_9_5',	'P_350_9_6',	'P_350_9_7',	'P_350_9_8'),
              # Name of the destination column that will identify the original
              # column that the measurement came from
              variable.name="categoria",
              value.name="resposta"
)

POP <- DFP350 %>% group_by(categoria, resposta) %>%  summarise(valor = n())

POPS1<-POP[POP$resposta=="Sí",]
POPS1<-POPS1[complete.cases(POPS1),]

POPS1$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 8)

POPS1$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST',
                   'Tercer Sector'), each=8)

#length(POPS1$aver) # Nombre de Centres que responen

POPS2<-POP[POP$resposta=="No",]
POPS2<-POPS2[complete.cases(POPS2),]

POPS2$aver <-rep(c('Dades administratives',
                   'Dades citacions',
                   'Gestió',
                   'Informes',
                   'Resultats laboratori',
                   'Resultats cardiologia',
                   'Altre documentació', 
                   'Serveis Socials'), 8)


POPS2$aver2<-rep(c('Atenció Primaria',
                   'Atenció Hospitalaria',
                   'Atenció Sanitaria', 
                   'Atenció a la Salut Mental', 
                   'PPI externs', 
                   'Proveïdors de Subministraments',
                   'SST',
                   'Tercer Sector'), each=8)


POPS1$Valorno<-POPS2$valor
POPS1$Valorprop<- with(POPS1, valor/(valor+Valorno)*100)


POPS1$aver2 <- factor(POPS1$aver2, 
                      levels=rev(c('Atenció Primaria',
                                   'Atenció Hospitalaria',
                                   'Atenció Sanitaria', 
                                   'Atenció a la Salut Mental', 
                                   'PPI externs', 
                                   'Proveïdors de Subministraments',
                                   'SST',
                                   'Tercer Sector')))

POPS1$aver <- factor(POPS1$aver, 
                     levels=c('Informes',
                              'Resultats laboratori',
                              'Dades administratives',
                              'Resultats cardiologia',
                              'Altre documentació',
                              'Dades citacions',
                              'Gestió', 
                              'Serveis Socials'))

POPS1$year <-2016

POP2016<-POPS1

POP2016$cat<-with(POP2016, paste(aver2,aver,sep="-"))
levels(POPS1$aver2)


POP2014B<-rbind(POP2014,POP2015[c(57:64),] )
POP2014B$year<-2014

POP2014B[c(57:64),c(3,7)]<-0

sapply(POP2014B, class)
POP2014B$categoria<-as.factor(POP2014B$categoria)
POP2014B$aver2<-as.factor(POP2014B$aver2)


POP<-rbind(POP2014B,POP2015,POP2016)

POP$Valorprop<-POP$Valorprop/100
levels(POP$aver2)

POP$aver2 <- factor(POP$aver2, 
                    levels=rev(c('Atenció Primaria',
                                 'Atenció Hospitalaria',
                                 'Atenció Sanitaria', 
                                 'Atenció a la Salut Mental', 
                                 'PPI externs', 
                                 'Proveïdors de Subministraments',
                                 'SST',
                                 'Tercer Sector')))


write.csv(POP, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B4_P_350.csv")


#### NOTA: EL SIGUIENTE CHUNCK PRODUCE UN GRÁFICO DE BARRAS NO INCLUIDO EN EL INFORME DE ESTA SECCION

ggplot(data=POP, aes(x=(cat), y=Valorprop, fill=aver2,label=paste(round(Valorprop*100,0), sep="")))+
  #geom_path()+
  geom_bar(stat="identity")+
  scale_y_continuous(breaks=.4,labels=percent)+
  coord_flip()+
  expand_limits(y=c(0,.8))+
  facet_grid( ~ year)+
  
  geom_text(
    position = position_stack(vjust = 0.5),
    size=4.5)+
  labs(x="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none", 
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))
setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_350.png", scale = 1, dpi = 300, height =12, width = 12)



# NOTA: ajustar POP[POP$year==2014,] según el año que se quiera extraer. 

ggplot(data = POP[POP$year==2014,], aes(x = aver2, y = aver, label=Valorprop*100)) +
  geom_tile(aes(fill = Valorprop)) + 
  geom_text(aes(label = paste(valor,"\n",round(Valorprop*100, 0),"%",sep="")),size=3,colour = "white")+
  #scale_x_discrete(position = "top") +
  labs(#title="Institucions que responen afirmativament (expresat com percentatge)",
    subtitle="",
    x="",
    y="")+
  coord_flip()+
  #facet_wrap(~ resposta, ncol = 1)+
  scale_fill_gradient(name='Percentatge\n',low = "#34408D", high = "#E97420") +
  facet_grid( ~ year)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 90),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))


setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
#NOTA: CAMBIAR EL NOMBRE SEGÚN EL AÑO QUE SE QUIERA GUARDAR COMO IMAGEN 
ggsave("P_350_2014b.png", scale = 1, dpi = 300, height =8, width = 9)




##### P_120; P_121; P_124 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_120','P_121','P_124','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_120','P_121','P_124'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Motor d'integració o EAI", 
                              "Estàndard HL7",
                              "LOINC")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B4_P_120_121_124.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]
# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = abs,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("04_P_120_121_124_abs.png", scale = 1, dpi = 300, height =6, width = 6)


##### P_239 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_239',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_239'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí",
                             "No, però ho tenim previst",
                             "No")




dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B4_P_239.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("04_P_239.png", scale = 1, dpi = 300, height =6, width = 12)



##################
####  BLOQUE 5 ###
##################
#### P126 ####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_126',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_126'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Fa menys de 2 anys",
                             "Fa entre 2 i 4 anys",
                             "Fa més de 4 anys",
                             "Mai l’he fet")

dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("Mai l’he fet",
                                                          "Fa menys de 2 anys",
                                                          "Fa entre 2 i 4 anys",
                                                          "Fa més de 4 anys"
                                                          )))

dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B5_P_126.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("05_P_126.png", scale = 1, dpi = 300, height =6, width = 12)


##### P_037, P_494, P_141, P_142 ####


DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  
  df<-df[,c('P_037','P_141','P_142',"P_342", 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_037','P_141','P_142',"P_342"),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta") 
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),] 
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2)) 
  
  POP<-POP[POP$resposta=="1",]
  
}) 

dffinal <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Dispositius personals (BYOD)", 
                              "Còpies de seguretat fora de l'edifici del CPD",
                              "Proves cada 6 mesos de recuperació\n dels medis que emmagatzemen",
                              "Pla de contingència de negoci")


write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B5_P_037_141_142_342.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]
# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = rel,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,75))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  guides(colour  = guide_legend(nrow = 4))+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("04_P_037_141_142_rel.png", scale = 1, dpi = 300, height =6, width = 6)



##################
#### BLOQUE 6 ####
##################
#### P_032_2 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_032_2_1',
            'P_032_2_2',
            'P_032_2_3',
            'P_032_2_4',
            'P_032_2_5',
            'P_032_2_6',
            'P_032_2_7',
            'P_032_2_8',
            'P_032_2_9',
            'P_032_2_10',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_032_2_1',
                                'P_032_2_2',
                                'P_032_2_3',
                                'P_032_2_4',
                                'P_032_2_5',
                                'P_032_2_6',
                                'P_032_2_7',
                                'P_032_2_8',
                                'P_032_2_9',
                                'P_032_2_10'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)


dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("RRHH Contractació/Nòmina",
                              "RRHH Gestió de torns",
                              "RRHH Gestió de\n talent i competències",
                              "Econòmic/Financer",
                              "Comptabilitat analítica",
                              "Compres/Emmagatzematge",
                              "Datawarehouse",
                              "Quadre de comandament",
                              "Manteniment",
                              "Gestió de continguts")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

dffinalf$resposta<-as.factor(dffinalf$resposta)

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B6_P_032_2.csv")

ggplot(data = dffinalf,
       aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  facet_wrap(~ categoria, ncol = 1)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_032_2.png", scale = 1, dpi = 300, height =16, width = 12)

#### P_032_3 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_032_3_1',
            'P_032_3_2',
            'P_032_3_3',
            'P_032_3_4',
            'P_032_3_5',
            'P_032_3_6',
            'P_032_3_7',
            'P_032_3_8',
            'P_032_3_9',
            'P_032_3_10', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_032_3_1',
                                'P_032_3_2',
                                'P_032_3_3',
                                'P_032_3_4',
                                'P_032_3_5',
                                'P_032_3_6',
                                'P_032_3_7',
                                'P_032_3_8',
                                'P_032_3_9',
                                'P_032_3_10'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)

POP4$categoria<-as.factor(POP4$categoria)
levels(POP4$categoria)
levels(POP4$categoria)<-c("RRHH Contractació/Nòmina",
                          "RRHH Gestió de torns",
                          "RRHH Gestió de\n talent i competències",
                          "Econòmic/Financer",
                          "Comptabilitat analítica",
                          "Compres/Emmagatzematge",
                          "Datawarehouse",
                          "Quadre de comandament",
                          "Manteniment",
                          "Gestió de continguts")

write.csv(POP4, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_032_3.csv")

ggplot(data=POP4, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_032_3.png", scale = 1, dpi = 300, height =12, width = 12)


#### P_N_001 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_001_1',
            'P_N_001_2',
            'P_N_001_3',
            'P_N_001_4',
            'P_N_001_5',
            'P_N_001_6',
            'P_N_001_7',
            'P_N_001_8',
            'P_N_001_9',
            'P_N_001_10',
            'P_N_001_11',
            'P_N_001_12', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_001_1',
                                'P_N_001_2',
                                'P_N_001_3',
                                'P_N_001_4',
                                'P_N_001_5',
                                'P_N_001_6',
                                'P_N_001_7',
                                'P_N_001_8',
                                'P_N_001_9',
                                'P_N_001_10',
                                'P_N_001_11',
                                'P_N_001_12'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
 
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))

  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c("Costaisa", 
                            'CSS Agresso',
                            'Denario',
                            'Desenvolupament Propi\nNo Open Source (DP)',
                            'HumanSoft',
                            'Integrho',
                            'SAP',
                            'SAVAC',
                            'Sisinf',
                            'Meta4',
                            'Altres',
                            'No disposo')

POP1$categoria2 <- c("Productes que utilitza")
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_001.csv")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))


ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
 # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_001.png", scale = 1, dpi = 300, height =12, width = 12)


#### P_N_002 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_002_1',
            'P_N_002_2',
            'P_N_002_3',
            'P_N_002_4',
            'P_N_002_5',
            'P_N_002_6',
            'P_N_002_7',
            'P_N_002_8',
            'P_N_002_9',
            'P_N_002_10',
            'P_N_002_11',
            'P_N_002_12',
            'P_N_002_13', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_002_1',
                                'P_N_002_2',
                                'P_N_002_3',
                                'P_N_002_4',
                                'P_N_002_5',
                                'P_N_002_6',
                                'P_N_002_7',
                                'P_N_002_8',
                                'P_N_002_9',
                                'P_N_002_10',
                                'P_N_002_11',
                                'P_N_002_12',
                                'P_N_002_13'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c("CCS Agresso", 
                            'Contaplus',
                            'Costaisa',
                            'Desenvolupament Propi\nNo Open Source (DP)',
                            'Dimoni',
                            'Navision axapta',
                            'Navision dynamics',
                            'SAGES',
                            'SAP',
                            'SincrhosCOMP',
                            'UVCONTA',
                            'Altres',
                            'No disposo')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_002.csv")


ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_002.png", scale = 1, dpi = 300, height =16, width = 12)

#### P_N_003 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_003_1',
            'P_N_004_2',
            'P_N_005_3',
            'P_N_003_4',
            'P_N_003_5',
            'P_N_003_6',
            'P_N_003_7',
            'P_N_003_8',
            'P_N_003_9',
            'P_N_003_10',
            'P_N_003_11',
            'P_N_003_12', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_003_1',
                                'P_N_004_2',
                                'P_N_005_3',
                                'P_N_003_4',
                                'P_N_003_5',
                                'P_N_003_6',
                                'P_N_003_7',
                                'P_N_003_8',
                                'P_N_003_9',
                                'P_N_003_10',
                                'P_N_003_11',
                                'P_N_003_12'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c('Desenvolupament Propi\nNo Open Source (DP)',
                            "Desenvolupament Propi\nOpen Source (DPO)", 
                            'GOWINXD',
                            'Oracle',
                            'Pentaho',
                            'SAP-BO',
                            'SAS',
                            'Savac',
                            'Stacks',
                            'QlickView',
                            'Altres',
                            'No disposo')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_003.csv")


ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_003.png", scale = 1, dpi = 300, height =16, width = 12)

#### P_N_004 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_004_1',
            'P_N_004_2',
            'P_N_004_3',
            'P_N_004_4',
            'P_N_004_5',
            'P_N_004_6',
            'P_N_004_7',
            'P_N_004_8',
            'P_N_004_9',
            'P_N_004_10',
            'P_N_004_11', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_004_1',
                                'P_N_004_2',
                                'P_N_004_3',
                                'P_N_004_4',
                                'P_N_004_5',
                                'P_N_004_6',
                                'P_N_004_7',
                                'P_N_004_8',
                                'P_N_004_9',
                                'P_N_004_10',
                                'P_N_004_11'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c('Business Objects',
                            'CCS',
                            'Desenvolupament Propi\nNo Open Source (DP)',
                            "Desenvolupament Propi\nOpen Source (DPO)", 
                            'KHALIX',
                            'Qlik View',
                            'Oracle',
                            'Pentaho',
                            'SAS',
                            'Altres',
                            'No disposo')

POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_004.csv")


ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_004.png", scale = 1, dpi = 300, height =16, width = 12)

#### P_N_005 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_005_1',
            'P_N_005_2',
            'P_N_005_3',
            'P_N_005_4',
            'P_N_005_5',
            'P_N_005_6',
            'P_N_005_7',
            'P_N_005_8', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_005_1',
                                'P_N_005_2',
                                'P_N_005_3',
                                'P_N_005_4',
                                'P_N_005_5',
                                'P_N_005_6',
                                'P_N_005_7',
                                'P_N_005_8'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <-c(
  'Desenvolupament Propi\nNo Open Source (DP)',
  "Desenvolupament Propi\nOpen Source (DPO)", 
  'Helpdesk',
  'Prisma',
  'SAP',
  'SPI',
  'Altres',
  'No disposo')

POP1$categoria2 <- c("Productes que utilitza")

colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_005.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_005.png", scale = 1, dpi = 300, height =12, width = 12)

#### P_N_006 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_N_006_1',
            'P_N_006_2',
            'P_N_006_3',
            'P_N_006_4',
            'P_N_006_5', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_N_006_1',
                                'P_N_006_2',
                                'P_N_006_3',
                                'P_N_006_4',
                                'P_N_006_5'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  
  DFP028$resposta<-with(DFP028,ifelse(resposta=='X', 1, 0))
  
  DFP028[is.na(DFP028)]<-0
  
  POP <- DFP028 %>% group_by(categoria, resposta,year) %>%  summarise(valor = n())
  
  POP1<-POP[POP$resposta==1,]
  
})

POP1 <- do.call("rbind", dfList2)

levels(POP1$categoria) <- c('Drupal',
                            "Joomla", 
                            'Open CMS,',
                            'Desenvolupament prop',
                            'Altres',
                            'No disposo')


POP1$categoria2 <- c("Productes que utilitza")


colfunc <- colorRampPalette(c("#34408D", "#34A8DF"))
write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B2_P_N_006.csv")

ggplot(data=POP1, aes(x=year  , y=valor, color=categoria2, label=valor)) + 
  expand_limits(y=c(0,40))+
  geom_line(size=2)+
  # scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #coord_flip()+
  facet_wrap(~ categoria, ncol = 2)+
  scale_fill_manual(values=rev(c(colfunc(length(unique(POP1$categoria))))))+
  #labs(caption="Elaboraci?: TICSALUT\nDades: las que sean")+
  #geom_text(aes(y = valor+1),size=5, vjust = 0.5)+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=15,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("06_P_N_006.png", scale = 1, dpi = 300, height =12, width = 12)





##################
#### BLOQUE 7 ####
##################
#### P_248 #####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_248','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_248'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Suport a més organitzacions")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B7_P_248.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]
# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = rel,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  guides(colour  = guide_legend(ncol = 2))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("07_P_248_rel.png", scale = 1, dpi = 300, height =6, width = 6)

#### P_146 #####  #### MIRAR QUE PASA CON ESTA #####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_146_2_1',
            'P_146_2_2',
            'P_146_2_3','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_146_2_1',
                                'P_146_2_2',
                                'P_146_2_3'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),] # QUITAMOS LOS CASOS DE NO RESPOSTA
  DFP028<-DFP028[DFP028$resposta>0, ]
  

  
  
})

dffinal <- do.call("rbind", dfList2)


dfffinal<-rbind(dffinal)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Suport a més organitzacions")
write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B7_P_146_2.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

ggplot(data = rel,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  guides(colour  = guide_legend(ncol = 2))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("07_P_248_rel.png", scale = 1, dpi = 300, height =6, width = 6)








##################
##### BLOQUE 8 ###
##################
#### P_182, P_082,p_392 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_182','P_082','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_182','P_082'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)

#### P_392(15,16) #####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_392','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_392'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal354 <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal,dffinal354)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Diagnòstic per la Imatge", 
                              "Visualitzar imatges del PAC",
                              "P_392")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B8_P_182_82_392.csv")


DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

# NOTA: CAMBIAR data= rel o abs según se quiera el gráfico en terminos relativos o absolutos. 

ggplot(data = abs,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,75))+
  guides(colour  = guide_legend(ncol = 2))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("08_P_182_82_392_abs.png", scale = 1, dpi = 300, height =6, width = 6)

#### P_393 ####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_393',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_393'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí", "No", "No disposa")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")
dffinal2$categoria<-"Sistema de còpies de seguretat"

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B8_P_393.csv")


ggplot(data = dffinal2,
       aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  facet_wrap(~ categoria, ncol = 3)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("08_P_393.png", scale = 1, dpi = 300, height =6, width = 12)








##################
#### BLOQUE 9 ####
##################
#### P_074_2 ####
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_074_2_1', 'P_074_2_2', 'P_074_2_3', 'P_074_2_4', 'P_074_2_5',
            'P_074_2_6', 'P_074_2_7', 'P_074_2_8', 'P_074_2_9', 'P_074_2_10',
            'P_074_2_11',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_074_2_1', 'P_074_2_2', 'P_074_2_3', 'P_074_2_4', 'P_074_2_5',
                                'P_074_2_6', 'P_074_2_7', 'P_074_2_8', 'P_074_2_9', 'P_074_2_10',
                                'P_074_2_11'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Avui real",
                             "No",
                             "No aplica",
                             "Planificat en el futur")


levels(dffinal2$categoria)<-c("Accés a Internet dins\n l’entorn assistencial",
                              "Gestions per Internet",
                              "Autogestió dins del\n recinte assistencial",
                              "Recordatori de cites\n per SMS",
                              "Recordatori de cites\n per correu electrònic",
                              "Consulta assistencial\n per telèfon",
                              "Notificació de resultats\n per SMS",
                              "Promoció genèrica\n proactiva de la salut",
                              "Promoció personalitzada\n proactiva de la salut",
                              "Prevenció de la salut",
                              "Adherència al tractament")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B9_P_074.csv")


ggplot(data = dffinal2,aes(y = rel, x = year, colour=resposta)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  facet_wrap(~ categoria, ncol=3)+
  guides(colour  = guide_legend(ncol = 4))+
  labs(x="",
       y="Centres que responen sí\n")+
 # facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
# NOTA: CAMBIAR EL NOMBRE DEL ARCHIVO rel o abs según se quiera guardar el gráfico en terminos relativos o absolutos. 
ggsave("09_P_074_.png", scale = 1, dpi = 300, height =12, width = 12)






##################
#### BLOQUE 10 ###
##################
##### 488, de momento solo datos para 2016 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_488','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_488'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)


dffinal$categoria<-as.factor(dffinal$categoria)

levels(dffinal$categoria)<-c("Servei Web")

ggplot(data = dffinal,aes(y = eti, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_176_266_267_354.png", scale = 1, dpi = 300, height =6, width = 12)


#####  379_2 #####

DATOS<-list(X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_379_2_1', 'P_379_2_2', 'P_379_2_3', 'P_379_2_4', 'P_379_2_5',
            'P_379_2_6', 'P_379_2_7', 'P_379_2_8', 'P_379_2_9', 'P_379_2_10',
            'P_379_2_11', 'P_379_2_12', 'P_379_2_13', 'P_379_2_14',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_379_2_1', 'P_379_2_2', 'P_379_2_3', 'P_379_2_4', 'P_379_2_5',
                                'P_379_2_6', 'P_379_2_7', 'P_379_2_8', 'P_379_2_9', 'P_379_2_10',
                                'P_379_2_11', 'P_379_2_12', 'P_379_2_13', 'P_379_2_14'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Avui real",
                             "No",
                             "No aplica",
                             "Planificat en el futur")


levels(dffinal2$categoria)<-c("Programació de visites\n d'Atenció Primària",
                              "Canvi de metge\n de capçalera",
                              "Programació de visita\n de vacunació internacional",
                              "Recomanacions personalitzades\n i d'autocontrol de salut",
                              "Serveis per fer suggeriments\n i/o reclamacions",
                              "eConsulta",
                              "Promoció genèrica\n de la salut",
                              "Promoció personalitzada\n de la salut",
                              "Informació de prevenció",
                              "Adherència al tractament",
                              "Notificació de resultats",
                              "Prescripció apps pròpies",
                              "Prescripció apps\n de tercers",
                              "Altres usos")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")
write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B10_P_379_2.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ categoria, ncol = 3, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("10_P_379_2.png", scale = 1, dpi = 300, height =16, width = 12)


### P_435_2_1 DE MOMENTO SOLO HAY DATOS PARA 2016 #####
##################
#### BLOQUE 11 ###
##################
#### P_391 #######

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_391')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_391'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)
dffinal$year<-c(2015:2016)

write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B11_P_391.csv")

ggplot(data = dffinal,aes(y = eti, x = year, fill=resposta,label=paste(round(eti*100,0), sep=""))) +
  geom_bar(stat="identity",colour="black",width=.6)+
  geom_text(
    position = position_stack(vjust = 1.1),
    size=4.5)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,.25))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="none",
        legend.background = element_rect(fill="#F2F2F2", size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_391.png", scale = 1, dpi = 300, height =4, width = 12)


##################
#### BLOQUE 12 ###
##################
#### P_172, P_097,P108 #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_172')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_172'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)
dffinal$year<-c(2009:2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_097')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_097'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal21 <- do.call("rbind", dfList2)
dffinal21$year<-c(2009:2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_108')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_108'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal24 <- do.call("rbind", dfList2)
dffinal24$year<-c(2010:2016)

dfffinal<-rbind(dffinal,dffinal21,dffinal24)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Telemonitorització", 
                              "Teleconsulta local",
                              "Telediagnòstic/ interconsulta")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B12_P_97_172_189.csv")

ggplot(data = dfffinal,aes(y = eti, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_97_172_108.png", scale = 1, dpi = 300, height =6, width = 12)

#### P_98 ####


DATOS<-list(X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_098_2_1', 'P_098_2_2', 'P_098_2_3', 'P_098_2_4', 'P_098_2_5',
            'P_098_2_6', 'P_098_2_7', 'P_098_2_8', 'P_098_2_9', 'P_098_2_10',
            'P_098_2_11', 'P_098_2_12', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_098_2_1', 'P_098_2_2', 'P_098_2_3', 'P_098_2_4', 'P_098_2_5',
                                'P_098_2_6', 'P_098_2_7', 'P_098_2_8', 'P_098_2_9', 'P_098_2_10',
                                'P_098_2_11', 'P_098_2_12'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal <- do.call("rbind", dfList2)

dffinal$categoria<-as.factor(dffinal$categoria)
levels(dffinal$categoria)
levels(dffinal$categoria)<-c("Cardiologia (HTA...)",
                             "Dermatologia",
                             "Geriatria",
                             "Ginecologia",
                             "Medicina familiar",
                             "Nefrologia",
                             "Neurologia",
                             "Obstetrícia",
                             "Oncologia",
                             "Pediatria",
                             "Psiquiatria",
                             "Altres")


dffinal$abs<-dffinal$valor
dffinal$rel<-dffinal$eti*100
dffinal$ambos<-paste(paste(dffinal$rel,"%", sep=""), paste("(", dffinal$abs,")", sep=""),sep=" ")


write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B8_P_393.csv")


ggplot(data = dffinal,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2010,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_098.png", scale = 1, dpi = 300, height =10, width = 12)


#### P_100 ####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_100_2_7', 'P_100_2_8', 'P_100_2_9','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_100_2_7', 'P_100_2_8', 'P_100_2_9'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal <- do.call("rbind", dfList2)

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_100_2_1', 'P_100_2_2', 'P_100_2_3', 'P_100_2_4', 'P_100_2_5',
            'P_100_2_6', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_100_2_1', 'P_100_2_2', 'P_100_2_3', 'P_100_2_4', 'P_100_2_5',
                                'P_100_2_6'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)
dffinalf<-rbind(dffinal, dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Diabetis",
                              "HTA (Insuficiència cardíaca)",
                              "Insuficiència respiratoria",
                              "Rehabilitació",
                              "VIH",
                              "Nafres",
                              "Cures",
                              "Cures pal·liatives",
                              "Altres")

dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")


write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B12_P_100_2.csv")


ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2011,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_100.png", scale = 1, dpi = 300, height =10, width = 12)

#### P_102 ####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_102_2_11', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_102_2_11'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal <- do.call("rbind", dfList2)


DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_102_2_1', 'P_102_2_2', 'P_102_2_3', 'P_102_2_4', 'P_102_2_5',
            'P_102_2_6', 'P_102_2_7','P_102_2_8','P_102_2_9','P_102_2_10', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_102_2_1', 'P_102_2_2', 'P_102_2_3', 'P_102_2_4', 'P_102_2_5',
                                'P_102_2_6', 'P_102_2_7','P_102_2_8','P_102_2_9','P_102_2_10'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal$categoria<-as.character(dffinal$categoria)
dffinal2$categoria<-as.character(dffinal2$categoria)

dffinalf<-rbind(dffinal, dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Cardiologia (HTA...)",
                              "Cures pal·liatives",
                              "Altres",
                              "Dermatologia",
                              "Nefrologia",
                              "Neurologia",
                              "Oncologia",
                              "Pediatria",
                              "Psiquiatria",
                              "Urologia",
                              "Geriatria")



dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B12_P_102_2.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2011,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_102.png", scale = 1, dpi = 300, height =10, width = 12)



#### P_128 ####

P128<-X2010[,c('P_128')]

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_128', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_128'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$resposta<-as.factor(dffinalf$resposta)
levels(dffinalf$resposta)
levels(dffinalf$resposta)<-c("Sí com a proveédor del servei",
                             "Sí, com a receptor del servei",
                             "Sí,tant com a proveédor com a receptor del servei",
                             "No")

dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")


write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B12_P_128.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_128.png", scale = 1, dpi = 300, height =6, width = 12)




#### p_189 ####
P189<-X2009[,c('P_189')]

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_189', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_189'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$resposta<-as.factor(dffinalf$resposta)
levels(dffinalf$resposta)
levels(dffinalf$resposta)<-c("Sí com a proveédor del servei",
                             "Sí, com a receptor del servei",
                             "Sí,tant com a proveédor com a receptor del servei",
                             "No")

dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")


write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B12_P_189.csv")


ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("P_189.png", scale = 1, dpi = 300, height =6, width = 12)




##################
#### BLOQUE 13 ###
##################
#### P176T, P267T, P354(15,16), 266T, 438(16), 439(16) #####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_176','P_267','P_266','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_176','P_267','P_266'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)

#### P354(15,16) #####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_354','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_354'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal354 <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal,dffinal354)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Atenció Primària", 
                              "Accés ETC informe pre-alt",
                              "Centre CAUP",
                              "Desenvolupament cloud SISCAT")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_176_266_267_354.csv")


ggplot(data = dfffinal,aes(y = eti, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_176_266_267_354.png", scale = 1, dpi = 300, height =6, width = 12)



#### P_043_0_2 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_043_0_2_1',
            'P_043_0_2_2',
            'P_043_0_2_3',
            'P_043_0_2_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_043_0_2_1',
                                'P_043_0_2_2',
                                'P_043_0_2_3',
                                'P_043_0_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_043_0_2.csv")


ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2010,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_043_0_2.png", scale = 1, dpi = 300, height =8, width = 12)



#### P_043_0_3 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_043_0_3_1',
            'P_043_0_3_2',
            'P_043_0_3_3',
            'P_043_0_3_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_043_0_3_1',
                                'P_043_0_3_2',
                                'P_043_0_3_3',
                                'P_043_0_3_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)

POP4$categoria<-as.factor(POP4$categoria)
levels(POP4$categoria)
levels(POP4$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria")

write.csv(POP4, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_043_0_3.csv")


ggplot(data=POP4, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
      legend.title = element_blank(),
      legend.text =element_text( vjust=0.5, size=15,colour="black"),
      legend.position= "right",# c(.8,.1),
      legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
      axis.title.x = element_blank(),
      axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
      axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
      axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
      strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
      strip.background = element_rect(fill="#F2F2F2"),
      panel.grid.major=element_line(colour="#E6E6E6"),
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
      panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_043_0_3.png", scale = 1, dpi = 300, height =8, width = 12)


#### P_251_2 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_251_2_1',
            'P_251_2_2',
            'P_251_2_3',
            'P_251_2_4',
            'P_251_2_5',
            'P_251_2_6',
            'P_251_2_7',
            'P_251_2_8',
            'P_251_2_9',
            'P_251_2_10',
            'P_251_2_11',
            'P_251_2_12',
            'P_251_2_13',
            'P_251_2_14',
            'P_251_2_15',
            'P_251_2_16',
            'P_251_2_17',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_251_2_1',
                                  'P_251_2_2',
                                  'P_251_2_3',
                                  'P_251_2_4',
                                  'P_251_2_5',
                                  'P_251_2_6',
                                  'P_251_2_7',
                                  'P_251_2_8',
                                  'P_251_2_9',
                                  'P_251_2_10',
                                  'P_251_2_11',
                                  'P_251_2_12',
                                  'P_251_2_13',
                                  'P_251_2_14',
                                  'P_251_2_15',
                                  'P_251_2_16',
                                  'P_251_2_17'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinal2$categoria<-as.factor(dffinal2$categoria)
levels(dffinal2$categoria)
levels(dffinal2$categoria)<-c("Interacció entre medicaments",
                              "Interacció de fàrmacs amb\n al·lèrgies de pacients",
                              "Interacció de fàrmacs amb\n resultats de laboratori",
                              "Dosi màxima",
                              "Interacció per gènere\n del pacient",
                              "Interacció per edat del pacient",
                              "Interacció de fàrmacs\n amb patologia",
                              "Dubte de la interacció\n amb alimentació",
                              "Duplicitats terapèutiques\n (redundància)",
                              "Detecció d’utilització\n inadequada de medicació",
                              "Manca d’adherència al tractament",
                              "Alertes a nivell de proves",
                              "Duplicitat en el petitori\n d’ordres de laboratori",
                              "Duplicitat en el petitori\n d’ordres de radiologia",
                              "Detecció d’utilització\n inadequada de proves",
                              "Alertes a nivell de\n procés/resultat",
                              "Resultats crítics del laboratori")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")


write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_251_2.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2011,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_251_3.png", scale = 1, dpi = 300, height =12, width = 12)



#### P_252_2 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_252_2_1',
            'P_252_2_2',
            'P_252_2_3',
            'P_252_2_4',
            'P_252_2_5',
            'P_252_2_6',
            'P_252_2_7',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_252_2_1',
                                'P_252_2_2',
                                'P_252_2_3',
                                'P_252_2_4',
                                'P_252_2_5',
                                'P_252_2_6',
                                'P_252_2_7'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinal2$categoria<-as.factor(dffinal2$categoria)
levels(dffinal2$categoria)
levels(dffinal2$categoria)<-c("Proposta de petitori de proves",
                              "Recordatori proves pendents",
                              "Recordatori de vacunes",
                              "Recordatori de test d’scrinning",
                              "Proposta d’opcions\n de tractament a seguir",
                              "Ajuda al diagnòstic\n per la imatge",
                              "Proposta d’accions\n personalitzades segons situació\ndel pacient")


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")


write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_252_2.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=eti*100)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2011,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_252_2.png", scale = 1, dpi = 300, height =8, width = 12)



#### P_274 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_274',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_274'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí, en temps real",
                              "Sí, en menys de 24h",
                              "Sí, en més de 24h",
                              "No la tenim integrada")



dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_274.csv")

ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_274.png", scale = 1, dpi = 300, height =6, width = 12)

#### P_275 ######


DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_275',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_275'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Si, només de consulta",
                             "Si, per consulta i modificació",
                             "No disposen")



dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")


write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B13_P_275.csv")



ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("13_P_275.png", scale = 1, dpi = 300, height =6, width = 12)






##################
##### BLOQUE 14 ## 
##################
#### P_177, P_276, P_358 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_177','P_276','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_177','P_276'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)

#### P354(15,16) #####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_358','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_358'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal354 <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal,dffinal354)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Atenció Hospitalària", 
                              "Informació clínica a la HCE",
                              "Solució Web ePAT")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_177_276_358.csv")


ggplot(data = dfffinal,aes(y = eti, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position=c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_177_276_358.png", scale = 1, dpi = 300, height =6, width = 12)


#### P_047_2 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_047_2_1',
            'P_047_2_2',
            'P_047_2_3',
            'P_047_2_4',
            'P_047_2_5', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_047_2_1',
                                'P_047_2_2',
                                'P_047_2_3',
                                'P_047_2_4',
                                'P_047_2_5'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria",
                              "Datawarehouse d’ús clínic")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_047_2.csv")



ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_047_0_2.png", scale = 1, dpi = 300, height =8, width = 12)


#### P_047_3 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_047_3_1',
            'P_047_3_2',
            'P_047_3_3',
            'P_047_3_4',
            'P_047_3_5', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_047_3_1',
                                'P_047_3_2',
                                'P_047_3_3',
                                'P_047_3_4',
                                'P_047_3_5'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)

POP4$categoria<-as.factor(POP4$categoria)
levels(POP4$categoria)
levels(POP4$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria",
                              "Datawarehouse d’ús clínic")


write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_047_3.csv")


ggplot(data=POP4, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_047_0_3.png", scale = 1, dpi = 300, height =8, width = 12)



#### P_054 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_054',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_054'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("0%",
                             "0.1-24.9%",
                             "25-49.5%",
                             "50-99.9%",
                             "100%")

dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("0%",
                                                      "0.1-24.9%",
                                                      "25-49.5%",
                                                      "50-99.9%",
                                                      "100%")))


dffinal2$resposta2<-with(dffinal2, ifelse(resposta=="0%", 0,
                                   ifelse(resposta=="0.1-24.9%", 12.5, 
                                   ifelse(resposta=="25-49.5%", 37.5,
                                   ifelse(resposta=="50-99.9%", 75,
                                   ifelse(resposta=="100%", 100,0))))))



dffinal2$resposta3<-with(dffinal2,resposta2*valor)

POP3 <- dffinal2 %>% 
  group_by(year) %>%  
  mutate(eti3 =sum(resposta3)/sum(valor))

POP4<-POP3 %>% distinct(year, eti3)

dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_054.csv")

ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_054.png", scale = 1, dpi = 300, height =6, width = 12)


### version rara pedida por francesc

ggplot(data=POP4, 
       aes(x=year, y=eti3/100)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  #facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(0,1))+
  scale_y_continuous(labels=percent)+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Percentatge de documentació\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_054b.png", scale = 1, dpi = 300, height =6, width = 12)






#### P_283 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_283',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_283'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("No disposo",
                             "No, però ho tinc previst",
                             "Sí, web ePAT 3.5 o inferior",
                             "Sí, web ePAT 4.0 o superior",
                             "Sí, altres")

dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("No disposo",
                                                          "No, però ho tinc previst",
                                                          "Sí, web ePAT 3.5 o inferior",
                                                          "Sí, web ePAT 4.0 o superior",
                                                          "Sí, altres")))



dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_283.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_283.png", scale = 1, dpi = 300, height =6, width = 12)

#### P_281 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_281',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_281'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí, en temps real",
                             "Sí, en menys de 24h",
                             "Sí, en més de 24h",
                             "No la tenim integrada")

#dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("Sí, en temps real",
 #                                                         "Sí, en menys de 24h",
  #                                                        "Sí, en més de 24h",
   #                                                       "No la tenim integrada")))


dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_281.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_281.png", scale = 1, dpi = 300, height =6, width = 12)

#### P_282 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_282',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_282'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí, només de consulta",
                             "Sí, per consulta i modificació",
                             "No disposen")

dffinal2$resposta<-factor(dffinal2$resposta, levels=rev(c("Sí, només de consulta",
                                                          "Sí, per consulta i modificació",
                                                          "No disposen")))

dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B14_P_282.csv")


ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("14_P_282.png", scale = 1, dpi = 300, height =6, width = 12)






##################
##### BLOQUE 15 ##
##################
### P_178','P_289','P_296', ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_178','P_289','P_296','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_178','P_289','P_296'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)

### P354(15,16) #####

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_399','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_399'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal354 <- do.call("rbind", dfList2)

dfffinal<-rbind(dffinal,dffinal354)

dfffinal$categoria<-as.factor(dfffinal$categoria)

levels(dfffinal$categoria)<-c("Atenció Sociosanitària", 
                              "EAR",
                              "PADES",
                              "HCE")

write.csv(dfffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_178_289_296_399.csv")

DF4_long<-gather(dfffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)

abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

ggplot(data = rel,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
  scale_y_continuous(breaks=c(0,25,50, 75,100))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,100))+
  labs(x="",
       y="Centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_178_289_296_399_rel.png", scale = 1, dpi = 300, height =6, width = 6)

### P_066_2 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_066_2_1',
            'P_066_2_2',
            'P_066_2_3',
            'P_066_2_4',
             'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_066_2_1',
                                'P_066_2_2',
                                'P_066_2_3',
                                'P_066_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_066_2_5',
            'P_066_2_6',
            'P_066_2_7',
            'P_066_2_8',
            'P_066_2_9',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_066_2_5',
                                'P_066_2_6',
                                'P_066_2_7',
                                'P_066_2_8',
                                'P_066_2_9'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})

dffinal3 <- do.call("rbind", dfList2)

#levels(dffinal3$categoria)<-c(levels(dffinal2$categoria),levels(dffinal3$categoria))
#levels(dffinal2$categoria)<-c(levels(dffinal2$categoria),levels(dffinal3$categoria))

dffinal2$categoria<-as.character(dffinal2$categoria)
dffinal3$categoria<-as.character(dffinal3$categoria)

dffinalf<-rbind(dffinal2,dffinal3)


dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria",
                              "Auxiliar d'infermeria",
                              "Treball social",
                              "Psicologia-neuropsicologia",
                              "Fisioteràpia",
                              "Altres professionals")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

dffinalf$resposta<-as.factor(dffinalf$resposta)
write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_066_2.csv")

ggplot(data = dffinalf,
       aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  facet_wrap(~ categoria, ncol = 3)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_066_2.png", scale = 1, dpi = 300, height =8, width = 12)



### P_066_3 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_066_3_1',
            'P_066_3_2',
            'P_066_3_3',
            'P_066_3_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_066_3_1',
                                'P_066_3_2',
                                'P_066_3_3',
                                'P_066_3_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)


DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_066_3_5',
            'P_066_3_6',
            'P_066_3_7',
            'P_066_3_8',
            'P_066_3_9',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_066_3_5',
                                'P_066_3_6',
                                'P_066_3_7',
                                'P_066_3_8',
                                'P_066_3_9'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP1 <- do.call("rbind", dfList2)

POP5<-POP1 %>% distinct(categoria, eti3)

POP5$categoria<-as.factor(POP5$categoria)

POP6<-rbind(POP4,POP5)
POP6$categoria<-as.factor(POP6$categoria)

levels(POP6$categoria)
levels(POP6$categoria)<-c("HIS: part administrativa",
                          "Estació mèdica",
                          "Estació d’infermeria",
                          "Pla de cures d’infermeria",
                          "Auxiliar d'infermeria",
                          "Treball social",
                          "Psicologia-neuropsicologia",
                          "Fisioteràpia",
                          "Altres professionals")


write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_066_3.csv")


ggplot(data=POP6, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 3)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_066_3.png", scale = 1, dpi = 300, height =8, width = 12)



### P_365  ######

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_365_2_1',
            'P_365_2_2',
            'P_365_2_3',
            'P_365_2_4', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_365_2_1',
                                'P_365_2_2',
                                'P_365_2_3',
                                'P_365_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Per atendre a pacients\ni/o als seus cuidadors",
                              "Per millorar la comunicació\n entre els professionals",
                              "Per millorar la comunicació\n entre la institució i \nels professionals",
                              "Per millorar la comunicació\n pacient - professional")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_365_2.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_365.png", scale = 1, dpi = 300, height =8, width = 12)


### P_385  ######

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_385_2_1',
            'P_385_2_2',
            'P_385_2_3',
            'P_385_2_4',
            'P_385_2_5',
            'P_385_2_6',
            'P_385_2_7',
            'P_385_2_8',
            'P_385_2_9',
            'P_385_2_10',
            'P_385_3_1',
            'P_385_3_2',
            'P_385_3_3',
            'P_385_3_4',
            'P_385_3_5',
            'P_385_3_6',
            'P_385_3_7',
            'P_385_3_8',
            'P_385_3_9',
            'P_385_3_10', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_385_2_1',
                                'P_385_2_2',
                                'P_385_2_3',
                                'P_385_2_4',
                                'P_385_2_5',
                                'P_385_2_6',
                                'P_385_2_7',
                                'P_385_2_8',
                                'P_385_2_9',
                                'P_385_2_10',
                                'P_385_3_1',
                                'P_385_3_2',
                                'P_385_3_3',
                                'P_385_3_4',
                                'P_385_3_5',
                                'P_385_3_6',
                                'P_385_3_7',
                                'P_385_3_8',
                                'P_385_3_9',
                                'P_385_3_10'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  
  DFP028$TI<-substr(DFP028$categoria,7,7)
  DFP028$TIPO<-with(DFP028,ifelse(TI=="2", "Paper", "Electrònic"))
  
  #DFP028$TIPO <-rep(c("Paper", 'Electr?nic'), each=1200)
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  
  POP <- DFP028 %>%  
    group_by(TIPO,year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Ambulatòria\nAmbulatori (EAIA)",
                              "Ambulatòria\nAmbulatori\n(Hospital de dia)",
                              "Equips de suport\nEquips de suport \n(Domicili - PADES)",
                              "Equips de suport\nEquips de suport \n(H. Aguts - UFISS)",
                              "Internament\nHospital sociosanitari \n(convalescència/\npostaguts)",
                              "Internament\nHospital sociosanitari \n(subaguts)",
                              "Internament\nHospital sociosanitari \n(mitja estada\n polivalent)",
                              "Internament\nHospital sociosanitari \n(cures pal·liatives)",
                              "Internament\nHospital sociosanitari \n(llarga estada)",
                              "Internament\nHospital sociosanitari \n(psicogeriatria de\nmitja estada)",
                              "Ambulatòria\nAmbulatori (EAIA)",
                              "Ambulatòria\nAmbulatori (Hospital de dia)",
                              "Equips de suport\nEquips de suport \n(Domicili - PADES)",
                              "Equips de suport\nEquips de suport \n(H. Aguts - UFISS)",
                              "Internament\nHospital sociosanitari \n(convalescència/\npostaguts)",
                              "Internament\nHospital sociosanitari \n(subaguts)",
                              "Internament\nHospital sociosanitari \n(mitja estada\npolivalent)",
                              "Internament\nHospital sociosanitari \n(cures pal·liatives)",
                              "Internament\nHospital sociosanitari \n(llarga estada)",
                              "Internament\nHospital sociosanitari \n(psicogeriatria\nde mitja estada)")



dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")


write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_385.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(TIPO~ categoria, ncol = 4)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_385.png", scale = 1, dpi = 300, height =18, width = 12)



### P_364_2  ######

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_364_2_1',
            'P_364_2_2',
            'P_364_2_3',
            'P_364_2_4',
            'P_364_2_5',
            'P_364_2_6', 'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_364_2_1',
                                'P_364_2_2',
                                'P_364_2_3',
                                'P_364_2_4',
                                'P_364_2_5',
                                'P_364_2_6'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028$resposta<-with(DFP028, ifelse(resposta=="Sí,Dispositius mòbils", "Dispositius mòbils", resposta))
  DFP028$resposta<-with(DFP028, ifelse(resposta=="Sí,Altres", "Altres", resposta))
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)


dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("Estimulació\ncognitiva", 
                              'Persones amb malalties neurològiques\nque cursen amb discapacitat',
                              'Persones amb malalties\navan?ades i terminals', 
                              'Educació sanitària\nPacient',
                              'Educació sanitària\nCuidador',
                              'Altres')


dffinalf$resposta<-as.factor(dffinalf$resposta)
levels(dffinalf$resposta)

dffinalf$resposta<-factor(dffinalf$resposta, levels=c("Sí",
                                                      "No",
                                                      "Dispositius mòbils",
                                                      "Online",
                                                      "Altres"))


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_364_2.csv")


ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_364.png", scale = 1, dpi = 300, height =8, width = 12)



### P_297 ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)


dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_297',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_297'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

dffinal2$resposta<-as.factor(dffinal2$resposta)
levels(dffinal2$resposta)
levels(dffinal2$resposta)<-c("Sí, en temps real",
                             "Sí, en menys de 24h",
                             "Sí, en més de 24h",
                             "No la tenim integrada")




dffinal2$abs<-dffinal2$valor
dffinal2$rel<-dffinal2$eti*100
dffinal2$ambos<-paste(paste(dffinal2$rel,"%", sep=""), paste("(", dffinal2$abs,")", sep=""),sep=" ")


write.csv(dffinal2, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B15_P_297.csv")

ggplot(data = dffinal2,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  #geom_line(size=2)+
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  #facet_wrap(~ categoria, ncol = 3)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="right",
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("15_P_297.png", scale = 1, dpi = 300, height =6, width = 12)


##################
#### BLOQUE 16 ###
##################
### P_181, P_292  ######
DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_181','P_292','year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_181','P_292'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028<-DFP028[complete.cases(DFP028),]
  
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(Absolutos = n()) %>%
    mutate(Relativos = round((Absolutos / sum(Absolutos)*100),2))
  
  POP<-POP[POP$resposta=="1",]
  
})

dffinal <- do.call("rbind", dfList2)

dffinal$categoria<-as.factor(dffinal$categoria)

levels(dffinal$categoria)<-c("Atenció Salut Mental i Adiccions", 
                              "HCE")

write.csv(dffinal, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B16_P_181_292.csv")

DF4_long<-gather(dffinal, groupz, pop, c(Absolutos:Relativos), factor_key=TRUE)


abs<-DF4_long[DF4_long$groupz=="Absolutos",]
rel<-DF4_long[DF4_long$groupz=="Relativos",]

ggplot(data = abs,aes(y = pop, x = year, colour=categoria)) +
  geom_line(size=2)+
   scale_y_continuous(breaks = c(0,25,50,75))+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,75))+
  labs(x="", 
       y="Nombre de centres que responen sí\n")+
  facet_wrap(~ groupz, ncol = 2, scales="free")+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position="bottom", #c(.85,.87),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("16_P_181_292_abs.png", scale = 1, dpi = 300, height =6, width = 6)



### P_69_2  ######

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_069_2_1',
            'P_069_2_2',
            'P_069_2_3',
            'P_069_2_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_069_2_1',
                                'P_069_2_2',
                                'P_069_2_3',
                                'P_069_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})


dffinal2 <- do.call("rbind", dfList2)

DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_069_2_1',
            'P_069_2_2',
            'P_069_2_3',
            'P_069_2_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_069_2_1',
                                'P_069_2_2',
                                'P_069_2_3',
                                'P_069_2_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  
  DFP028<-DFP028[!DFP028$resposta=="Producte",]
  #DFP028$resposta<-as.numeric(DFP028$resposta)
  POP <- DFP028 %>%  
    group_by(year,categoria, resposta) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)),2))
  
  
})

dffinal3 <- do.call("rbind", dfList2)

dffinalf<-rbind(dffinal2)

dffinalf$categoria<-as.factor(dffinalf$categoria)
levels(dffinalf$categoria)
levels(dffinalf$categoria)<-c("HIS: part administrativa",
                              "Estació mèdica",
                              "Estació d’infermeria",
                              "Pla de cures d’infermeria")


dffinalf$abs<-dffinalf$valor
dffinalf$rel<-dffinalf$eti*100
dffinalf$ambos<-paste(paste(dffinalf$rel,"%", sep=""), paste("(", dffinalf$abs,")", sep=""),sep=" ")

write.csv(dffinalf, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B16_P_069_2.csv")

ggplot(data = dffinalf,aes(y = eti, x = year, fill=resposta, label=ambos)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(
    position = position_stack(vjust = .5),
    size=4.5)+
  # geom_bar(aes(fill = resposta))
  scale_y_continuous(labels=percent)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  expand_limits(y=c(0,1))+
  labs(y="")+
  facet_wrap(~ categoria, ncol = 2)+
  #scale_fill_manual(values=(c(colfunc(length(unique(POP$resposta))))))+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("16_P_069_2.png", scale = 1, dpi = 300, height =8, width = 12)

### P_69_3 ####

DATOS<-list(X2009,X2010,X2011,X2012,X2013,X2014,X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_069_3_1',
            'P_069_3_2',
            'P_069_3_3',
            'P_069_3_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_069_3_1',
                                'P_069_3_2',
                                'P_069_3_3',
                                'P_069_3_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP <- do.call("rbind", dfList2)

POP4<-POP %>% distinct(categoria, eti3)


DATOS<-list(X2015,X2016)

dfList2 <- lapply(DATOS, function(df) {
  df<-df[,c('P_069_3_1',
            'P_069_3_2',
            'P_069_3_3',
            'P_069_3_4',
            'year')]
  
  DFP028 <- melt(df,    #### CAMBIAMOS EL DF PARA QUE APAREZCA EN FORMATO LONG, COMO LO NECISTAMOS PARA LOS GRÁFICSO 
                 # The source columns
                 measure.vars=c('P_069_3_1',
                                'P_069_3_2',
                                'P_069_3_3',
                                'P_069_3_4'),
                 # Name of the destination column that will identify the original
                 # column that the measurement came from
                 variable.name="categoria",
                 value.name="resposta")
  
  # AGRUPAMOS EL DF POR CATEGORIA DE RESPUESTA, SUMAMOS CUANTAS RESPESTAS HAY EN CADA CATEGORIA Y 
  # CREAMOS UNA NUEVA COLUMNA QUE REFLEJE EL PESO PORCENTUAL DE CADA UNA UNA DE LAS CATEGORIAS DE RESPUESTA
  #DFP028$resposta<-with(DFP028,ifelse(resposta=="Si", "Sí",resposta ))
  
  DFP028<-DFP028[complete.cases(DFP028),]
  #DFP028<-DFP028[!DFP028$resposta=="Producte",]
  DFP028<-DFP028[!DFP028$resposta=='No aplica', ]
  DFP028$resposta<-as.character(DFP028$resposta)
  
  POP <- DFP028 %>%  
    group_by(categoria,resposta, year) %>%  
    summarise(valor = n()) %>%
    mutate(eti = round((valor / sum(valor)*100),2),
           eti2 =as.numeric(resposta)*valor)
  POP3 <- POP %>% 
    group_by(categoria,year) %>%  
    mutate(eti3 =sum(eti2)/sum(valor))
  
})


POP1 <- do.call("rbind", dfList2)

POP5<-POP1 %>% distinct(categoria, eti3)

POP5$categoria<-as.factor(POP5$categoria)

POP6<-rbind(POP4)
POP6$categoria<-as.factor(POP6$categoria)

levels(POP6$categoria)
levels(POP6$categoria)<-c("HIS: part administrativa",
                          "Estació mèdica",
                          "Estació d’infermeria",
                          "Pla de cures d’infermeria")

write.csv(POP1, file="C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON\\CSV\\B16_P_069_3.csv")

ggplot(data=POP6, 
       aes(x=year, y=eti3)) + 
  geom_path(size=2)+
  scale_x_continuous(breaks=seq(2009,2016,1))+
  facet_wrap(~ categoria, ncol = 2)+
  expand_limits(y=c(1,4))+
  #scale_fill_manual(values=c('#e97420','#34A8DF','#3485C3','#3462A8','#34408D','#BDBDBD'))+
  labs(y="Grau de satisfacció\n")+
  theme(plot.title = element_text(lineheight=5.6, size=15, face="bold"),
        legend.title = element_blank(),
        legend.text =element_text( vjust=0.5, size=15,colour="black"),
        legend.position= "right",# c(.8,.1),
        legend.background = element_rect(fill=NA, size=.5, linetype="dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=12,colour="black"),
        axis.text.x  = element_text( vjust=0.5, size=12,colour="black",angle = 0),
        strip.text = element_text(size = 15, colour = "#424242", angle = 0, face = "bold"),
        strip.background = element_rect(fill="#F2F2F2"),
        panel.grid.major=element_line(colour="#E6E6E6"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background =  element_rect(fill = "#FFFFFF" ,colour = "#FFFFFF"),
        panel.background =element_rect(fill ="#FFFFFF",colour = "#FFFFFF"))

setwd("C:\\Users\\jgaleano\\Desktop\\TICSALUT\\IMAGENES_LON")
ggsave("16_P_069_3.png", scale = 1, dpi = 300, height =8, width = 12)


