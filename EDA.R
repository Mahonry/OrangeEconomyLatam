# EDA scatter plot mtcars

plot(mtcars$mpg ~ mtcars$cyl, xlab = "Cilindros", ylab = "Millas por galon",
     main = "Relacion cilindros y millas por galon")

plot(mtcars$mpg ~ mtcars$hp, xlab = "HP", ylab = "Millas por galon",
     main = "Relacion HP y millas por galon")

#EDA orangeec
plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab = "Inversion en educacion (%PIB)", ylab = "Desempleo",
     main = "Relacion inversion en educacion y desempleo")

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,
     xlab = "Aporte Economia Naranja al PIB(%)", ylab = "PIB per capita",
     main = "Relacion Economia Naranja y PIB per capita")

# Histogramas mtcars

hist(mtcars$hp, geom = "histrogram", xlab = "HP", main = "Carros segun HP")


ggplot(mtcars, aes(x=hp)) + geom_histogram() + labs(x = "HP", y = "Frecuencia", title = 
    "HP's") + theme(legend.position = "none")+ theme(panel.background = element_blank(),
    panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=cyl)) + geom_histogram(binwidth = 30) + labs(x = "HP", y = "Frecuencia", title = 
      "HP's") + theme(legend.position = "none") + theme(panel.background = element_blank(),
      panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=mtcars,aes(x=hp), fill = "blue", color="red", binwidth = 20) + 
  labs(x = "HP", y = "Frecuencia", title = "HP's") + xlim(c(80,280)) +theme(panel.background = element_blank(),
  panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

#Histogramas Economia Naranja
ggplot()+geom_histogram(data=orangeec,aes(x=GDP.PC), fill = "blue", color="red", binwidth = 2000) + 
  labs(x = "PIB per capita", y = "Cantidad de paises", title = "PIB per capita paises LATAM")   
+theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,aes(x=Creat.Ind...GDP), fill = "blue", color="red", binwidth = 1) + 
  labs(x = "Aporte economia naranja al PIB (%)", y = "Cantidad de paises", title = "Contribucion Economia Naraja en LATAM")   
+theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

#

ggplot()+geom_histogram(data=orangeec,aes(x=Internet.penetration...population), fill = "blue", color ="yellow", binwidth = 5) + 
  labs(x = "Penetración internet (%) población", y = "Cantidad de paises", title = "Penetración internet en países LATAM")   
+theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())


#BoxPlot mtcars

boxplot(mtcars$hp, ylab = "HP's", main = "Caballos de fuerza mtcars")

ggplot(mtcars, aes(x=as.factor(cyl),y=hp, fill=cyl)) + geom_boxplot(alpha = 0.6) + labs(x = "Cilindros", y = "HP's",title = "HP's según cilindro") +
theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=as.factor(am),y=mpg,fill=am))+geom_boxplot()+
  labs(x = "Tipo de caja", y ="Millas por galón",title="Millas por galón según tipo de caja") +
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

#
mtcars$am <- factor(mtcars$am, levels = c(0,1), labels = c("Automatic","Manual"))

#BoxPlot orangeec

economy <- mean(orangeec$GDP.PC)
orangeec <- orangeec %>% mutate(Strong_economy = ifelse(GDP.PC<economy,"Por debajo promedio PIB per capita",
                                                        "Sobre promedio PIB per capita"))

ggplot(orangeec, aes(x=Strong_economy, y = Creat.Ind...GDP,fill=Strong_economy))+
  geom_boxplot(alpha = 0.4) + labs(x="Tipo de pais", y="Aporte economia naranja al PIB", title = 
                                     "Aporte economia naranja en PIB paises LATAM con alto y bajo PIB per capita") + 
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())
#
ggplot(orangeec, aes(x=Strong_economy, y = Internet.penetration...population,fill=Strong_economy))+
  geom_boxplot(alpha = 0.4) + labs(x="Tipo de pais", y="Penetracion de Internet (%)", title = 
                                     "Penetracion de internet en paises LATAM con alto y bajo PIB per capita") + 
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())


# Scatter plot
ggplot(mtcars,aes(hp,mpg)) + geom_point() + labs(x="HP's", y="Millas por galon", title = "Relacion HP's y millas por galos")+
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())


ggplot(mtcars,aes(wt,hp)) + geom_point() + labs(x="Peso (miles de lbs)", y="HP's", title = "Relacion peso  y potencia")+
  theme(panel.background = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

  
ggplot(mtcars,aes(hp,qsec))+geom_point(aes(color = am, size = cyl)) +
  labs(x="HP's", y = "Tiempo en 1/4 de milla", title = "Caballos-velocidad según cilindraje y tipo de caja")

#Scatter plot en economia naranja 

ggplot(orangeec,aes(Internet.penetration...population,Creat.Ind...GDP))+geom_point(aes(color = factor(Strong_economy), size = GDP.Growth..)) +
  labs(x="Penetracion de internet", y = "Aporte Orange Economy al PIB ", title = "Internet y aporte orange economy según economia y crecimiento PIB")

ggplot(orangeec,aes(Education.invest...GDP,Unemployment))+geom_point(aes(color = factor(Strong_economy), size = X..pop.below.poverty.line)) +
  labs(x="Inversion en educacion (%) PIB", y = "Desempleo", title = "Inversion en educacion y desempleo según linea de pobreza y crecimiento PIB")


my_graph <- ggplot(orangeec, aes(Internet.penetration...population, Creat.Ind...GDP, label 
                                 = row.names(orangeec))) + 
  geom_point() + 
  labs(x = "Penetracion Internet", y = "Aporte economia naranja", title = "Penetracion Internet y aporte economia naranja")

p = ggplotly(my_graph)
p

#Correlaciones

cor(mtcars[,2:5])

pairs(orangeec[,2:6])
pairs(orangeec[,5:10])

#
newdata <- subset(orangeec, select = c(5,6,10,11,12,13))
pairs(newdata)

#Verificando correlaciones
cor(orangeec[,2:6])

#
cor(orangeec[,2:6], use = "complete.obs")
cor(orangeec[,5:10], use = "complete.obs")
cor(newdata, use = "complete.obs")

sd <- sd(orangeec$Internet.penetration...population)
mean <- mean(orangeec$Internet.penetration...population)


coef <- (sd/mean)*100
coef

# 

mean <- mean(orangeec$Creat.Ind...GDP, na.rm = TRUE)
mean
sd <- sd(orangeec$Creat.Ind...GDP, na.rm = TRUE)
sd
coef <- (sd/mean)*100
coef


# Ajustando datos para mejores visualizaciones

orangeec <- orangeec %>% 
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5, "2.5 % o mas", "menos de 2.5 %"))

orangeec <- orangeec %>% 
  mutate(Anaranjados = ifelse(Creat.Ind...GDP > 2.5,
                              "Mas anaranjados",
                              "Menos anaranjados"))

#Ranking

orangeec %>% 
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>% filter(Country %in% c("Mexico","Panama","Paraguay","Argentina","Colombia","Brazil"))

TopNaranjas %>%
  arrange(desc(Creat.Ind...GDP))


ggplot(TopNaranjas, aes(x = Internet.penetration...population, y = Services...GDP, size = GDP.PC))+
  geom_point() +
  facet_wrap(~Country)



ggplot(TopNaranjas, aes(x = Education.invest...GDP, y = Creat.Ind...GDP, size = Unemployment))+
  geom_point() +
  facet_wrap(~Country)

myColors <- brewer.pal(6,"Reds")

ggplot(TopNaranjas, aes(x = Internet.penetration...population, y = GDP.PC, fill = Creat.Ind...GDP))+
  geom_tile() +
  facet_wrap(~Country) +
  scale_fill_gradientn(colors = myColors)






  
