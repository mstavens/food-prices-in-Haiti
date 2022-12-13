rm(list = ls())
#setwd("C:/Users/Bierhoff Theolien/Desktop/ML Pavilus/Projet ML")
# liste des packages dont on a besoin
# liste des packages dont on a besoin
required_pkgs <- c("ggmap", "rgdal", "rgeos", "maptools",'tis',
                   "dplyr", "tidyr", "tmap", "mapview","psych", "GGally",
                   "tidyverse", "sf", "sp","leaflet", 'tigris','stargazer',
                   "lubridate","Hmisc", "magrittr", 'rworldmap', 'rugarch')

# Liste de l'ensemble des packages dÃ©jÃ  installÃ©s sur notre R 
installed_pkgs <- installed.packages()

# Liste des packages manquants par rapport Ã  notre liste de packages requis pour le travail
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed_pkgs[, 1])]
missing_pkgs # Afficher la liste des packages manquants

# TÃ©lÃ©charger les packages manquants si il y en a
if (length(missing_pkgs) == 0 ) {
  message("No missing dependencies. You are good to go!")
} else {
  message("We are going to install the list of those packages:\n", missing_pkgs) 
  install.packages(missing_pkgs)
  
  message("All packages installed. You are good to go!")
  
}


# Il faut maintenant appeler les packages
lapply(required_pkgs, library, character.only = TRUE) # load the required packages
library(fpp2)
library(crossval)
library(forecast)
library(dplyr)
library(tidyverse)
library(tseries)
library(TTR)
library(cluster)
library(factoextra)



########################################################################################################################
###################### PremiÃ¨re Ã©tape finie. Il faut maintenant importer la base de donnÃ©es ############################ 
########################################################################################################################
# Importation de la base de donnÃ©es sur les prix des commoditÃ©s.
mydata <- read.csv("C:/Users/Mano/Documents/CTPEA/4e/ML PROJECT/HFP.csv") 
#The database is in the repository, please download it and modify the previous line according to the path of the database you downloaded.
#Ch_lieu <- read_csv("Haiti_GPS.csv")
str(mydata)
mydata <- mydata[-1,] # On enlÃ¨ve la  premiÃ¨re ligne
mydata <- mydata %>%
  select(c(date, admin1, admin2, latitude,
           longitude, category, commodity,
           unit, price))


#mydata <- st_as_sf(mydata, coords = c("longitude", "latitude"))
#Ch_lieu <- st_as_sf(Ch_lieu, coords = c("longitude", "latitude"))


str(mydata) # Les variables ne sont pas dans la forme souhaitÃ©e, nous devons les transformer. 
#str(Ch_lieu)

# Transformation des donnÃ©es dans le type qu'il faut
mydata$latitude <- as.double(mydata$latitude)
mydata$longitude <- as.double(mydata$longitude)

mydata$date <- as.Date(mydata$date)
#mydata$date <- as.Date(mydata$date, "%y/%m/%d")
mydata$admin1 <- as.factor(mydata$admin1)
mydata$admin2 <- as.factor(mydata$admin2)
mydata$category <- as.factor(mydata$category)
mydata$commodity <- as.factor(mydata$commodity)
mydata$unit <- as.factor(mydata$unit)
mydata$price <- as.numeric(mydata$price)

glimpse(mydata)

levels(mydata$admin1) # Cette ligne nous dit les diffÃ©rents niveaux qui existent dans la variable de type "Factor" admin1

# Traduire certains mots en Francais

levels(mydata$admin1)[levels(mydata$admin1) == "North"] <- "Nord"
levels(mydata$admin1)[levels(mydata$admin1) == "North-East"] <- "Nord-Est"
levels(mydata$admin1)[levels(mydata$admin1) == "North-West"] <- "Nord-Ouest"
levels(mydata$admin1)[levels(mydata$admin1) == "West"] <- "Ouest"
levels(mydata$admin1)[levels(mydata$admin1) == "South"] <- "Sud"
levels(mydata$admin1)[levels(mydata$admin1) == "South-East"] <- "Sud-Est"

levels(mydata$admin1) # On vÃ©rifie que la traduction a Ã©tÃ© faite (Voir rÃ©sultat au niveau de la console)

# On se rend compte aussi que le nom des variables n'est pas en franÃ§ais, on va donc les changer:
# Changer le nom de certaines colonnes

names(mydata)[1] <- 'Date' # On assigne le nom de Date Ã  la premiÃ¨re colonne
names(mydata)[2] <- 'Departement'
names(mydata)[3] <- 'Ville'
names(mydata)[4] <- 'latitude'
names(mydata)[7] <- 'commodity'
names(mydata)[9] <- 'Prix'

attach(mydata)
levels(category)
table(category)
levels(unit)
table((mydata$unit))

# On constate qui y a dans la colonne Unit les niveaux: marmite et pound.  on va uniformiser les prix. Pound sera considere.

for (i in 1:length(mydata$unit)){
  if ( mydata$unit[i] == "Marmite") {
    mydata$Prix[i] <- mydata$Prix[i]/2.7/2.20462  
  }
  else {
    mydata$Prix[i] <- mydata$Prix[i]
  }
}

levels(mydata$unit)[levels(mydata$unit) == "Marmite"] <- "Pound"
table(mydata$unit)


################## Visualisation ###################################### 


# Jetons un coup d'oeil sur la carte
map_market_1 <- mapview(mydata, xcol = "longitude", ycol = "latitude", zcol = "Prix", crs = 4269, grid = F)
map_market_1




### Faisons un travail par commoditÃ©

#### Riz local


riz_local <- mydata %>%
  subset(commodity == "Rice (local)")


summary(riz_local)

riz_importe <- mydata %>%
  subset(commodity == "Rice (tchako)")
subset(subset(mais_importe,Departement=="Sud"), Prix==max(Prix))
mais_local <- mydata %>%
  subset(commodity == "Maize meal (local)")

mais_importe <- mydata %>%
  subset(commodity == "Maize meal (imported)")

farine <- mydata %>%
  subset(commodity == "Wheat flour (imported)")

sorgho <- mydata %>%
  subset(commodity == "Sorghum")

plot(riz_importe %>%
       select(c(Departement, Prix)),
     main = "Prix du riz importé par département")

plot(mais_local %>%
       select(c(Departement, Prix)),
     main = "Prix du maïs local par département")

plot(mais_importe %>%
       select(c(Departement, Prix)),
     main = "Prix du maïs importé par département")


plot(farine %>%
       select(c(Departement, Prix)),
     main = "Prix de la farine par département")

plot(sorgho %>%
       select(c(Departement, Prix)),
     main = "Prix du sorgho par département")

plot(riz_local %>%
       select(c(Departement, Prix)),
     main = "Prix du riz local par Departement")































plot(riz_local %>%
       subset(Departement == "Artibonite") %>%
       select(c(Date, Prix)), type = "l", 
     main = "Prix du riz local pour Artibonite")


summary(riz_local %>%
          subset(Departement == "Artibonite") %>%
          select(c(Date, Prix)))




plot(riz_local %>%
       subset(Departement == "Artibonite") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans l'Artibonite" )
plot(riz_local %>%
       subset(Departement == "Ouest") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans l'Ouest" )

plot(riz_local %>%
       subset(Departement == "Sud") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans le Sud" )

plot(riz_local %>%
       subset(Departement == "Nord") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans le Nord" )

plot(riz_local %>%
       subset(Departement == "Grande'Anse") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans la Grande Anse" )

plot(riz_local %>%
       subset(Departement == "Ouest") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans l'Ouest" )

plot(riz_local %>%
       subset(Departement == "Centre") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans le Centre" )

plot(riz_local %>%
       subset(Departement == "Nord-Est") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans le Nord-Est" )

plot(riz_local %>%
       subset(Departement == "Sud-Est") %>%
       select(c(Date, Prix)), type = "l",
     main = "Prix du riz local dans le Sud-Est" )

Legende <- riz_local$Departement
diri <- qplot(riz_local$Date, riz_local$Prix , colour = Legende)+
  geom_line() +
  labs(title = paste0("Evolution des prix du riz local vendu par livre de janvier 2015 Ã  avril 2022"),
       
       x = 'Date',
       y = 'HTG',
       caption = 'SOurce: WFP') +
  
  theme_bw(base_family = "TT Times New Roman")
diri
# Vu que le graphe est trÃ¨s condensÃ©, on va prendre des groupes de Departements

# Riz local vendu par le Grand Nord (Artibonite, Nord, Nord-Est et Nord-Ouest)
riz_local_GN <- mydata %>%
  subset(commodity == "Rice (local)") %>%
  subset(Departement == "Artibonite"| Departement == "Nord-Est" |
           Departement == "Nord-Ouest" | Departement == "Nord")

Grand_Nord <- riz_local_GN$Departement

diri_GN <- qplot(riz_local_GN$Date, riz_local_GN$Prix , colour = Grand_Nord)+
  geom_line() +
  labs(title = paste0("Evolution des prix du riz local vendu par livre dans 4 Departements du Grand Nord de janvier 2015 Ã  avril 2022"),
       
       x = 'Date',
       y = 'HTG',
       caption = 'Source: WFP') +
  
  theme_bw(base_family = "TT Times New Roman")
diri_GN

# Riz local vendu par marmite dans l'Ouest, le Centre et le Sud-Est 
riz_local_COSE <- mydata %>%
  subset(commodity == "Rice (local)") %>%
  subset(Departement == "Ouest"| Departement == "Sud-Est" |
           Departement == "Centre") 

COSE <- riz_local_COSE$Departement

diri_COSE <- qplot(riz_local_COSE$Date, riz_local_COSE$Prix , colour = COSE)+
  geom_line() +
  labs(title = paste0("Evolution des prix du riz local vendu par livre de janvier 2015 Ã  avril 2022"),
       
       x = 'Date',
       y = 'HTG',
       caption = 'Source: WFP') +
  
  theme_bw(base_family = "TT Times New Roman")
diri_COSE



ggplot(data = riz_local_GN, # the input data
       aes(x = longitude, y = latitude, fill = Prix, group = Departement)) # define variables


ggplot(data = riz_local_GN, # the input data
       aes(x = longitude, y = latitude, fill = Prix, group = commodity)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ Prix ) + # one plot per time slice
  scale_fill_gradient2(low = "blue", mid = "grey", high = "red", # colors
                       midpoint = 5, name = "Prix\n(HTG)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks

#mapview(riz_local_GN, xcol = "longitude", ycol = "latitude", zcol = "Prix" ,crs = 4269, grid = F)


#################### Faisons une analyse gÃ©rale ############################################### 
######################### Faisons la mÃªme demarche mais pour l'ensemble du pays. ###########################


data_wide_Pays <- spread(mydata, commodity, Prix) # transformer la matrice de la forme longue (long) Ã  la forme large (wide)

data_wide_Pays_Plus <- as.data.frame(na.omit(data_wide_Pays %>%
                                               # subset(unit == "Marmitte") %>%
                                               select(c(Date, unit,`Maize meal (local)`,
                                                        #`Maize meal (imported)`,
                                                        `Rice (tchako)`,
                                                        
                                                        # `Rice (imported)`,
                                                        `Rice (local)`,
                                                        `Wheat flour (imported)`, Ville))))

#mydata_wide <- spread(data_wide_Pays_Plus, Date, `Maize meal (local)`, `Wheat flour (imported)` ,`Rice (tchako)`)

names(data_wide_Pays_Plus)[2] <- "Unite"
names(data_wide_Pays_Plus)[3] <- "mais_local"
names(data_wide_Pays_Plus)[4] <- "riz_tchako"
names(data_wide_Pays_Plus)[5] <- "riz_local"
names(data_wide_Pays_Plus)[6] <- "farine_import"

# Calculcons les prix moyens par jour pour ces produits alimentaires
data_average_Price <- data_wide_Pays_Plus %>%
  select(-Unite) %>%
  mutate(date = floor_date(Date)) %>%
  group_by(date) %>%
  summarize_all(mean)

str(data_average_Price)




### Detecter les correlations entre les commodites

pairs(data_average_Price)

pairs.panels(data_average_Price)

ggpairs(data_average_Price)

### Croissance des prix des produits
mais_local_cr <- diff(log(data_average_Price$mais_local))*100
riz_tchako_cr <- diff(log(data_average_Price$riz_tchako))*100

riz_local_cr <-diff(log(data_average_Price$riz_local))*100
farine_import_cr <-diff(log(data_average_Price$farine_import))*100


date_Pays_plus <- as.data.frame(data_average_Price$Date[-1])

data_Pays_cr <- cbind(date_Pays_plus, mais_local_cr, riz_tchako_cr, riz_local_cr, farine_import_cr)

names(data_Pays_cr)[1] <- "Date"

########## Jetons un coup d'oeil sur la volatilitÃ© des prix de certains produits ###############################
par(mfrow = c(2,2))
plot(data_Pays_cr$Date, data_Pays_cr$mais_local_cr, type = "l", 
     main = "Croissance moyenne du prix du maÃ¯s", 
     ylab ="HTG",
     xlab = "date")

plot(data_Pays_cr$Date, data_Pays_cr$riz_tchako_cr, type = "l",
     main = "Croissance moyenne du prix du riz Tchako", 
     ylab ="HTG",
     xlab = "date")

plot(data_Pays_cr$Date, data_Pays_cr$riz_local_cr, type = "l",
     main = "Croissance moyenne du prix du riz local", 
     ylab ="HTG",
     xlab = "date")

plot(data_Pays_cr$Date, data_Pays_cr$farine_import_cr, type = "l",
     main = "Croissance moyenne du prix de la farine importÃ©e", 
     ylab ="HTG",
     xlab = "date")

pairs(data_Pays_cr)
pairs.panels(data_Pays_cr)
ggpairs(data_Pays_cr)

# Detectons si y a auto correlation
acf(mais_local_cr)
acf(riz_tchako_cr)
acf(riz_local_cr)
acf(farine_import_cr)

pacf(mais_local_cr)
pacf(riz_tchako_cr)
pacf(riz_local_cr)
pacf(farine_import_cr)

# MOdel ARIMA pour la croissance du prix du mais
garchSpec_mais <- ugarchspec(variance.model=list(model="sGARCH",
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,1)),
                             #methods(),
                             distribution.model="norm")
garchFit_mais <- ugarchfit(spec=garchSpec_mais, mais_local_cr)
garchFit_mais 
plot(garchFit_mais, which = 'all')





# MOdel ARIMA pour la croissance du prix du riz
garchSpec_riz <- ugarchspec(variance.model=list(model="sGARCH",
                                                garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(1,1)),
                            #methods(),
                            distribution.model="norm")
garchFit_riz <- ugarchfit(spec=garchSpec_riz, riz_local_cr)
garchFit_riz 
plot(garchFit_riz, which = 'all')


# MOdel ARIMA pour la croissance du prix du riz tchako
garchSpec_riz_tchako <- ugarchspec(variance.model=list(model="sGARCH",
                                                garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(1,1)),
                            #methods(),
                            distribution.model="norm")
garchFit_riz_tchako <- ugarchfit(spec=garchSpec_riz_tchako, riz_tchako_cr)
garchFit_riz_tchako 
plot(garchFit_riz_tchako, which = 'all')

# MOdel ARIMA pour la croissance du prix de la farine
garchSpec_farine <- ugarchspec(variance.model=list(model="sGARCH",
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(1,1)),
                               #methods(),
                               distribution.model="norm")
garchFit_farine <- ugarchfit(spec=garchSpec_farine, farine_import_cr)
garchFit_farine
plot(garchFit_farine, which = 'all')

# Construisons une base de donnÃ©es avec les volatilitÃ©s des prix de ces 3 produits

vola.mais    <- ts(garchFit_mais@fit$sigma^2)
vola.riz <- ts(garchFit_riz@fit$sigma^2)
vola.riz_tchako <- ts(garchFit_riz_tchako@fit$sigma^2)
vola.farine <- ts(garchFit_farine@fit$sigma^2)

vola.Plus <- as.data.frame(cbind(vola.mais, vola.riz, vola.riz_tchako ,vola.farine))


pairs(vola.Plus)
pairs.panels(vola.Plus)
ggpairs(vola.Plus)


######################### Faisons une analyse par Departement ################################ 

# Artibonite   

Food_Artibonite <- mydata %>%
  subset(Departement == "Artibonite")


glimpse(Food_Artibonite)

Food_Artibonite$Prix <- as.numeric(Food_Artibonite$Prix)
data_wide <- spread(Food_Artibonite, commodity, Prix) # transformer la matrice de la forme longue (long) Ã  la forme large (wide)

data_wide_Plus <- as.data.frame(na.omit(data_wide %>%
                                          select(c(Date,`Maize meal (local)`, `Rice (tchako)`, `Wheat flour (imported)`, Ville))))


names(data_wide_Plus)[2] <- "mais"
names(data_wide_Plus)[3] <- "riz_importe"
names(data_wide_Plus)[4] <- "farine_importe"

#mais_cr <- growth.rate(data_wide_Plus$mais, lag = 1, simple = T)


str(data_wide_Plus)


### Detecter les correlations entre les commodites

commodity_pairs <- pairs(data_wide_Plus[c("mais", "riz_importe", "farine_importe")])
commodity_pairs
commodity_pairs.panel <- pairs.panels(data_wide_Plus[c("mais", "riz_importe", "farine_importe")]);commodity_pairs.panel


mais_cr <- diff(log(data_wide_Plus$mais))*100
riz_cr <- diff(log(data_wide_Plus$riz_importe))*100
farine_cr <-diff(log(data_wide_Plus$farine_importe))*100

date_plus <- as.data.frame(data_wide_Plus$Date[-2])

data_cr <- cbind(date_plus, mais_cr, farine_cr, riz_cr)

commodity_pairs_cr <- pairs(data_cr[c("mais_cr", "riz_cr", "farine_cr")])
commodity_pairs.panel_cr <- pairs.panels(data_cr[c("mais_cr", "riz_cr", "farine_cr")])



########## Jetons un coup d'oeil sur la volatilitÃ© des prix de certains produits ###############################
plot(mais_cr, type = "l")
plot(riz_cr, type = "l")
plot(farine_cr, type = "l")



# MOdel ARIMA pour la croissance du prix du mais
garchSpec_mais <- ugarchspec(variance.model=list(model="sGARCH",
                                                 garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(1,2)),
                             #methods(),
                             distribution.model="norm")
garchFit_mais <- ugarchfit(spec=garchSpec_mais, mais_cr)
garchFit_mais 
plot(garchFit_mais, which = 'all')


# MOdel ARIMA pour la croissance du prix du riz
garchSpec_riz <- ugarchspec(variance.model=list(model="sGARCH",
                                                garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(1,2)),
                            #methods(),
                            distribution.model="norm")
garchFit_riz <- ugarchfit(spec=garchSpec_riz, riz_cr)
garchFit_riz 
plot(garchFit_riz, which = 'all')


# MOdel ARIMA pour la croissance du prix de la farine
garchSpec_farine <- ugarchspec(variance.model=list(model="sGARCH",
                                                   garchOrder=c(1,1)),
                               mean.model=list(armaOrder=c(1,2)),
                               #methods(),
                               distribution.model="norm")
garchFit_farine <- ugarchfit(spec=garchSpec_farine, farine_cr)
garchFit_farine
plot(garchFit_farine, which = 'all')

# Construisons une base de donnÃ©es avec les volatilitÃ©s des prix de ces 3 produits

vola.mais    <- ts(garchFit_mais@fit$sigma^2)
vola.riz <- ts(garchFit_riz@fit$sigma^2)
vola.farine <- ts(garchFit_farine@fit$sigma^2)

vola.Plus <- as.data.frame(cbind(vola.mais, vola.riz, vola.farine))


pairs(vola.Plus)
pairs.panels(vola.Plus)
ggpairs(vola.Plus)



########## Jetons un coup d'oeil sur la volatilitÃ© des prix de certains produits ###############################
plot(mais_local_cr, type = "l")
plot(riz_tchako_cr, type = "l")
plot(riz_local_cr, type = "l")
plot(farine_import_cr, type = "l")

acf(mais_local_cr)
acf(riz_tchako_cr)
acf(riz_local_cr)
acf(farine_import_cr)



# MOdel ARIMA pour la croissance du prix du mais
garchSpec_mais_Pays <- ugarchspec(variance.model=list(model="sGARCH",
                                                      garchOrder=c(1,1)),
                                  mean.model=list(armaOrder=c(1,2)),
                                  #methods(),
                                  distribution.model="norm")
garchFit_mais_Pays <- ugarchfit(spec=garchSpec_mais_Pays, mais_local_cr)
garchFit_mais_Pays 
plot(garchFit_mais_Pays, which = 'all')


# MOdel ARIMA pour la croissance du prix du riz tchako
garchSpec_riz_tchako <- ugarchspec(variance.model=list(model="sGARCH",
                                                       garchOrder=c(1,1)),
                                   mean.model=list(armaOrder=c(1,2)),
                                   #methods(),
                                   distribution.model="norm")
garchFit_riz_tchako <- ugarchfit(spec=garchSpec_riz_tchako, riz_tchako_cr)
garchFit_riz_tchako
plot(garchFit_riz_tchako, which = 'all')

# MOdel ARIMA pour la croissance du prix du riz tchako
garchSpec_riz_local <- ugarchspec(variance.model=list(model="sGARCH",
                                                      garchOrder=c(1,1)),
                                  mean.model=list(armaOrder=c(1,2)),
                                  #methods(),
                                  distribution.model="norm")
garchFit_riz_local <- ugarchfit(spec=garchSpec_riz_local, riz_local_cr)
garchFit_riz_local
plot(garchFit_riz_local, which = 'all')

# MOdel ARIMA pour la croissance du prix de la farine
garchSpec_farine_Pays <- ugarchspec(variance.model=list(model="sGARCH",
                                                        garchOrder=c(1,1)),
                                    mean.model=list(armaOrder=c(1,2)),
                                    #methods(),
                                    distribution.model="norm")
garchFit_farine_Pays <- ugarchfit(spec=garchSpec_farine_Pays, farine_import_cr)
garchFit_farine_Pays
plot(garchFit_farine_Pays, which = 'all')

# Construisons une base de donnÃ©es avec les volatilitÃ©s des prix de ces 3 produits

vola.mais_Pays    <- ts(garchFit_mais_Pays@fit$sigma^2)
vola.riz_tchako <- ts(garchFit_riz_tchako@fit$sigma^2)
vola.riz_local <- ts(garchFit_riz_local@fit$sigma^2)
vola.farine_Pays <- ts(garchFit_farine_Pays@fit$sigma^2)

vola.Plus_Pays <- as.data.frame(cbind(vola.mais_Pays, vola.riz_tchako, vola.riz_local, vola.farine_Pays))


pairs(vola.Plus_Pays)
pairs.panels(vola.Plus_Pays)
ggpairs(vola.Plus_Pays)

############################### A nalyse sur les donnÃ©es sur une base annuelle #####################################################

# Calculcons les prix moyens par annÃ©e pour ces produits alimentaires
data_average_Price_annuel <- data_wide_Pays_Plus %>%
  select(-Unite) %>%
  mutate(date = year(Date)) %>%
  group_by(date) %>%
  summarize_all(mean)

str(data_average_Price_annuel)
glimpse(data_average_Price_annuel)

# QUelques graphiques pour l'evoluton des prix moyens annuels
plot(data_average_Price_annuel$date, data_average_Price_annuel$riz_local, type = "l",
     main = "Prix du riz local",
     xlab = "AnnÃ©e",
     ylab = "HTG/lb")

plot(data_average_Price_annuel$date, data_average_Price_annuel$riz_tchako, type = "l",
     main = "Prix du riz tchako",
     xlab = "AnnÃ©e",
     ylab = "HTG/lb")

plot(data_average_Price_annuel$date, data_average_Price_annuel$mais_local, type = "l",
     main = "Prix du maÃ¯s local",
     xlab = "AnnÃ©e",
     ylab = "HTG/lb")

plot(data_average_Price_annuel$date, data_average_Price_annuel$farine_import, type = "l",
     main = "Prix de la farine importÃ©e",
     xlab = "AnnÃ©e",
     ylab = "HTG/lb")
# Quelque chose s'est passÃ© en 2016. QUoi?????? 


# Calculcons les prix moyens par mois pour ces produits alimentaires
data_average_Price_annuel <- data_wide_Pays_Plus %>%
  select(-Unite) %>%
  mutate(date = month(Date)) %>%
  group_by(date) %>%
  summarize_all(mean)

##############evaluation modele predictif#####################
logfitted_value <- log(data_wide_Plus$farine_importe)
for (i in 2:153){
  logfitted_value[i]<- logfitted_value[i-1]+(fitted(garchFit_farine)[i]/100)
  }
fitted_values <- exp(logfitted_value)
plot(data_wide_Plus$farine_importe)
lines(fitted_values)

####the model tren######
plot(aggregate(ts(data_wide_Plus$farine_importe), FUN = mean))
par(mfrow=c(2,2))
plot(data_average_Price %>%
       select(c(Date, mais_local)), type = "l",
     main = "Evolution du prix du maïs local au niveau national" )
plot(data_average_Price %>%
       select(c(Date, riz_tchako)), type = "l",
     main = "Evolution du prix du riz importé au niveau national" )
plot(data_average_Price %>%
       select(c(Date, riz_local)), type = "l",
     main = "Evolution du prix du riz local au niveau national" )
plot(data_average_Price %>%
       select(c(Date, farine_import)), type = "l",
     main = "Evolution du prix de la farine au niveau national" )


clustering_data <- subset(data_wide_Pays_Plus, data_wide_Pays_Plus$Date>"2008-07-15")
#les donnees sur Port-au-prince ne sont collectees qu'a partir de cette date


############################Préparation des données pour le clustering####################################
clustering_mais_local <- select(clustering_data, c(Date, Unite,mais_local, Ville))

clustering_mais_local_PAP <- clustering_mais_local %>%
  filter(Ville=="Port-au-Prince")
clustering_mais_local_Cap <- clustering_mais_local %>%
  filter(Ville=="Cap-Haitien")
clustering_mais_local_cayes <- clustering_mais_local %>%
  filter(Ville=="Les Cayes")
clustering_mais_local_Jacmel <- clustering_mais_local %>%
  filter(Ville=="Jacmel")
clustering_mais_local_Jeremie <- clustering_mais_local %>%
  filter(Ville=="Jeremie")
clustering_mais_local_Hinche <- clustering_mais_local %>%
  filter(Ville=="Hinche")
clustering_mais_local_gonaives <- clustering_mais_local %>%
  filter(Ville=="Gonaives")
clustering_mais_local_PdP <- clustering_mais_local %>%
  filter(Ville=="Port-de-Paix")
################################################################
date_retenue_cayes <- clustering_mais_local_cayes$Date
date_retenue_PAP <- clustering_mais_local_PAP$Date
date_retenue_Jacmel <- clustering_mais_local_Jacmel$Date
date_retenue_Jeremie <- clustering_mais_local_Jeremie$Date
date_retenue_Hinche <- clustering_mais_local_Hinche$Date
##################################################################
nrow(clustering_mais_local_Cap)
nrow(clustering_mais_local_PAP)
nrow(clustering_mais_local_cayes)
nrow(clustering_mais_local_Jacmel)
nrow(clustering_mais_local_Jeremie)
nrow(clustering_mais_local_Hinche)
nrow(clustering_mais_local_gonaives)
nrow(clustering_mais_local_PdP)
##############################Recherche de simultanéité dans les données disponibles########################################

clustering_mais_local_1 <- clustering_mais_local %>% 
  subset(Date == date_retenue_cayes[1])
for (i in 2:55){
  clustering_mais_local_1 <- rbind(clustering_mais_local_1, clustering_mais_local %>% 
                                     subset(Date==date_retenue_cayes[i]))
}

clustering_mais_local_2 <- clustering_mais_local_1 %>% 
  subset(Date == date_retenue_PAP[1])
for (i in 2:118){
  clustering_mais_local_2 <- rbind(clustering_mais_local_2, clustering_mais_local_1 %>% 
                                     subset(Date==date_retenue_PAP[i]))
}

clustering_mais_local_3 <- clustering_mais_local_2 %>% 
  subset(Date == date_retenue_Jacmel[1])
for (i in 2:114){
  clustering_mais_local_3 <- rbind(clustering_mais_local_3, clustering_mais_local_2 %>% 
                                     subset(Date==date_retenue_Jacmel[i]))
}

clustering_mais_local_4 <- clustering_mais_local_3 %>% 
  subset(Date == date_retenue_Jeremie[1])
for (i in 2:104){
  clustering_mais_local_4 <- rbind(clustering_mais_local_4, clustering_mais_local_3 %>% 
                                     subset(Date==date_retenue_Jeremie[i]))
}


clustering_mais_local_5 <- clustering_mais_local_4 %>% 
  subset(Date == date_retenue_Hinche[1])
for (i in 2:113){
  clustering_mais_local_5 <- rbind(clustering_mais_local_5, clustering_mais_local_4 %>% 
                                     subset(Date==date_retenue_Hinche[i]))
}
########################################################################
final_cluster_mais_local <- clustering_mais_local_5 %>%
  subset(Ville=="Port-au-Prince"|Ville=="Les Cayes"|Ville=="Jacmel"|Ville=="Jeremie"|Ville=="Hinche")
########################################################################

clustering_mais_local_PAP_1 <- clustering_mais_local_5 %>%
  filter(Ville=="Port-au-Prince")
clustering_mais_local_cayes_1 <- clustering_mais_local_5 %>%
  filter(Ville=="Les Cayes")
clustering_mais_local_Jacmel_1 <- clustering_mais_local_5 %>%
  filter(Ville=="Jacmel")
clustering_mais_local_Jeremie_1 <- clustering_mais_local_5 %>%
  filter(Ville=="Jeremie")
clustering_mais_local_Hinche_1 <- clustering_mais_local_5 %>%
  filter(Ville=="Hinche")
###############################Différence entre les prix########################################
clustering_mais_local_cayes_1$mais_local <- clustering_mais_local_cayes_1$mais_local - clustering_mais_local_PAP_1$mais_local
clustering_mais_local_Jacmel_1$mais_local <- clustering_mais_local_Jacmel_1$mais_local - clustering_mais_local_PAP_1$mais_local
clustering_mais_local_Jeremie_1$mais_local <- clustering_mais_local_Jeremie_1$mais_local - clustering_mais_local_PAP_1$mais_local
clustering_mais_local_Hinche_1$mais_local <- clustering_mais_local_Hinche_1$mais_local - clustering_mais_local_PAP_1$mais_local

########################Jeu de données final pour le clustering################
final_cluster_mais_local <- rbind(clustering_mais_local_Hinche_1,clustering_mais_local_cayes_1,clustering_mais_local_Jeremie_1,clustering_mais_local_Jacmel_1)







######################clustering################
###############Nous utilisons la méthode de clustering hiérarchique agglomérative###########
m <- c('average', 'single', 'complete', 'ward')
names(m) <-  c('average', 'single', 'complete', 'ward')
#fonction pour calculer les coefficients
ac <- function (x) {
  agnes(scale(final_cluster_mais_local$mais_local), method = x)$ac
}
map_dbl(m, ac)
#meilleure methode"ward"

#visualisons le dendogramme
hc <- agnes(scale(final_cluster_mais_local$mais_local), method = "ward")
pltree(hc, cex = 0.6, hang = -1, main = "Dendogramme de agnes")
fviz_nbclust(scale(final_cluster_mais_local$mais_local), FUN = hcut, method = "wss")

sub_grp <- cutree(hc, k=3)
table(sub_grp)

clustered_data <- final_cluster_mais_local %>%
  mutate(cluster = sub_grp)
clustered_data$Ville <- as.factor(as.character(clustered_data$Ville))

#fviz_cluster(list(data = clustered_data$mais_local, cluster = sub_grp))
library(RColorBrewer)
coul <- brewer.pal(4, 'Set2')
barplot(prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1), 
        beside = T,
        legend.text=T,
        col = coul,xlab = 'Cluster', ylab = "Proportion", main = "Répartition par Cluster")
prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1)

par(mfrow=c(1,1))
plot((subset(clustered_data, Ville=="Jeremie"))[,1], (subset(clustered_data, Ville=="Jeremie"))[,3], type='l', xlab='Temps', ylab='Différence de prix', main='Différence du prix du maïs local å Jérémie par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Les Cayes"))[,1], (subset(clustered_data, Ville=="Les Cayes"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du maïs local aux Cayes par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Jacmel"))[,1], (subset(clustered_data, Ville=="Jacmel"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du maïs local à Jacmel par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Hinche"))[,1], (subset(clustered_data, Ville=="Hinche"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du maïs local à Hinche par rapport à celui de Port-au-Prince')

############################Préparation des données pour le clustering####################################
clustering_riz_tchako <- select(clustering_data, c(Date, Unite,riz_tchako, Ville))

clustering_riz_tchako_PAP <- clustering_riz_tchako %>%
  filter(Ville=="Port-au-Prince")
clustering_riz_tchako_Cap <- clustering_riz_tchako %>%
  filter(Ville=="Cap-Haitien")
clustering_riz_tchako_cayes <- clustering_riz_tchako %>%
  filter(Ville=="Les Cayes")
clustering_riz_tchako_Jacmel <- clustering_riz_tchako %>%
  filter(Ville=="Jacmel")
clustering_riz_tchako_Jeremie <- clustering_riz_tchako %>%
  filter(Ville=="Jeremie")
clustering_riz_tchako_Hinche <- clustering_riz_tchako %>%
  filter(Ville=="Hinche")
clustering_riz_tchako_gonaives <- clustering_riz_tchako %>%
  filter(Ville=="Gonaives")
clustering_riz_tchako_PdP <- clustering_riz_tchako %>%
  filter(Ville=="Port-de-Paix")
################################################################
date_retenue_cayes <- clustering_riz_tchako_cayes$Date
date_retenue_PAP <- clustering_riz_tchako_PAP$Date
date_retenue_Jacmel <- clustering_riz_tchako_Jacmel$Date
date_retenue_Jeremie <- clustering_riz_tchako_Jeremie$Date
date_retenue_Hinche <- clustering_riz_tchako_Hinche$Date
##################################################################
nrow(clustering_riz_tchako_Cap)
nrow(clustering_riz_tchako_PAP)
nrow(clustering_riz_tchako_cayes)
nrow(clustering_riz_tchako_Jacmel)
nrow(clustering_riz_tchako_Jeremie)
nrow(clustering_riz_tchako_Hinche)
nrow(clustering_riz_tchako_gonaives)
nrow(clustering_riz_tchako_PdP)
##############################Recherche de simultanéité dans les données disponibles########################################

clustering_riz_tchako_1 <- clustering_riz_tchako %>% 
  subset(Date == date_retenue_cayes[1])
for (i in 2:55){
  clustering_riz_tchako_1 <- rbind(clustering_riz_tchako_1, clustering_riz_tchako %>% 
                                     subset(Date==date_retenue_cayes[i]))
}

clustering_riz_tchako_2 <- clustering_riz_tchako_1 %>% 
  subset(Date == date_retenue_PAP[1])
for (i in 2:118){
  clustering_riz_tchako_2 <- rbind(clustering_riz_tchako_2, clustering_riz_tchako_1 %>% 
                                     subset(Date==date_retenue_PAP[i]))
}

clustering_riz_tchako_3 <- clustering_riz_tchako_2 %>% 
  subset(Date == date_retenue_Jacmel[1])
for (i in 2:114){
  clustering_riz_tchako_3 <- rbind(clustering_riz_tchako_3, clustering_riz_tchako_2 %>% 
                                     subset(Date==date_retenue_Jacmel[i]))
}

clustering_riz_tchako_4 <- clustering_riz_tchako_3 %>% 
  subset(Date == date_retenue_Jeremie[1])
for (i in 2:104){
  clustering_riz_tchako_4 <- rbind(clustering_riz_tchako_4, clustering_riz_tchako_3 %>% 
                                     subset(Date==date_retenue_Jeremie[i]))
}


clustering_riz_tchako_5 <- clustering_riz_tchako_4 %>% 
  subset(Date == date_retenue_Hinche[1])
for (i in 2:113){
  clustering_riz_tchako_5 <- rbind(clustering_riz_tchako_5, clustering_riz_tchako_4 %>% 
                                     subset(Date==date_retenue_Hinche[i]))
}
########################################################################
final_cluster_riz_tchako <- clustering_riz_tchako_5 %>%
  subset(Ville=="Port-au-Prince"|Ville=="Les Cayes"|Ville=="Jacmel"|Ville=="Jeremie"|Ville=="Hinche")
########################################################################

clustering_riz_tchako_PAP_1 <- clustering_riz_tchako_5 %>%
  filter(Ville=="Port-au-Prince")
clustering_riz_tchako_cayes_1 <- clustering_riz_tchako_5 %>%
  filter(Ville=="Les Cayes")
clustering_riz_tchako_Jacmel_1 <- clustering_riz_tchako_5 %>%
  filter(Ville=="Jacmel")
clustering_riz_tchako_Jeremie_1 <- clustering_riz_tchako_5 %>%
  filter(Ville=="Jeremie")
clustering_riz_tchako_Hinche_1 <- clustering_riz_tchako_5 %>%
  filter(Ville=="Hinche")
###############################Différence entre les prix########################################
clustering_riz_tchako_cayes_1$riz_tchako <- clustering_riz_tchako_cayes_1$riz_tchako - clustering_riz_tchako_PAP_1$riz_tchako
clustering_riz_tchako_Jacmel_1$riz_tchako <- clustering_riz_tchako_Jacmel_1$riz_tchako - clustering_riz_tchako_PAP_1$riz_tchako
clustering_riz_tchako_Jeremie_1$riz_tchako <- clustering_riz_tchako_Jeremie_1$riz_tchako - clustering_riz_tchako_PAP_1$riz_tchako
clustering_riz_tchako_Hinche_1$riz_tchako <- clustering_riz_tchako_Hinche_1$riz_tchako - clustering_riz_tchako_PAP_1$riz_tchako

########################Jeu de données final pour le clustering################
final_cluster_riz_tchako <- rbind(clustering_riz_tchako_Hinche_1,clustering_riz_tchako_cayes_1,clustering_riz_tchako_Jeremie_1,clustering_riz_tchako_Jacmel_1)







######################clustering################
###############Nous utilisons la méthode de clustering hiérarchique agglomérative###########
m <- c('average', 'single', 'complete', 'ward')
names(m) <-  c('average', 'single', 'complete', 'ward')
#fonction pour calculer les coefficients
ac <- function (x) {
  agnes(scale(final_cluster_riz_tchako$riz_tchako), method = x)$ac
}
map_dbl(m, ac)
#meilleure methode"ward"

#visualisons le dendogramme
hc <- agnes(scale(final_cluster_riz_tchako$riz_tchako), method = "ward")
pltree(hc, cex = 0.6, hang = -1, main = "Dendogramme de agnes")
fviz_nbclust(scale(final_cluster_riz_tchako$riz_tchako), FUN = hcut, method = "wss")

sub_grp <- cutree(hc, k=3)
table(sub_grp)

clustered_data <- final_cluster_riz_tchako %>%
  mutate(cluster = sub_grp)
clustered_data$Ville <- as.factor(as.character(clustered_data$Ville))

#fviz_cluster(list(data = clustered_data$riz_tchako, cluster = sub_grp))
library(RColorBrewer)
coul <- brewer.pal(4, 'Set2')
barplot(prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1), 
        beside = T,
        legend.text=T,
        col = coul,xlab = 'Cluster', ylab = "Proportion", main = "Répartition par Cluster")
prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1)

par(mfrow=c(1,1))
plot((subset(clustered_data, Ville=="Jeremie"))[,1], (subset(clustered_data, Ville=="Jeremie"))[,3], type='l', xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz importé å Jérémie par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Les Cayes"))[,1], (subset(clustered_data, Ville=="Les Cayes"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz importé aux Cayes par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Jacmel"))[,1], (subset(clustered_data, Ville=="Jacmel"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz importé à Jacmel par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Hinche"))[,1], (subset(clustered_data, Ville=="Hinche"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz importé à Hinche par rapport à celui de Port-au-Prince')

clustering_farine_import <- select(clustering_data, c(Date, Unite,farine_import, Ville))

clustering_farine_import_PAP <- clustering_farine_import %>%
  filter(Ville=="Port-au-Prince")
clustering_farine_import_Cap <- clustering_farine_import %>%
  filter(Ville=="Cap-Haitien")
clustering_farine_import_cayes <- clustering_farine_import %>%
  filter(Ville=="Les Cayes")
clustering_farine_import_Jacmel <- clustering_farine_import %>%
  filter(Ville=="Jacmel")
clustering_farine_import_Jeremie <- clustering_farine_import %>%
  filter(Ville=="Jeremie")
clustering_farine_import_Hinche <- clustering_farine_import %>%
  filter(Ville=="Hinche")
clustering_farine_import_gonaives <- clustering_farine_import %>%
  filter(Ville=="Gonaives")
clustering_farine_import_PdP <- clustering_farine_import %>%
  filter(Ville=="Port-de-Paix")
################################################################
date_retenue_cayes <- clustering_farine_import_cayes$Date
date_retenue_PAP <- clustering_farine_import_PAP$Date
date_retenue_Jacmel <- clustering_farine_import_Jacmel$Date
date_retenue_Jeremie <- clustering_farine_import_Jeremie$Date
date_retenue_Hinche <- clustering_farine_import_Hinche$Date
##################################################################
nrow(clustering_farine_import_Cap)
nrow(clustering_farine_import_PAP)
nrow(clustering_farine_import_cayes)
nrow(clustering_farine_import_Jacmel)
nrow(clustering_farine_import_Jeremie)
nrow(clustering_farine_import_Hinche)
nrow(clustering_farine_import_gonaives)
nrow(clustering_farine_import_PdP)
##############################Recherche de simultanéité dans les données disponibles########################################

clustering_farine_import_1 <- clustering_farine_import %>% 
  subset(Date == date_retenue_cayes[1])
for (i in 2:55){
  clustering_farine_import_1 <- rbind(clustering_farine_import_1, clustering_farine_import %>% 
                                        subset(Date==date_retenue_cayes[i]))
}

clustering_farine_import_2 <- clustering_farine_import_1 %>% 
  subset(Date == date_retenue_PAP[1])
for (i in 2:118){
  clustering_farine_import_2 <- rbind(clustering_farine_import_2, clustering_farine_import_1 %>% 
                                        subset(Date==date_retenue_PAP[i]))
}

clustering_farine_import_3 <- clustering_farine_import_2 %>% 
  subset(Date == date_retenue_Jacmel[1])
for (i in 2:114){
  clustering_farine_import_3 <- rbind(clustering_farine_import_3, clustering_farine_import_2 %>% 
                                        subset(Date==date_retenue_Jacmel[i]))
}

clustering_farine_import_4 <- clustering_farine_import_3 %>% 
  subset(Date == date_retenue_Jeremie[1])
for (i in 2:104){
  clustering_farine_import_4 <- rbind(clustering_farine_import_4, clustering_farine_import_3 %>% 
                                        subset(Date==date_retenue_Jeremie[i]))
}


clustering_farine_import_5 <- clustering_farine_import_4 %>% 
  subset(Date == date_retenue_Hinche[1])
for (i in 2:113){
  clustering_farine_import_5 <- rbind(clustering_farine_import_5, clustering_farine_import_4 %>% 
                                        subset(Date==date_retenue_Hinche[i]))
}
########################################################################
final_cluster_farine_import <- clustering_farine_import_5 %>%
  subset(Ville=="Port-au-Prince"|Ville=="Les Cayes"|Ville=="Jacmel"|Ville=="Jeremie"|Ville=="Hinche")
########################################################################

clustering_farine_import_PAP_1 <- clustering_farine_import_5 %>%
  filter(Ville=="Port-au-Prince")
clustering_farine_import_cayes_1 <- clustering_farine_import_5 %>%
  filter(Ville=="Les Cayes")
clustering_farine_import_Jacmel_1 <- clustering_farine_import_5 %>%
  filter(Ville=="Jacmel")
clustering_farine_import_Jeremie_1 <- clustering_farine_import_5 %>%
  filter(Ville=="Jeremie")
clustering_farine_import_Hinche_1 <- clustering_farine_import_5 %>%
  filter(Ville=="Hinche")
###############################Différence entre les prix########################################
clustering_farine_import_cayes_1$farine_import <- clustering_farine_import_cayes_1$farine_import - clustering_farine_import_PAP_1$farine_import
clustering_farine_import_Jacmel_1$farine_import <- clustering_farine_import_Jacmel_1$farine_import - clustering_farine_import_PAP_1$farine_import
clustering_farine_import_Jeremie_1$farine_import <- clustering_farine_import_Jeremie_1$farine_import - clustering_farine_import_PAP_1$farine_import
clustering_farine_import_Hinche_1$farine_import <- clustering_farine_import_Hinche_1$farine_import - clustering_farine_import_PAP_1$farine_import

########################Jeu de données final pour le clustering################
final_cluster_farine_import <- rbind(clustering_farine_import_Hinche_1,clustering_farine_import_cayes_1,clustering_farine_import_Jeremie_1,clustering_farine_import_Jacmel_1)







######################clustering################
###############Nous utilisons la méthode de clustering hiérarchique agglomérative###########
m <- c('average', 'single', 'complete', 'ward')
names(m) <-  c('average', 'single', 'complete', 'ward')
#fonction pour calculer les coefficients
ac <- function (x) {
  agnes(scale(final_cluster_farine_import$farine_import), method = x)$ac
}
map_dbl(m, ac)
#meilleure methode"ward"

#visualisons le dendogramme
hc <- agnes(scale(final_cluster_farine_import$farine_import), method = "ward")
pltree(hc, cex = 0.6, hang = -1, main = "Dendogramme de agnes")
fviz_nbclust(scale(final_cluster_farine_import$farine_import), FUN = hcut, method = "wss")

sub_grp <- cutree(hc, k=3)
table(sub_grp)

clustered_data <- final_cluster_farine_import %>%
  mutate(cluster = sub_grp)
clustered_data$Ville <- as.factor(as.character(clustered_data$Ville))

#fviz_cluster(list(data = clustered_data$farine_import, cluster = sub_grp))
library(RColorBrewer)
coul <- brewer.pal(4, 'Set2')
barplot(prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1), 
        beside = T,
        legend.text=T,
        col = coul,xlab = 'Cluster', ylab = "Proportion", main = "Répartition par Cluster")
prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1)

par(mfrow=c(1,1))
plot((subset(clustered_data, Ville=="Jeremie"))[,1], (subset(clustered_data, Ville=="Jeremie"))[,3], type='l', xlab='Temps', ylab='Différence de prix', main='Différence du prix de la farine å Jérémie par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Les Cayes"))[,1], (subset(clustered_data, Ville=="Les Cayes"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix de la farine aux Cayes par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Jacmel"))[,1], (subset(clustered_data, Ville=="Jacmel"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix de la farine à Jacmel par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Hinche"))[,1], (subset(clustered_data, Ville=="Hinche"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix de la farine à Hinche par rapport à celui de Port-au-Prince')

############################Préparation des données pour le clustering####################################
clustering_riz_local <- select(clustering_data, c(Date, Unite,riz_local, Ville))

clustering_riz_local_PAP <- clustering_riz_local %>%
  filter(Ville=="Port-au-Prince")
clustering_riz_local_Cap <- clustering_riz_local %>%
  filter(Ville=="Cap-Haitien")
clustering_riz_local_cayes <- clustering_riz_local %>%
  filter(Ville=="Les Cayes")
clustering_riz_local_Jacmel <- clustering_riz_local %>%
  filter(Ville=="Jacmel")
clustering_riz_local_Jeremie <- clustering_riz_local %>%
  filter(Ville=="Jeremie")
clustering_riz_local_Hinche <- clustering_riz_local %>%
  filter(Ville=="Hinche")
clustering_riz_local_gonaives <- clustering_riz_local %>%
  filter(Ville=="Gonaives")
clustering_riz_local_PdP <- clustering_riz_local %>%
  filter(Ville=="Port-de-Paix")
################################################################
date_retenue_cayes <- clustering_riz_local_cayes$Date
date_retenue_PAP <- clustering_riz_local_PAP$Date
date_retenue_Jacmel <- clustering_riz_local_Jacmel$Date
date_retenue_Jeremie <- clustering_riz_local_Jeremie$Date
date_retenue_Hinche <- clustering_riz_local_Hinche$Date
date_retenue_gonaives <- clustering_riz_local_gonaives$Date
##################################################################
nrow(clustering_riz_local_Cap)
nrow(clustering_riz_local_PAP)
nrow(clustering_riz_local_cayes)
nrow(clustering_riz_local_Jacmel)
nrow(clustering_riz_local_Jeremie)
nrow(clustering_riz_local_Hinche)
nrow(clustering_riz_local_gonaives)
nrow(clustering_riz_local_PdP)
##############################Recherche de simultanéité dans les données disponibles########################################
clustering_riz_local_0 <- clustering_riz_local %>% 
  subset(Date == date_retenue_gonaives[1])
for (i in 2:50){
  clustering_riz_local_0 <- rbind(clustering_riz_local_0, clustering_riz_local %>% 
                                    subset(Date==date_retenue_gonaives[i]))
}

clustering_riz_local_1 <- clustering_riz_local_0 %>% 
  subset(Date == date_retenue_cayes[1])
for (i in 2:55){
  clustering_riz_local_1 <- rbind(clustering_riz_local_1, clustering_riz_local_0 %>% 
                                    subset(Date==date_retenue_cayes[i]))
}

clustering_riz_local_2 <- clustering_riz_local_1 %>% 
  subset(Date == date_retenue_PAP[1])
for (i in 2:118){
  clustering_riz_local_2 <- rbind(clustering_riz_local_2, clustering_riz_local_1 %>% 
                                    subset(Date==date_retenue_PAP[i]))
}

clustering_riz_local_3 <- clustering_riz_local_2 %>% 
  subset(Date == date_retenue_Jacmel[1])
for (i in 2:114){
  clustering_riz_local_3 <- rbind(clustering_riz_local_3, clustering_riz_local_2 %>% 
                                    subset(Date==date_retenue_Jacmel[i]))
}

clustering_riz_local_4 <- clustering_riz_local_3 %>% 
  subset(Date == date_retenue_Jeremie[1])
for (i in 2:104){
  clustering_riz_local_4 <- rbind(clustering_riz_local_4, clustering_riz_local_3 %>% 
                                    subset(Date==date_retenue_Jeremie[i]))
}


clustering_riz_local_5 <- clustering_riz_local_4 %>% 
  subset(Date == date_retenue_Hinche[1])
for (i in 2:113){
  clustering_riz_local_5 <- rbind(clustering_riz_local_5, clustering_riz_local_4 %>% 
                                    subset(Date==date_retenue_Hinche[i]))
}
########################################################################
final_cluster_riz_local <- clustering_riz_local_5 %>%
  subset(Ville=="Port-au-Prince"|Ville=="Les Cayes"|Ville=="Jacmel"|Ville=="Jeremie"|Ville=="Hinche"|Ville=="Gonaives")
########################################################################

clustering_riz_local_PAP_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Port-au-Prince")
clustering_riz_local_cayes_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Les Cayes")
clustering_riz_local_Jacmel_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Jacmel")
clustering_riz_local_Jeremie_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Jeremie")
clustering_riz_local_Hinche_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Hinche")
clustering_riz_local_gonaives_1 <- clustering_riz_local_5 %>%
  filter(Ville=="Gonaives")
###############################Différence entre les prix########################################
clustering_riz_local_cayes_1$riz_local <- clustering_riz_local_cayes_1$riz_local - clustering_riz_local_PAP_1$riz_local
clustering_riz_local_Jacmel_1$riz_local <- clustering_riz_local_Jacmel_1$riz_local - clustering_riz_local_PAP_1$riz_local
clustering_riz_local_Jeremie_1$riz_local <- clustering_riz_local_Jeremie_1$riz_local - clustering_riz_local_PAP_1$riz_local
clustering_riz_local_Hinche_1$riz_local <- clustering_riz_local_Hinche_1$riz_local - clustering_riz_local_PAP_1$riz_local
clustering_riz_local_gonaives_1$riz_local <- clustering_riz_local_gonaives_1$riz_local - clustering_riz_local_PAP_1$riz_local
########################Jeu de données final pour le clustering################
final_cluster_riz_local <- rbind(clustering_riz_local_Hinche_1,clustering_riz_local_cayes_1,clustering_riz_local_Jeremie_1,clustering_riz_local_Jacmel_1, clustering_riz_local_gonaives_1)

######################clustering################
###############Nous utilisons la méthode de clustering hiérarchique agglomérative###########
m <- c('average', 'single', 'complete', 'ward')
names(m) <-  c('average', 'single', 'complete', 'ward')
#fonction pour calculer les coefficients
ac <- function (x) {
  agnes(scale(final_cluster_riz_local$riz_local), method = x)$ac
}
map_dbl(m, ac)
#meilleure methode"ward"

#visualisons le dendogramme
hc <- agnes(scale(final_cluster_riz_local$riz_local), method = "ward")
pltree(hc, cex = 0.6, hang = -1, main = "Dendogramme de agnes")
fviz_nbclust(scale(final_cluster_riz_local$riz_local), FUN = hcut, method = "wss")

sub_grp <- cutree(hc, k=3)
table(sub_grp)

clustered_data <- final_cluster_riz_local %>%
  mutate(cluster = sub_grp)
clustered_data$Ville <- as.factor(as.character(clustered_data$Ville))

#fviz_cluster(list(data = clustered_data$riz_local, cluster = sub_grp))
library(RColorBrewer)
coul <- brewer.pal(5, 'Set2')
barplot(prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1), 
        beside = T,
        legend.text=T,
        col = coul,xlab = 'Cluster', ylab = "Proportion", main = "Répartition par Cluster")
prop.table(table(clustered_data[c( 'Ville','cluster')]), margin = 1)

par(mfrow=c(1,1))
plot((subset(clustered_data, Ville=="Jeremie"))[,1], (subset(clustered_data, Ville=="Jeremie"))[,3], type='l', xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz local å Jérémie par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Les Cayes"))[,1], (subset(clustered_data, Ville=="Les Cayes"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz local aux Cayes par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Jacmel"))[,1], (subset(clustered_data, Ville=="Jacmel"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz local à Jacmel par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Hinche"))[,1], (subset(clustered_data, Ville=="Hinche"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz local à Hinche par rapport à celui de Port-au-Prince')
plot((subset(clustered_data, Ville=="Gonaives"))[,1], (subset(clustered_data, Ville=="Gonaives"))[,3], type='l',xlab='Temps', ylab='Différence de prix', main='Différence du prix du riz local à Hinche par rapport à celui de Port-au-Prince')

###################transformation des données en série temporelle####################
ts_farine <- ts(data_average_Price[6], start = c(2005, 1), end = c(2020, 10), frequency = 12)
ts_farine
#acf(ts_farine)
#pacf(ts_farine)
#adf.test(ts_farine)
###############analyse preliminaire#############
#autoplot(ts_farine) +
#ggtitle('Evolution du prix du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')

Diff_prix <- diff(ts_farine)
#acf(Diff_prix)
#pacf(Diff_prix)

#autoplot(Diff_prix) +
#ggtitle('Evolution du prix en difference premiere du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')


#ggseasonplot(Diff_prix)

#ggsubseriesplot(Diff_prix)
##############################################
#Utilisation saisonniere naive
fit_naive <- snaive(ts_farine) #residus = 11.17

####lissage exponentiel######
fit_ets <- ets(ts_farine) # residual SD = 0.129

#############ARIMA Model############
fit_arima <- auto.arima(ts_farine, #ic="aic",
                        stepwise = TRUE, approximation = F, trace = T) # residuals SD = 8

############### Simple exponential smoothing################
fit_ses <- ses(ts_farine)


##############Holt's Trend Method#######
fit_holt <- holt(ts_farine)


##############TBATS MODEL############
fit_tbats <- tbats(ts_farine)



###########model evaluation##################


#####################

train_data <- window(ts_farine, start = c(2005, 1), end = c(2020, 7))
fit_arima1 <- auto.arima(train_data, stepwise = TRUE, approximation = F, trace = T)
fit_ets1 <- ets(train_data)
fit_naive1 <- snaive(train_data)
fit_ses1 <- ses(train_data)
fit_holt1 <- holt(train_data)
fit_tbats1 <- tbats(train_data)



a1 <- fit_arima1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a2 <- fit_ets1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a3 <- fit_naive1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a4 <- fit_ses1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a5 <- fit_holt1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a6 <- fit_tbats1 %>% forecast(h = 5) %>% accuracy(ts_farine)

a1[,c("RMSE", "MAE", "MAPE", "MASE")]

a2[,c("RMSE", "MAE", "MAPE", "MASE")]

a3[,c("RMSE", "MAE", "MAPE", "MASE")]

a4[,c("RMSE", "MAE", "MAPE", "MASE")]

a5[,c("RMSE", "MAE", "MAPE", "MASE")]

a6[,c("RMSE", "MAE", "MAPE", "MASE")]


autoplot(ts_farine) +
  autolayer(fitted(fit_arima), series = "Valeur \n estimée \n par la \n méthode \n ARIMA") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_farine) +
  autolayer(fitted(fit_ets), series = "Valeur \n estimée \n par la \n méthode \n ETS") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_farine) +
  autolayer(fitted(fit_naive), series = "Valeur \n estimée \n par la \n méthode \n naïve") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_farine) +
  autolayer(fitted(fit_ses), series = "Valeur \n estimée \n par la \n méthode \n SES") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_farine) +
  autolayer(fitted(fit_holt), series = "Valeur \n estimée \n par la \n méthode \n HOLT") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_farine) +
  autolayer(fitted(fit_tbats), series = "Valeur \n estimée \n par la \n méthode \n TBATS") +
  ylab('HTG') + xlab("Temps")


##############forecast##############
fcst_date <- c("Mai 2022", "Juin 2022", "Juillet 2022", "Aout 2022", "Septembre 2022")
fcst1 <- forecast(fit_naive, h=5)
autoplot(fcst1)
fcst1 <- data.frame(forecast(fit_naive, h=5))
rownames(fcst1) <- fcst_date
fcst1



fcst2 <- forecast(fit_ets, h=5)
autoplot(fcst2)
fcst2 <- data.frame(forecast(fit_ets, h=5))
rownames(fcst2) <- fcst_date
fcst2

fcst3 <- forecast(fit_arima, h=5)
autoplot(fcst3)
fcst3 <- data.frame(forecast(fit_arima, h=5))
rownames(fcst3) <- fcst_date
fcst3

fcst4 <- forecast(fit_ses, h=5)
autoplot(fcst4)
fcst4 <- data.frame(forecast(fit_ses, h=5))
rownames(fcst4) <- fcst_date
fcst4

fcst5 <- forecast(fit_holt, h=5)
autoplot(fcst5)
fcst5 <- data.frame(forecast(fit_holt, h=5))
rownames(fcst5) <- fcst_date
fcst5

fcst6 <- forecast(fit_tbats, h=5)
autoplot(fcst6)
fcst6 <- data.frame(forecast(fit_tbats, h=5))
rownames(fcst6) <- fcst_date
fcst6

###################transformation des données en série temporelle####################
ts_mais_local <- ts(data_average_Price[3], start = c(2005, 1), end = c(2020, 10), frequency = 12)
ts_mais_local
#acf(ts_mais_local)
#pacf(ts_mais_local)
#adf.test(ts_mais_local)
###############analyse preliminaire#############
#autoplot(ts_mais_local) +
#ggtitle('Evolution du prix du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')

Diff_prix <- diff(ts_mais_local)
#acf(Diff_prix)
#pacf(Diff_prix)

#autoplot(Diff_prix) +
#ggtitle('Evolution du prix en difference premiere du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')


#ggseasonplot(Diff_prix)

#ggsubseriesplot(Diff_prix)
##############################################
#Utilisation saisonniere naive
fit_naive <- snaive(ts_mais_local) #residus = 11.17

####lissage exponentiel######
fit_ets <- ets(ts_mais_local) # residual SD = 0.129

#############ARIMA Model############
fit_arima <- auto.arima(ts_mais_local, #ic="aic",
                        stepwise = TRUE, approximation = F, trace = T) # residuals SD = 8

############### Simple exponential smoothing################
fit_ses <- ses(ts_mais_local)


##############Holt's Trend Method#######
fit_holt <- holt(ts_mais_local)


##############TBATS MODEL############
fit_tbats <- tbats(ts_mais_local)



###########model evaluation##################


#####################

train_data <- window(ts_mais_local, start = c(2005, 1), end = c(2020, 7))
fit_arima1 <- auto.arima(train_data, stepwise = TRUE, approximation = F, trace = T)
fit_ets1 <- ets(train_data)
fit_naive1 <- snaive(train_data)
fit_ses1 <- ses(train_data)
fit_holt1 <- holt(train_data)
fit_tbats1 <- tbats(train_data)



a1 <- fit_arima1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a2 <- fit_ets1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a3 <- fit_naive1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a4 <- fit_ses1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a5 <- fit_holt1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a6 <- fit_tbats1 %>% forecast(h = 5) %>% accuracy(ts_mais_local)

a1[,c("RMSE", "MAE", "MAPE", "MASE")]

a2[,c("RMSE", "MAE", "MAPE", "MASE")]

a3[,c("RMSE", "MAE", "MAPE", "MASE")]

a4[,c("RMSE", "MAE", "MAPE", "MASE")]

a5[,c("RMSE", "MAE", "MAPE", "MASE")]

a6[,c("RMSE", "MAE", "MAPE", "MASE")]


autoplot(ts_mais_local) +
  autolayer(fitted(fit_arima), series = "Valeur \n estimée \n par la \n méthode \n ARIMA") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_mais_local) +
  autolayer(fitted(fit_ets), series = "Valeur \n estimée \n par la \n méthode \n ETS") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_mais_local) +
  autolayer(fitted(fit_naive), series = "Valeur \n estimée \n par la \n méthode \n naïve") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_mais_local) +
  autolayer(fitted(fit_ses), series = "Valeur \n estimée \n par la \n méthode \n SES") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_mais_local) +
  autolayer(fitted(fit_holt), series = "Valeur \n estimée \n par la \n méthode \n HOLT") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_mais_local) +
  autolayer(fitted(fit_tbats), series = "Valeur \n estimée \n par la \n méthode \n TBATS") +
  ylab('HTG') + xlab("Temps")


##############forecast##############
fcst_date <- c("Mai 2022", "Juin 2022", "Juillet 2022", "Aout 2022", "Septembre 2022")
fcst1 <- forecast(fit_naive, h=5)
autoplot(fcst1)
fcst1 <- data.frame(forecast(fit_naive, h=5))
rownames(fcst1) <- fcst_date
fcst1



fcst2 <- forecast(fit_ets, h=5)
autoplot(fcst2)
fcst2 <- data.frame(forecast(fit_ets, h=5))
rownames(fcst2) <- fcst_date
fcst2

fcst3 <- forecast(fit_arima, h=5)
autoplot(fcst3)
fcst3 <- data.frame(forecast(fit_arima, h=5))
rownames(fcst3) <- fcst_date
fcst3

fcst4 <- forecast(fit_ses, h=5)
autoplot(fcst4)
fcst4 <- data.frame(forecast(fit_ses, h=5))
rownames(fcst4) <- fcst_date
fcst4

fcst5 <- forecast(fit_holt, h=5)
autoplot(fcst5)
fcst5 <- data.frame(forecast(fit_holt, h=5))
rownames(fcst5) <- fcst_date
fcst5

fcst6 <- forecast(fit_tbats, h=5)
autoplot(fcst6)
fcst6 <- data.frame(forecast(fit_tbats, h=5))
rownames(fcst6) <- fcst_date
fcst6

###################transformation des données en série temporelle####################
ts_riz_local <- ts(data_average_Price[3], start = c(2005, 1), end = c(2020, 10), frequency = 12)
ts_riz_local
#acf(ts_riz_local)
#pacf(ts_riz_local)
#adf.test(ts_riz_local)
###############analyse preliminaire#############
#autoplot(ts_riz_local) +
#ggtitle('Evolution du prix du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')

Diff_prix <- diff(ts_riz_local)
#acf(Diff_prix)
#pacf(Diff_prix)

#autoplot(Diff_prix) +
#ggtitle('Evolution du prix en difference premiere du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')


#ggseasonplot(Diff_prix)

#ggsubseriesplot(Diff_prix)
##############################################
#Utilisation saisonniere naive
fit_naive <- snaive(ts_riz_local) #residus = 11.17

####lissage exponentiel######
fit_ets <- ets(ts_riz_local) # residual SD = 0.129

#############ARIMA Model############
fit_arima <- auto.arima(ts_riz_local, #ic="aic",
                        stepwise = TRUE, approximation = F, trace = T) # residuals SD = 8

############### Simple exponential smoothing################
fit_ses <- ses(ts_riz_local)


##############Holt's Trend Method#######
fit_holt <- holt(ts_riz_local)


##############TBATS MODEL############
fit_tbats <- tbats(ts_riz_local)



###########model evaluation##################


#####################

train_data <- window(ts_riz_local, start = c(2005, 1), end = c(2020, 7))
fit_arima1 <- auto.arima(train_data, stepwise = TRUE, approximation = F, trace = T)
fit_ets1 <- ets(train_data)
fit_naive1 <- snaive(train_data)
fit_ses1 <- ses(train_data)
fit_holt1 <- holt(train_data)
fit_tbats1 <- tbats(train_data)



a1 <- fit_arima1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a2 <- fit_ets1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a3 <- fit_naive1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a4 <- fit_ses1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a5 <- fit_holt1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a6 <- fit_tbats1 %>% forecast(h = 5) %>% accuracy(ts_riz_local)

a1[,c("RMSE", "MAE", "MAPE", "MASE")]

a2[,c("RMSE", "MAE", "MAPE", "MASE")]

a3[,c("RMSE", "MAE", "MAPE", "MASE")]

a4[,c("RMSE", "MAE", "MAPE", "MASE")]

a5[,c("RMSE", "MAE", "MAPE", "MASE")]

a6[,c("RMSE", "MAE", "MAPE", "MASE")]


autoplot(ts_riz_local) +
  autolayer(fitted(fit_arima), series = "Valeur \n estimée \n par la \n méthode \n ARIMA") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_local) +
  autolayer(fitted(fit_ets), series = "Valeur \n estimée \n par la \n méthode \n ETS") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_local) +
  autolayer(fitted(fit_naive), series = "Valeur \n estimée \n par la \n méthode \n naïve") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_local) +
  autolayer(fitted(fit_ses), series = "Valeur \n estimée \n par la \n méthode \n SES") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_local) +
  autolayer(fitted(fit_holt), series = "Valeur \n estimée \n par la \n méthode \n HOLT") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_local) +
  autolayer(fitted(fit_tbats), series = "Valeur \n estimée \n par la \n méthode \n TBATS") +
  ylab('HTG') + xlab("Temps")


##############forecast##############
fcst_date <- c("Mai 2022", "Juin 2022", "Juillet 2022", "Aout 2022", "Septembre 2022")
fcst1 <- forecast(fit_naive, h=5)
autoplot(fcst1)
fcst1 <- data.frame(forecast(fit_naive, h=5))
rownames(fcst1) <- fcst_date
fcst1



fcst2 <- forecast(fit_ets, h=5)
autoplot(fcst2)
fcst2 <- data.frame(forecast(fit_ets, h=5))
rownames(fcst2) <- fcst_date
fcst2

fcst3 <- forecast(fit_arima, h=5)
autoplot(fcst3)
fcst3 <- data.frame(forecast(fit_arima, h=5))
rownames(fcst3) <- fcst_date
fcst3

fcst4 <- forecast(fit_ses, h=5)
autoplot(fcst4)
fcst4 <- data.frame(forecast(fit_ses, h=5))
rownames(fcst4) <- fcst_date
fcst4

fcst5 <- forecast(fit_holt, h=5)
autoplot(fcst5)
fcst5 <- data.frame(forecast(fit_holt, h=5))
rownames(fcst5) <- fcst_date
fcst5

fcst6 <- forecast(fit_tbats, h=5)
autoplot(fcst6)
fcst6 <- data.frame(forecast(fit_tbats, h=5))
rownames(fcst6) <- fcst_date
fcst6

###################transformation des données en série temporelle####################
ts_riz_tchako <- ts(data_average_Price[3], start = c(2005, 1), end = c(2020, 10), frequency = 12)
ts_riz_tchako
#acf(ts_riz_tchako)
#pacf(ts_riz_tchako)
#adf.test(ts_riz_tchako)
###############analyse preliminaire#############
#autoplot(ts_riz_tchako) +
#ggtitle('Evolution du prix du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')

Diff_prix <- diff(ts_riz_tchako)
#acf(Diff_prix)
#pacf(Diff_prix)

#autoplot(Diff_prix) +
#ggtitle('Evolution du prix en difference premiere du riz local en Gourdes: 2005-2022')+
#ylab('Prix riz local')


#ggseasonplot(Diff_prix)

#ggsubseriesplot(Diff_prix)
##############################################
#Utilisation saisonniere naive
fit_naive <- snaive(ts_riz_tchako) #residus = 11.17

####lissage exponentiel######
fit_ets <- ets(ts_riz_tchako) # residual SD = 0.129

#############ARIMA Model############
fit_arima <- auto.arima(ts_riz_tchako, #ic="aic",
                        stepwise = TRUE, approximation = F, trace = T) # residuals SD = 8

############### Simple exponential smoothing################
fit_ses <- ses(ts_riz_tchako)


##############Holt's Trend Method#######
fit_holt <- holt(ts_riz_tchako)


##############TBATS MODEL############
fit_tbats <- tbats(ts_riz_tchako)



###########model evaluation##################


#####################

train_data <- window(ts_riz_tchako, start = c(2005, 1), end = c(2020, 7))
fit_arima1 <- auto.arima(train_data, stepwise = TRUE, approximation = F, trace = T)
fit_ets1 <- ets(train_data)
fit_naive1 <- snaive(train_data)
fit_ses1 <- ses(train_data)
fit_holt1 <- holt(train_data)
fit_tbats1 <- tbats(train_data)



a1 <- fit_arima1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a2 <- fit_ets1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a3 <- fit_naive1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a4 <- fit_ses1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a5 <- fit_holt1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a6 <- fit_tbats1 %>% forecast(h = 5) %>% accuracy(ts_riz_tchako)

a1[,c("RMSE", "MAE", "MAPE", "MASE")]

a2[,c("RMSE", "MAE", "MAPE", "MASE")]

a3[,c("RMSE", "MAE", "MAPE", "MASE")]

a4[,c("RMSE", "MAE", "MAPE", "MASE")]

a5[,c("RMSE", "MAE", "MAPE", "MASE")]

a6[,c("RMSE", "MAE", "MAPE", "MASE")]


autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_arima), series = "Valeur \n estimée \n par la \n méthode \n ARIMA") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_ets), series = "Valeur \n estimée \n par la \n méthode \n ETS") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_naive), series = "Valeur \n estimée \n par la \n méthode \n naïve") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_ses), series = "Valeur \n estimée \n par la \n méthode \n SES") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_holt), series = "Valeur \n estimée \n par la \n méthode \n HOLT") +
  ylab('HTG') + xlab("Temps")

autoplot(ts_riz_tchako) +
  autolayer(fitted(fit_tbats), series = "Valeur \n estimée \n par la \n méthode \n TBATS") +
  ylab('HTG') + xlab("Temps")


##############forecast##############
fcst_date <- c("Mai 2022", "Juin 2022", "Juillet 2022", "Aout 2022", "Septembre 2022")
fcst1 <- forecast(fit_naive, h=5)
autoplot(fcst1)
fcst1 <- data.frame(forecast(fit_naive, h=5))
rownames(fcst1) <- fcst_date
fcst1



fcst2 <- forecast(fit_ets, h=5)
autoplot(fcst2)
fcst2 <- data.frame(forecast(fit_ets, h=5))
rownames(fcst2) <- fcst_date
fcst2

fcst3 <- forecast(fit_arima, h=5)
autoplot(fcst3)
fcst3 <- data.frame(forecast(fit_arima, h=5))
rownames(fcst3) <- fcst_date
fcst3

fcst4 <- forecast(fit_ses, h=5)
autoplot(fcst4)
fcst4 <- data.frame(forecast(fit_ses, h=5))
rownames(fcst4) <- fcst_date
fcst4

fcst5 <- forecast(fit_holt, h=5)
autoplot(fcst5)
fcst5 <- data.frame(forecast(fit_holt, h=5))
rownames(fcst5) <- fcst_date
fcst5

fcst6 <- forecast(fit_tbats, h=5)
autoplot(fcst6)
fcst6 <- data.frame(forecast(fit_tbats, h=5))
rownames(fcst6) <- fcst_date
fcst6
