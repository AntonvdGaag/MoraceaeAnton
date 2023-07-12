library(dplyr)
library(broom)
library(plyr)
library(readr)
library(tidyverse)
library(readxl)

load(file = "./analysis/homogenization.RData")

# Get a version of net.long with only native species, call it net.long.nat
net.long.nat <- subset(net.long, plant.native.status == "native" & animal.native.status == "native")

# Remove all non interactions (0) from the dataframe
net.long.nat2<-subset(net.long.nat, value!="0")

# Remove all non Moraceae family species
net.long.nat3<-subset(net.long.nat2, plant.family=="Moraceae")

# Load Moraceae species into dataframe
library(readxl)
Moraceae_all_species <- read_excel("analysis/Moraceae_spreadsheet_for_data_scoringv2(1).xlsx")
names(Moraceae_all_species)[names(Moraceae_all_species) == "Plant_species"] <- "plant.accepted.species"
Moraceae_all_species$plant.accepted.species <- gsub('_', 
                                                    ' ', 
                                                    Moraceae_all_species$plant.accepted.species)
Moraceae_only_species = subset(Moraceae_all_species, select = c(plant.accepted.species,fruit.size))
Moraceae_only_species <- Moraceae_only_species[-c(403,404,405,406,407,408,409,410),]

# Merge interaction dataframe with our specieslist
net.long.nat4 <- merge(Moraceae_only_species, net.long.nat3, by=c("plant.accepted.species"))
net.long.nat5 <- merge(Moraceae_spatial, net.long.nat3, by=c("plant.accepted.species"))

### Fruit size plots
Moraceae_only_species$fruit.size <- as.numeric(Moraceae_only_species$fruit.size)
Moraceae_only_species2 <- Moraceae_only_species[-c(344,348,114),] #delete 3 species without values

d <- density(Moraceae_only_species2$fruit.size)
plot(d)

# grouping the Moraceae species by spatial
Moraceae_spatial <- subset(Moraceae_all_species, select = c(plant.accepted.species,fruit.size,`Continent 1`))
library(sm)
Moraceae_spatial$fruit.size <- as.numeric(Moraceae_spatial$fruit.size)
Moraceae_spatial <- Moraceae_spatial[-c(344,348,114),] #delete 3 species without values
Moraceae_spatial$`Continent 1` <- as.factor(Moraceae_spatial$`Continent 1`)# Continent 1 needs to be a factor
d2 <- sm.density.compare(Moraceae_spatial$fruit.size,`Continent 1`)
density()

# boxplot for the 4 continents
boxplot(fruit.size ~ `Continent 1`, data = Moraceae_spatial, col = "red")

# boxplot for the 4 continents with the three large values
Moraceae_spatial2 <- Moraceae_spatial[-c(3,4,392),]
Moraceae_spatial$`Continent 1`[Moraceae_spatial$`Continent 1` == "OCEANIA"] <- "ASIA"
Moraceae_spatial$`Continent 1` <- droplevels(Moraceae_spatial$`Continent 1`, exclude = "OCEANIA")
boxplot(log(fruit.size) ~ `Continent 1`, 
        data = Moraceae_spatial, 
        col = c("green","red","green"),
        xlab = "Tropical region",
        ylab = "log scaled fruit size",
        names = c("Afrotropics","Neotropics","Indo-malay"),
        main = "Moraceae fruit size in all tropical regions" 
        )
table(Moraceae_spatial$`Continent 1`)

# Test for Normality
# Africa
library("ggpubr")
Africa <- Moraceae_spatial2[Moraceae_spatial2$`Continent 1` == "AFRICA",]
ggdensity(Africa$fruit.size)
ggqqplot(Africa$fruit.size)
shapiro.test(log(Africa$fruit.size)) # Shapiro-Wilk normality test -> not normal

#Asia
Asia <- Moraceae_spatial2[Moraceae_spatial2$`Continent 1` == "ASIA",]
ggdensity(log(Asia$fruit.size))
ggqqplot(log(Asia$fruit.size))
shapiro.test(log(Asia$fruit.size)) # Shapiro-Wilk normality test -> not normal

#Americas
Americas <- Moraceae_spatial2[Moraceae_spatial2$`Continent 1` == "AMERICAS",]
ggdensity(Americas$fruit.size)
ggqqplot(Americas$fruit.size)
shapiro.test(log(Americas$fruit.size)) # Shapiro-Wilk normality test -> not normal

#Oceania
Oceania <- Moraceae_spatial2[Moraceae_spatial2$`Continent 1` == "OCEANIA",]
ggdensity(Oceania$fruit.size)
ggqqplot(Oceania$fruit.size)
shapiro.test(Oceania$fruit.size) # Shapiro-Wilk normality test -> not normal

#Kruskal-Wallis test 
library(dplyr)
kruskal.test(fruit.size ~ `Continent 1`, data = Moraceae_spatial)
# p-value = 0.00758 -> there are significant differences between the continents

#It is not known which continents differ from each other, but this can be done with a pairwise comparison
# Multiple pairwise-comparison between groups
pairwise.wilcox.test(Moraceae_spatial$fruit.size, Moraceae_spatial$`Continent 1`,
                     p.adjust.method = "BH")
# Only America is significantly different from Asia and Africa.

### Birds interaction

# Birds measurments
McFadden <- read_excel("analysis/McFadden-et-al_Avian-frugivore-traits_V2.xlsx", sheet=2)
McFadden2 <- subset(McFadden, select = c(1,13))
ggdensity(McFadden2$`Gape Size`)

#Merge data sets with gape width
Birds <- subset(net.long.nat4, animal.group == "birds")
names(McFadden2)[names(McFadden2) == "Species"] <- "animal.accepted.species"
Birds2 <- merge(Birds, McFadden2, by=c("animal.accepted.species"))

#gape size for only birds with and without interaction
ggplot(Moraceae_spatial1.1, aes(x = fruit.size, y = plant.accepted.genus)) +
  geom_point(aes(color = cut(fruit.size, breaks = c(0, 25, 70, Inf))), position = "identity", alpha = 0.5) +
  labs(x = "fruit size", y = "genus", color = "fruit size category") +
  scale_color_manual(values = c("green", "blue", "red"), labels = c("small", "medium", "large")) +
  scale_x_continuous(breaks = seq(0, max(Moraceae_spatial1.1$fruit.size), 25), limits = c(0, NA)) +
  theme_minimal()

min_width_allbirds <- min(McFadden2$`Gape Size`)
max_width_allbirds <- max(McFadden2$`Gape Size`)

min_width_intbirds <- min(McFadden4$`Gape Size`)
max_width_intbirds <- max(McFadden4$`Gape Size`)

ggplot(McFadden2, aes(x = `Gape Size`, y = animal.accepted.species)) +
  geom_point() +
  labs(x = "", y = "Gape Width") +
  theme_bw()

library(ggplot2)

McFadden2.1 <-McFadden2
Mcfadden4.1 <-McFadden4
McFadden2.1$Factor <- "All"
Mcfadden4.1$Factor <- "Int"
McFadden2and4 <- rbind(McFadden2.1,Mcfadden4.1)

ggplot(McFadden2and4, aes(x = `Gape Size`, y = Factor)) +
  geom_jitter(height = 0.3, width = 0.2, alpha = 0.7) +
  geom_boxplot(alpha = 0.3, fill = "black", color = "red") +
  labs(x = "Gape Width", y = "") +
  theme_bw() 





# gape size for only birds
McFadden3 <- subset(Birds2, select = c(1,42))
McFadden3.5 <- unique(McFadden3)
McFadden4 <- McFadden3[!duplicated(McFadden3$animal.accepted.species), ]
ggdensity(McFadden4$`Gape Size`)

# density plot for Moraceae and gape width
MB <- subset(Birds2, select = c(2,3))
MB$fruit.size <- as.numeric(MB$fruit.size)
MB <- MB[!duplicated(MB$plant.accepted.species), ]
ggdensity(MB$fruit.size)
McFadden4$Bird <- 'Bird'
McFadden4$Bird <- as.factor(McFadden4$Bird)
MB$Moraceae <- 'Moraceae'
MB$Moraceae <- as.factor(MB$Moraceae)
str(MB)

MB2 <- MB
names(MB2)[names(MB2) == "plant.accepted.species"] <- "accepted.species"
names(MB2)[names(MB2) == "fruit.size"] <- "length"
names(MB2)[names(MB2) == "Moraceae"] <- "Factor"

MCF5 <- McFadden4
names(MCF5)[names(MCF5) == "animal.accepted.species"] <- "accepted.species"
names(MCF5)[names(MCF5) == "Gape Size"] <- "length"
names(MCF5)[names(MCF5) == "Bird"] <- "Factor"

MB2MCF5 <- rbind(MB2,MCF5)
ggdensity(MB2MCF5, x="length", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

## density plot for Moraceae and gape width based on continent

# Moraceae
MoraceaeA <- Moraceae_all_species
names(MoraceaeA)[names(MoraceaeA) == "plant.accepted.species"] <- "accepted.species"
MBA <- merge(MB2, MoraceaeA, by = "accepted.species") 
MBA2 <- subset(MBA, select = c(1,2,3,7),)

# Birds
Birds3 <- merge(Birds, Moraceae_spatial, by=c("plant.accepted.species"))
SPB <- Birds3
names(SPB)[names(SPB) == "animal.accepted.species"] <- "accepted.species"
SPB <- subset(SPB, select = c(7,43),)
SPB2 <- merge(MCF5,SPB, by="accepted.species")
SPB2 <- SPB2[!duplicated(SPB2$accepted.species), ]
SPB3 <- subset(SPB2, select = c(1,2,3,4),)

# Rbind
MBA2SPB3 <- rbind(MBA2,SPB3)

# Africa
Africa2 <- MBA2SPB3[MBA2SPB3$`Continent 1` == "AFRICA",]
ggdensity(Africa2, x="length", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

#Asia
Asia2 <- MBA2SPB3[MBA2SPB3$`Continent 1` == "ASIA",]
ggdensity(Asia2, x="length", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

#Americas
Americas2 <- MBA2SPB3[MBA2SPB3$`Continent 1` == "AMERICAS",]
ggdensity(Americas2, x="length", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

#Oceania
Oceania2 <- MBA2SPB3[MBA2SPB3$`Continent 1` == "OCEANIA",]
ggdensity(Oceania2, x="length", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

### Birds check for large animals.
Birds4 <- subset(net.long.nat5, animal.group == "birds")
names(McFadden2)[names(McFadden2) == "Species"] <- "animal.accepted.species"
Birds5 <- merge(Birds, McFadden2, by=c("animal.accepted.species"))

#fruit sizes that each bird species eats
Birds5$fruit.size <- as.numeric(Birds5$fruit.size)

boxplot(log(fruit.size)~animal.accepted.species,
        data=Birds5,
        main="Fruit size which birds consume",
        col="orange",
        border="brown"
)

# Gape width for Moraceae and all
McFadden2 # All Birds
McFadden4 # Birds with our Moraceae interaction
McFadden2.1 <- McFadden2
McFadden2.1$Bird <- 'Bird2'
McFadden2.1$Bird <- as.factor(McFadden2.1$Bird)

McFadden2.1and4 <- rbind(McFadden2.1,McFadden4)
McFadden2.1and4$Bird <- as.factor(McFadden2.1and4$Bird)
names(McFadden2.1and4)[names(McFadden2.1and4) == "Gape Size"] <- "Gape_Size"
ggdensity(McFadden2.1and4, x ="Gape_Size", add= "mean", rug= T, color= "Bird", fill = "Bird", palette = c("green","red"))


### Mammals interaction
mammals <- read_excel("analysis/traits_mammals_species_level(1).xlsx")
unique(mammals$animal_group_fricke)
mammals2 <- subset(mammals, plant_family_fricke == "Moraceae")
primates <- subset(mammals2, animal_group_fricke == 'primates')
primates2 <- primates[!duplicated(primates$animal_accepted_species_fricke), ] # get only primates and their weigth

primates$Body_Mass_combine <- as.numeric(primates$Body_Mass_combine)
PBMc <- subset(primates, select = c(2,21))
ggdensity(PBMc$Body_Mass_combine) # does not work well

primates2$BodyMass.Value_elton <- as.numeric(primates2$BodyMass.Value_elton)
PBMe <- subset(primates2, select = c(2,29))
ggdensity(PBMe$BodyMass.Value_elton/1000)

primates2$Mass.g_phyl <- as.numeric(primates2$Mass.g_phyl)
PBMg <- subset(primates2, select = c(2,22))
ggdensity(log(PBMg$Mass.g_phyl))

# Moraceae and primates interaction.
PBMg2 <- PBMg
names(PBMg2)[names(PBMg2) == "animal_accepted_species_fricke"] <- "animal.accepted.species"
PriMog <- merge(PBMg2, net.long.nat4, by=c("animal.accepted.species"))

#check if primate species are indeed the largest
unique(PriMog$animal.accepted.species) #49 species
PriMog2 <- PriMog[!duplicated(PriMog$animal.accepted.species), ] # get single values for each primate
ggdensity(PriMog2$Mass.g_phyl/1000)

TD_phyl <- read_excel("analysis/Trait_data_phyl.xlsx")
TDpp <- subset(TD_phyl, Order.1.2 == "Primates")
TDpp2 <- subset(TDpp, select = (c(1,12))) # all primates weights
TDpp2$Mass.g <- as.numeric(TDpp2$Mass.g)
ggdensity(log(TDpp2$Mass.g))

TDpp2$Factor <- 'all'
TDpp2$Factor <- as.factor(TDpp2$Factor)

PBMg2$Factor <- "Moraceae"
PBMg2$Factor <- as.factor(PBMg2$Factor)

TDpp3 <- TDpp2
names(TDpp3)[names(TDpp3) == "Binomial.1.2"] <- "accepted.species"
names(TDpp3)[names(TDpp3) == "Mass.g"] <- "Weigth"
TDpp3$Weigth = log(TDpp3$Weigth)

PBMg3 <- PBMg2
names(PBMg3)[names(PBMg3) == "animal.accepted.species"] <- "accepted.species"
names(PBMg3)[names(PBMg3) == "Mass.g_phyl"] <- "Weigth"
PBMg3$Weigth = log(PBMg3$Weigth)

TDpp3PBMg3 <- rbind(TDpp3,PBMg3) # The primates that have interaction seem to contain the big species.
ggdensity(TDpp3PBMg3, x ="Weigth", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

a=460/72

# Moraceae and Primate interaction continued
PriMog$fruit.size <- as.numeric(PriMog$fruit.size)

boxplot(log(fruit.size)~animal.accepted.species,
        data=PriMog,
        main="Fruit size which primates consume",
        col="orange",
        border="brown"
)
# Primate body mass variable has to be made equivalent to fruit size.
range(PriMog$fruit.size) # fruit size range 3.5 - 45.0
range(PriMog$Mass.g_phyl/1000, na.rm=T) # weigth (in kg) range 0.45 - 120.95
PBeq = 41.5/120.5 # 41.5/120.5 = 0,34

# Primate body size compared to fruit size
PBMg4 <- PBMg2
PBMg4$Factor <- 'Primate'
PFS <- subset(PriMog, select = c(3,4)) # fruit size of species that interact with primates
PFS <- PFS[!duplicated(PFS$plant.accepted.species), ]
PFS$Factor <- "Moraceae"

names(PBMg4)[names(PBMg4) == "animal.accepted.species"] <- "accepted.species"
names(PBMg4)[names(PBMg4) == "Mass.g_phyl"] <- "Size"
PBMg4$Size <- PBMg4$Size/1000
PBMg4$Size <- PBMg4$Size*PBeq

names(PFS)[names(PFS) == "plant.accepted.species"] <- "accepted.species"
names(PFS)[names(PFS) == "fruit.size"] <- "Size"

PBMg4PFS <- rbind(PBMg4,PFS) #combine fruit size and primates.
ggdensity(PBMg4PFS, x ="Size", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

### Bats
Moraceae_spatial # Moraceae trait and spatial data per Moraceae species
net.long.nat5# Interaction data between Moraceae and animal species with traits and spatial data
mammals # body weight of all mammals

bats <- subset(net.long.nat5, animal.group=="bats")

#fruit sizes for each bat species         1
boxplot(log(fruit.size)~animal.accepted.species,
        data=bats,
        main="Fruit size which bats consume",
        col="orange",
        border="brown",
        range = 0
)
# average fruit sizes for each bat species
library(ggplot2)
ggplot(bats, aes(x=animal.accepted.species, y=fruit.size)) + 
  geom_point(stat="summary", fun="mean") 
bats1.1 <- aggregate(bats$fruit.size, list(bats$animal.accepted.species), FUN=mean)
names(bats1.1)[names(bats1.1) == "Group.1"] <- "animal_accepted_species_fricke"

# average fruit sizes for each bat species ascending order      2
bats1.2 <- merge(bats1.1, bats3, by=c("animal_accepted_species_fricke"))
bats1.2$Mass.g_phyl <- as.numeric(bats1.2$Mass.g_phyl)
bats1.2.1 <- bats1.2[order(bats1.2$Mass.g_phyl,decreasing = F),]
names(bats1.2.1)[names(bats1.2.1) == "x"] <- "fruit.size"

ggplot(bats1.2.1, aes(x=animal_accepted_species_fricke, y=fruit.size)) +
  geom_point(stat="summary", fun="mean")+
  scale_x_discrete(limits=bats1.2.1$animal_accepted_species_fricke) 

library(ggpmisc)
ggplot(bats1.2.1,aes(log(Mass.g_phyl), log(fruit.size))) +
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

cor(log(bats1.2.1$Mass.g_phyl), log(bats1.2.1$fruit.size)) # 0.35 correlation
batlinearmodel <- lm(log(Mass.g_phyl) ~ log(fruit.size), data=bats1.2.1)  # add frugivore factors
summary(batlinearmodel)

# R= 0.1228 means the model explains 12 procent of the data.
# The model plots how the fruit size changes in regard to the weight of bats,

##
ggplot(bats1.2.1,aes(log(fruit.size), log(Mass.g_phyl))) +
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

cor(log(bats1.2.1$fruit.size), log(bats1.2.1$Mass.g_phyl)) # 0.35 correlation
batlinearmodel1.1 <- lm(log(fruit.size) ~ log(Mass.g_phyl), data=bats1.2.1) 
summary(batlinearmodel1.1)

#Remove largest 3 bats weight values
bats1.2.2 <- bats1.2.1[-c(59,58,57),]
ggplot(bats1.2.2,aes(log(Mass.g_phyl), log(fruit.size))) +
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2")))

cor(log(bats1.2.2$Mass.g_phyl), log(bats1.2.2$fruit.size)) # 0.44 correlation
batlinearmodel2 <- lm(log(Mass.g_phyl) ~ log(fruit.size), data=bats1.2.2) 
summary(batlinearmodel2) 

# R= 0.20 means the model explains 20 procent of the data.

## bat weight with interaction vs all
bats2 <- bats[!duplicated(bats$animal.accepted.species), ] # bats with interaction
bats2.1 <- bats2
names(bats2.1)[names(bats2.1) == "animal.accepted.species"] <- "animal_accepted_species_fricke"

bats3 <- merge(mammals2, bats2.1, by=c("animal_accepted_species_fricke"))
bats3 <- subset(bats3, select = c(1,22),)
bats3 <- bats3[!duplicated(bats3$animal_accepted_species_fricke), ]
bats3 <- subset(bats3, Mass.g_phyl!="NA")
bats3$factor <- "BatsWithInteraction"

batsall <- subset(mammals, animal_group_fricke=="bats")
batsall <- batsall[!duplicated(batsall$animal_accepted_species_fricke), ]
batsall <- subset(batsall, select = c(2,22),) # use body mass prom phyl
batsall <- subset(batsall, Mass.g_phyl!="NA")
batsall$factor <- "allbats"

bats3batsall <- rbind(bats3,batsall)
bats3batsall$factor <- as.factor(bats3batsall$factor)
bats3batsall$Mass.g_phyl <- as.numeric(bats3batsall$Mass.g_phyl)
ggdensity(bats3batsall, x="Mass.g_phyl", add= "mean", rug= T, color= "factor", fill = "factor", palette = c("green","red"))


### Interaction between all Moraceae and all animals
unique(net.long.nat5$animal.accepted.species) # 656
dd <- aggregate(net.long.nat5$fruit.size, list(net.long.nat5$animal.accepted.species), FUN=mean)
names(dd)[names(dd) == "Group.1"] <- "animal.accepted.species"
names(dd)[names(dd) == "x"] <- "fruit.size"
net.long.nat5 <- 
dd <- merge(net.long.nat5.1, dd, by=c("animal.accepted.species"))

mammals2.1 <- mammals
names(mammals2.1)[names(mammals2.1) == "animal_accepted_species_fricke"] <- "animal.accepted.species"
net.long.nat6 <- merge(net.long.nat5, mammals2.1, by=c("animal.accepted.species"))
dd <- subset(net.long.nat6, select = c(1,3,4,13,63),)
dd2 <- aggregate(net.long.nat6$fruit.size, list(net.long.nat6$animal.accepted.species), FUN=mean)
names(dd2)[names(dd2) == "Group.1"] <- "animal.accepted.species"
names(dd2)[names(dd2) == "x"] <- "fruit.size"
dd <- dd[,-2]
dd3 <- merge(dd, dd2, by=c("animal.accepted.species"))
dd4 <- dd3[!duplicated(dd3$animal.accepted.species), ]
str(dd4)
dd4$animal.group <- as.factor(dd4$animal.group)
dd4$Mass.g_phyl <- as.numeric(dd4$Mass.g_phyl)
dd5 <- subset(dd4, Mass.g_phyl!="NA")

# plot the data
plot(log(dd5$Mass.g_phyl)~ log(dd5$fruit.size), col=dd5$animal.group, pch=19)
par(xpd=T)
legend("topright", inset=c(-0.1,-0.2), legend = levels(dd5$animal.group), pch=19, col=unique(dd5$animal.group))

mod<-lm(log(dd5$Mass.g_phyl)~ log(dd5$fruit.size))
summary(mod)
par(xpd=F)
abline(mod)

# plot the data ggplot
ggplot(dd5, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) # per animal.group

ggplot(dd5, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=`Continent 1`, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) # per continent

# per animal group per continent
# Africa
dd5Africa <- subset(dd5, dd5$`Continent 1` == "AFRICA")
ggplot(dd5Africa, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

dd5Abats <- subset(dd5Africa, animal.group == "bats")
dd5Amamm.carns <- subset(dd5Africa, animal.group == "mamm.carns")
dd5Amamm.herbs <- subset(dd5Africa, animal.group == "mamm.herbs")
dd5Amamm.others <-subset(dd5Africa, animal.group == "mamm.others")
dd5Aprimates <-subset(dd5Africa, animal.group == "primates")

dd5Abatslm<-lm(log(dd5Abats$Mass.g_phyl)~ log(dd5Abats$fruit.size))
dd5Amamm.carnslm <- lm(log(dd5Amamm.carns$Mass.g_phyl)~ log(dd5Amamm.carns$fruit.size))
dd5Amamm.herbslm <- lm(log(dd5Amamm.herbs$Mass.g_phyl)~ log(dd5Amamm.herbs$fruit.size))
dd5Amamm.otherslm <- lm(log(dd5Amamm.others$Mass.g_phyl)~ log(dd5Amamm.others$fruit.size))
dd5Aprimateslm <- lm(log(dd5Aprimates$Mass.g_phyl)~ log(dd5Aprimates$fruit.size))
summary(dd5Abatslm)
summary(dd5Amamm.carnslm)
summary(dd5Amamm.herbslm)
summary(dd5Amamm.otherslm)
summary(dd5Aprimateslm)

###############DELETE
TEST <- merge(dd5Amamm.others,net.long.nat5, by = "animal.accepted.species")

################
# Americas
dd5Americas <- subset(dd5, dd5$`Continent 1` == "AMERICAS")
ggplot(dd5Americas, aes(x=fruit.size, y=log(Mass.g_phyl), color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

dd5Nbats <- subset(dd5Americas, animal.group == "bats")
dd5Nmamm.carns <- subset(dd5Americas, animal.group == "mamm.carns")
dd5Nmamm.herbs <- subset(dd5Americas, animal.group == "mamm.herbs")
dd5Nmamm.others <-subset(dd5Americas, animal.group == "mamm.others")
dd5Nprimates <-subset(dd5Americas, animal.group == "primates")

dd5Nbatslm<-lm(log(dd5Nbats$Mass.g_phyl)~ log(dd5Nbats$fruit.size))
dd5Nmamm.carnslm <- lm(log(dd5Nmamm.carns$Mass.g_phyl)~ log(dd5Nmamm.carns$fruit.size))
dd5Nmamm.herbslm <- lm(log(dd5Nmamm.herbs$Mass.g_phyl)~ log(dd5Nmamm.herbs$fruit.size))
dd5Nmamm.otherslm <- lm(log(dd5Nmamm.others$Mass.g_phyl)~ log(dd5Nmamm.others$fruit.size))
dd5Nprimateslm <- lm(log(dd5Nprimates$Mass.g_phyl)~ log(dd5Nprimates$fruit.size))
summary(dd5Nbatslm)
summary(dd5Nmamm.carnslm)
summary(dd5Nmamm.herbslm)
summary(dd5Nmamm.otherslm)
summary(dd5Nprimateslm)
# Asia
dd5Asia <- subset(dd5, dd5$`Continent 1` == "ASIA")
ggplot(dd5Asia, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

dd5Ibats <- subset(dd5Asia, animal.group == "bats")
dd5Imamm.carns <- subset(dd5Asia, animal.group == "mamm.carns")
dd5Imamm.herbs <- subset(dd5Asia, animal.group == "mamm.herbs")
dd5Imamm.others <-subset(dd5Asia, animal.group == "mamm.others")
dd5Iprimates <-subset(dd5Asia, animal.group == "primates")

dd5Ibatslm<-lm(log(dd5Ibats$Mass.g_phyl)~ log(dd5Ibats$fruit.size))
dd5Imamm.carnslm <- lm(log(dd5Imamm.carns$Mass.g_phyl)~ log(dd5Imamm.carns$fruit.size))
dd5Imamm.herbslm <- lm(log(dd5Imamm.herbs$Mass.g_phyl)~ log(dd5Imamm.herbs$fruit.size))
dd5Imamm.otherslm <- lm(log(dd5Imamm.others$Mass.g_phyl)~ log(dd5Imamm.others$fruit.size))
dd5Iprimateslm <- lm(log(dd5Iprimates$Mass.g_phyl)~ log(dd5Iprimates$fruit.size))
summary(dd5Ibatslm)
summary(dd5Imamm.carnslm)
summary(dd5Imamm.herbslm)
summary(dd5Imamm.otherslm)
summary(dd5Iprimateslm)

library(ggplot2)
library(cowplot)

# Create the first plot
plot1 <- ggplot(dd5Africa, aes(x=log(Mass.g_phyl), y=fruit.size, color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=F) +
  labs(title = "Africa") +
  guides(color = FALSE, shape = FALSE)+
  labs(x = "Body Mass", y = "Fruit Size") +
  ylim(0, 45) +
  xlim(0, 16)

# Create the second plot
plot2 <- ggplot(dd5Americas, aes(x=log(Mass.g_phyl), y=fruit.size, color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=F) +
  labs(title = "Americas") +
  guides(color = FALSE, shape = FALSE) +
  labs(x = "Body Mass", y = "Fruit Size") +
  ylim(0, 45) +
  xlim(0, 16)

# Create the third plot
plot3 <- ggplot(dd5Asia, aes(x=log(Mass.g_phyl), y=fruit.size, color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=F) +
  labs(title = "Asia") +
  guides(color = FALSE, shape = FALSE) +
  labs(x = "Body Mass", y = "Fruit Size") +
  ylim(0, 45) +
  xlim(0, 16)

# Combine the three plots
combined_plot <- plot_grid(plot1, plot2, plot3, labels = c("A", "B", "C"), ncol = 3)

# Create a shared legend
legend <- get_legend(plot1)
combined_plot_with_legend <- plot_grid(
  combined_plot,
  legend,
  nrow = 2,
  rel_heights = c(8, 3)
)


# Display the combined plot with legend
print(combined_plot_with_legend)











# Assume you have two ggplot objects called plot1 and plot2

# Create your ggplot objects
plot1 <- ggplot(dd5, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=animal.group, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
plot2 <- ggplot(dd5, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=`Continent 1`, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# Combine the two plots
combined_plot <- grid.arrange(plot1, plot2, nrow = 1)

# Display the combined plot
print(combined_plot)


dd5bats <- subset(dd5, animal.group == "bats")
dd5mamm.carns <- subset(dd5, animal.group == "mamm.carns")
dd5mamm.herbs <- subset(dd5, animal.group == "mamm.herbs")
dd5mamm.others <-subset(dd5, animal.group == "mamm.others")
dd5primates <-subset(dd5, animal.group == "primates")

modbats <- lm(log(dd5bats$fruit.size)~ log(dd5bats$Mass.g_phyl))
summary(modbats)
modcarns <- lm(log(dd5mamm.carns$fruit.size)~ log(dd5mamm.carns$Mass.g_phyl))
summary(modcarns)
modherbs <- lm(log(dd5mamm.herbs$fruit.size)~ log(dd5mamm.herbs$Mass.g_phyl))
summary(modherbs)
modothers <- lm(log(dd5mamm.others$fruit.size)~ log(dd5mamm.others$Mass.g_phyl))
summary(modothers)
modprimates <- lm(log(dd5primates$fruit.size)~ log(dd5primates$Mass.g_phyl))
summary(modprimates)

#2 bat families

bats1.3 <- subset(bats, select = c(2,3,8,10))
bats1.3 <- bats1.3[!duplicated(bats1.3$animal.accepted.species), ]
names(bats1.3)[names(bats1.3) == "animal.accepted.species"] <- "animal_accepted_species_fricke"
bats1.3 <- merge(bats1.3, mammals, by="animal_accepted_species_fricke")
bats1.3 <- bats1.3[!duplicated(bats1.3$animal_accepted_species_fricke), ]
bats1.3 <- subset(bats1.3, select = c(1,2,3,4,25))
bats1.3 <- subset(bats1.3, Mass.g_phyl!="NA")
bats1.3$animal.family <- as.factor(bats1.3$animal.family)
bats1.3$Mass.g_phyl <- as.numeric(bats1.3$Mass.g_phyl)

ggplot(bats1.3, aes(x=log(fruit.size), y=log(Mass.g_phyl), color=animal.family, shape="cyl")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


## IUCN
mammals2$IUCN.Status.1.2_phyl <- as.factor(mammals2$IUCN.Status.1.2_phyl)
mammalsIUCN <- subset(mammals2, select = c(2,27))
mammalsIUCN <- mammalsIUCN[!duplicated(mammalsIUCN$animal_accepted_species_fricke), ]
names(mammalsIUCN)[names(mammalsIUCN) == "animal_accepted_species_fricke"] <- "animal.accepted.species"
mammalsIUCN2<- merge(net.long.nat5, mammalsIUCN, by=c("animal.accepted.species")) # 743 interactions with IUCN data between mammals and Moraceae
table(mammalsIUCN2$IUCN.Status.1.2_phyl)
# CR  DD  EN  LC  NA  NT  VU 
# 15   8  76 538  61  18  27 

# Amount of interactions (all species)
mammalsIUCN2.1 <- subset(mammalsIUCN2, select=c(2,3,43))
mammalsIUCN2.1$plant.accepted.species <- as.factor(mammalsIUCN2.1$plant.accepted.species)
table(mammalsIUCN2.1$plant.accepted.species)
IUCNinteractions <- read_excel("analysis/IUCNinteractions.xlsx")
IUCNinteractions2 <- merge(IUCNinteractions, Moraceae_only_species, by=c("plant.accepted.species"))
IUCNinteractions2 <- IUCNinteractions2[order(IUCNinteractions2$fruit.size,decreasing = F),]

ggplot(IUCNinteractions2, aes(x=plant.accepted.species, y=interaction)) +
  geom_point(stat="summary", fun="mean") +
  scale_x_discrete(limits=IUCNinteractions2$plant.accepted.species) # amount of interaction for Moraceae species in ascending fruit size.

ggplot(data=IUCNinteractions2, mapping=aes(fruit.size,interaction)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) 

# Amount of interactions when threatened species go extinct
mammalsIUCN2.2 <- subset(mammalsIUCN2.1, select=c(1,3))
mammalsIUCN2.2 <- mammalsIUCN2.2[!duplicated(mammalsIUCN2.2$plant.accepted.species), ]
IUCNinteractions3 <- merge(IUCNinteractions2, mammalsIUCN2.2, by=c("plant.accepted.species"))
table(IUCNinteractions3$IUCN.Status.1.2_phyl)
# CR DD EN LC NA NT VU 
# 1  1 18 77  2  2  5 

IUCNinteractions3.1 <- IUCNinteractions3

IUCNinteractions3.1 <-IUCNinteractions3.1[!(IUCNinteractions3.1$IUCN.Status.1.2_phyl=="CR"),]
IUCNinteractions3.1 <-IUCNinteractions3.1[!(IUCNinteractions3.1$IUCN.Status.1.2_phyl=="DD"),]
IUCNinteractions3.1 <-IUCNinteractions3.1[!(IUCNinteractions3.1$IUCN.Status.1.2_phyl=="EN"),]
IUCNinteractions3.1 <-IUCNinteractions3.1[!(IUCNinteractions3.1$IUCN.Status.1.2_phyl=="NA"),]
IUCNinteractions3.1 <-IUCNinteractions3.1[!(IUCNinteractions3.1$IUCN.Status.1.2_phyl=="VU"),]

ggplot(data=IUCNinteractions3.1, mapping=aes(fruit.size,interaction)) +
  geom_point()

ggplot(IUCNinteractions3.1, aes(x=plant.accepted.species, y=interaction)) +
  geom_point(stat="summary", fun="mean") +
  scale_x_discrete(limits=IUCNinteractions3.1$plant.accepted.species)

# compare interactions between all and only least concern
IUCNinteractions2.1 <- IUCNinteractions2
IUCNinteractions2.1$Factor <- "all"

IUCNinteractions3.2 <- IUCNinteractions3.1
IUCNinteractions3.2$Factor <- "LC"

IUCNinteractions3.2 <- IUCNinteractions3.2[,-4]
InteractionAllvsLC <- rbind(IUCNinteractions2.1,IUCNinteractions3.2)
ggdensity(InteractionAllvsLC, x="fruit.size", add= "mean", rug= T, color= "Factor", fill = "Factor", palette = c("green","red"))

ggplot(InteractionAllvsLC, aes(x=fruit.size, y=interaction, color=Factor, shape="cyl")) +
  geom_point()

ranges <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)
IUCNinteractions2.1$size_group <- cut(IUCNinteractions2.1$fruit.size, breaks = ranges, labels = c("0-5mm", "5-10mm", "10-15mm", "15-20mm", "20-25mm", "25-30mm", "30-35mm", "35-40mm", "40-45mm"))

IUCNinteractions2.1$size_group <- as.factor(IUCNinteractions2.1$size_group)

sum_by_factorALL <- aggregate(interaction ~ size_group, data = IUCNinteractions2.1, sum)


IUCNinteractions3.2$size_group <- cut(IUCNinteractions3.2$fruit.size, breaks = ranges, labels = c("0-5mm", "5-10mm", "10-15mm", "15-20mm", "20-25mm", "25-30mm", "30-35mm", "35-40mm", "40-45mm"))

IUCNinteractions3.2$size_group <- as.factor(IUCNinteractions3.2$size_group)

sum_by_factorLC <- aggregate(interaction ~ size_group, data = IUCNinteractions3.2, sum)

barplot(sum_by_factorALL$interaction, names.arg = levels(sum_by_factorALL$size_group))

sum_by_factorALL$IUCN <- "all"
sum_by_factorLC$IUCN <- "LC"
sum_by_factorALLLC<- rbind(sum_by_factorALL,sum_by_factorLC)

ggplot(data=sum_by_factorALLLC, aes(x=size_group, y=interaction, fill=IUCN)) +
  geom_bar(stat="identity", position=position_dodge())

percentageLC <- data.frame(
  range = (c("0-5", "5-10", '10-15', '15-20', '20-25', '25-30', '30-35', '35-40', '40-45')),
  percentage = c(41, 13, 12, 5, 35, 23, 25, 50, 20)
)



### Histogram for IUCN interaction
hist(InteractionAllvsLC$fruit.size)
hist(IUCNinteractions2.1$interaction)
# First distribution
hist(IUCNinteractions2.1, breaks=10, xlim=IUCNinteractions2.1$fruit.size, col=rgb(1,0,0,0.5), xlab="fruit.size", 
     ylab="interactions", main="distribution of height of 2 durum wheat varieties" )

# Second with add=T to plot on top
hist(Primadur, breaks=30, xlim=c(0,300), col=rgb(0,0,1,0.5), add=T)


# IUCN status of Moraceae with interactions.
library(taxize)
MoraceaeIUCN <- subset(IUCNinteractions2, select = c(plant.accepted.species))
API= "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
MoraceaeIUCN2 <- iucn_summary(MoraceaeIUCN$plant.accepted.species, distr_detail = F, key = API)
MoraceaeIUCN3 <- iucn_status(MoraceaeIUCN2) %>% as.data.frame() %>% 
  rownames_to_column(var = "plant.accepted.species") %>% set_names(c("plant.accepted.species", "IUCN"))

#IUCN status of all Moraceae
MoraceaeIUCN4 <- iucn_summary(Moraceae_only_species$plant.accepted.species, distr_detail = F, key = API)
MoraceaeIUCN5 <- iucn_status(MoraceaeIUCN4) %>% as.data.frame() %>% 
  rownames_to_column(var = "plant.accepted.species") %>% set_names(c("plant.accepted.species", "IUCN"))
str(MoraceaeIUCN5$IUCN)
table(MoraceaeIUCN5)
# fruit size against IUCN status for Moraceae
fruit.sizeIUCN <- merge(Moraceae_only_species, MoraceaeIUCN5, by=c("plant.accepted.species"))
fruit.sizeIUCN <- fruit.sizeIUCN[order(fruit.sizeIUCN$fruit.size,decreasing = F),]
fruit.sizeIUCN$IUCN <- as.factor(fruit.sizeIUCN$IUCN)

#
boxplot(fruit.size~IUCN,
        data=fruit.sizeIUCN,
        main="Fruit size in regard to IUCN",
        col="orange",
        border="brown",
        range = 0
)


### MAP OF THE WORLD
net.long.nat5world <- subset(net.long.nat5, animal.group %in% c("bats", "primates", "mamm.others", "birds", "mamm.herbs", "mamm.carns"))
# Load required libraries
library(ggplot2)
library(maps)
library(mapproj)

# Create a base map
world_map <- map_data("world")

# Define the countries to highlight
highlighted_countries <- c("Indonesia", "Thailand", "French Guiana", "Panama",
                           "Colombia", "Costa Rica", "Mexico", "Nigeria",
                           "Brazil", "China", "Trinidad and Tobago", "Bolivia",
                           "Peru", "India", "Sri Lanka", "Malaysia",
                           "Guatemala", "Bangladesh", "Madagascar", "South Africa",
                           "Philippines", "Taiwan", "Argentina", "Gabon",
                           "Uganda", "Papua New Guinea", "Australia", "Kenya",
                           "Malawi", "Ivory Coast", "Venezuela", "Cameroon",
                           "Seychelles", "Rwanda", "French Polynesia", "Trinidad and Tobago")

# Filter the world map data for the highlighted countries
highlighted_map <- subset(world_map, region %in% highlighted_countries)

# Define the coordinates to highlight
places <- net.long.nat5world[, c("locality.id", "latitude", "longitude")]

# Compute the count of rows for each locality.id
places$count <- table(places$locality.id)[match(places$locality.id, names(table(places$locality.id)))]
places2 <- places[!duplicated(places$locality.id), ]

# Plot the base map
p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray90", color = "gray40") +
  coord_fixed(1.3)

# Add the highlighted countries to the map
p <- p +
  geom_polygon(data = highlighted_map, aes(x = long, y = lat, group = group), fill = "green", color = "black") +
  geom_point(data = places2, aes(x = longitude, y = latitude), color = "blue", size = (places2$count)/50) +
  labs(title = "Documented Moraceae Interactions") +
  theme_minimal()

# Display the map
print(p)


















#### Map of native range of all Moraceae species ####
# Install and load the required packages
install.packages("leaflet")
install.packages("geojsonio")
library(leaflet)
library(geojsonio)

# Read the GeoJSON file
geojson <- geojson_read("analysis/wgsrpd-master/geojson/level3.geojson", )

# Create the map
map <- leaflet()

# Add the GeoJSON layer
map <- addGeoJSON(map, geojson = geojson, color = "red")

# Display the map
map


indices <- c(1:369)

# Create an empty dataframe
allbotcountry <- data.frame()

# Loop over the indices and extract the values
for (i in indices) {
  value <- geojson[["features"]][[i]][["properties"]][["LEVEL3_NAM"]]
  allbotcountry <- rbind(allbotcountry, value)
}

# Rename the dataframe column
colnames(allbotcountry) <- "LEVEL3_NAM"




######################## Antiaris Toxicara
# Create a dataframe with the botanical countries
Antiaris_toxicaria<- data.frame(
  LEVEL3_NAM = c("Andaman Is.", "Angola", "Benin", "Bismarck Archipelago", "Borneo", "Burkina",
              "Burundi", "Cambodia", "Cameroon", "Central African Republic", "Chad",
              "China South-Central", "China Southeast", "Congo", "Ethiopia", "Gabon",
              "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Gulf of Guinea Is.", "Hainan",
              "India", "Ivory Coast", "Jawa", "Kenya", "Laos", "Lesser Sunda Is.", "Liberia",
              "Madagascar", "Malaya", "Mali", "Maluku", "Myanmar", "New Guinea", "Nigeria",
              "Northern Territory", "Queensland", "Rwanda", "Santa Cruz Is.", "Senegal",
              "Sierra Leone", "Solomon Is.", "Sri Lanka", "Sudan", "Sulawesi", "Sumatera",
              "Tanzania", "Thailand", "Togo", "Uganda", "Vanuatu", "Vietnam", "Yemen",
              "Zambia", "Zaïre")
)

# Print the dataframe
print(botanical_countries)





# Create a vector of the botanical countries
botanical_countries <- c("Andaman Is.", "Angola", "Benin", "Bismarck Archipelago", "Borneo",
                         "Burkina", "Burundi", "Cambodia", "Cameroon", "Central African Republic",
                         "Chad", "China South-Central", "China Southeast", "Congo", "Ethiopia",
                         "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
                         "Gulf of Guinea Is.", "Hainan", "India", "Ivory Coast", "Jawa",
                         "Kenya", "Laos", "Lesser Sunda Is.", "Liberia", "Madagascar",
                         "Malaya", "Mali", "Maluku", "Myanmar", "New Guinea", "Nigeria",
                         "Northern Territory", "Queensland", "Rwanda", "Santa Cruz Is.",
                         "Senegal", "Sierra Leone", "Solomon Is.", "Sri Lanka", "Sudan",
                         "Sulawesi", "Sumatera", "Tanzania", "Thailand", "Togo", "Uganda",
                         "Vanuatu", "Vietnam", "Yemen", "Zambia", "Zaïre")

# Filter the GeoJSON dataframe based on the botanical countries
filtered_data <- data[grepl(paste(botanical_countries, collapse = "|"), 
                            data[["features"]][[1]][["properties"]][["LEVEL3_NAM"]]), ]

# Print the filtered dataframe
print(filtered_data)





library(leaflet)
library(geojsonio)

# Read the GeoJSON file
geojson <- geojson_read("analysis/wgsrpd-master/geojson/level3.geojson")

# Specify the botanical countries where Antiaris toxicaria is native
native_countries <- Antiaris_toxicara

# Filter the GeoJSON data to include only the native range of Antiaris toxicaria
native_range <- geojson[geojson$botanical_country %in% native_countries, ]

# Create the map
map <- leaflet()

# Add the filtered GeoJSON layer for the native range
map <- addGeoJSON(map, geojson = native_range, color = "red")

# Display the map
map



# Filter the GeoJSON data for Alberta
filtered_data <- geojson$features[[1:3]]$properties$LEVEL3_NAM == "Alberta", ]

# Create a leaflet map
m <- leaflet() %>% addTiles()

# Add the filtered GeoJSON data to the map
m <- m %>% addGeoJSON(data = filtered_data)

# Display the map
m







# Load the required package
library(rgdal)

# Set the path to the Shapefile (without file extension)
shapefile_path <- "analysis/wgsrpd-master/level3/level3.shp"

# Read the Shapefile
map_data <- readOGR(dsn = shapefile_path, layer = basename(shapefile_path))

# Print summary information about the map data
summary(map_data)





# Load the required package
library(sf)
library(ggplot2)
# Set the path to the Shapefile (without file extension)
shapefile_path <- "analysis/wgsrpd-master/level3/level3.shp"

# Read the Shapefile
map_data <- st_read(dsn = shapefile_path)

# Print summary information about the map data
summary(map_data)
plot(map_data)

# Convert sf object to data.frame
map_data_df <- as.data.frame(map_data)

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = map_data)





# Load the required packages
library(sf)
library(ggplot2)

# Set the path to the Shapefile (without file extension)
shapefile_path <- "analysis/wgsrpd-master/level3/level3.shp"

# Read the Shapefile
map_data <- st_read(dsn = shapefile_path)

# Create a new column in the map_data indicating if a country is in the native range or not
map_data$is_native_range <- ifelse(map_data[["LEVEL3_NAM"]] %in% Antiaris_toxicaria[["LEVEL3_NAM"]], "Native Range", "Other")

# Plot the map with desired colors
ggplot() +
  geom_sf(data = map_data, aes(fill = is_native_range), color = "black") +
  scale_fill_manual(values = c("green", "white"),
                    breaks = c("Native Range", "Other"),
                    labels = c("Native Range", "Other")) +
  theme_minimal()





# Load the required packages
library(sf)
library(ggplot2)
library(dplyr)

# Set the path to the Shapefile (without file extension)
shapefile_path <- "analysis/wgsrpd-master/level3/level3.shp"

# Read the Shapefile
map_data <- st_read(dsn = shapefile_path)

# Define the column names for the botanical countries in the Shapefile and the native range data
botanical_countries_col <- "LEVEN3_NAM"
native_range_col <- "Antiaris_toxicaria"  # Assuming this column contains the native range information

# Create a new column in the map_data indicating if a country is in the native range or not for Antiaris_toxicaria
map_data$is_native_range_antiaris <- ifelse(map_data[[botanical_countries_col]] %in% Antiaris_toxicaria[[native_range_col]], "Antiaris_toxicaria", "Other")

# Load the data for the additional Moraceae species
other_species_data <- read.csv("path/to/other/species/data.csv")  # Replace with the path to your data file

# Calculate the frequency of occurrence of each native range category
native_range_freq <- other_species_data %>%
  group_by(Native_Range_Category) %>%
  summarise(Frequency = n())

# Merge the frequency data with the map data
map_data <- left_join(map_data, native_range_freq, by = c("LEVEL3_NAM" = "Native_Range_Category"))

# Set the color palette for each native range category
color_palette <- c("green", "red", "blue")

# Plot the map with colors based on the native range frequency
ggplot() +
  geom_sf(data = map_data, aes(fill = Frequency), color = "black") +
  scale_fill_gradientn(colors = color_palette, na.value = "white", guide = "legend",
                       breaks = seq(min(native_range_freq$Frequency), max(native_range_freq$Frequency), length.out = length(color_palette)),
                       labels = seq(min(native_range_freq$Frequency), max(native_range_freq$Frequency), length.out = length(color_palette))) +
  theme_minimal()















# Load the required packages
library(sf)
library(ggplot2)
library(dplyr)

# Set the path to the Shapefile (without file extension)
shapefile_path <- "/path/to/your/shapefile"

# Read the Shapefile
map_data <- st_read(dsn = shapefile_path)

# Define the column names for the botanical countries in the Shapefile and the native range data
botanical_countries_col <- "LEVEN3_NAM"
native_range_col <- "Antiaris_toxicaria"  # Assuming this column contains the native range information

# Create a new column in the map_data indicating if a country is in the native range or not for Antiaris_toxicaria
map_data$is_native_range_antiaris <- ifelse(map_data[[botanical_countries_col]] %in% Antiaris_toxicaria[[native_range_col]], "Antiaris_toxicaria", "Other")

# Load the data for the additional Moraceae species
other_species_data <- read.csv("path/to/other/species/data.csv")  # Replace with the path to your data file

# Merge the additional species data with the map data
map_data <- left_join(map_data, other_species_data, by = c("LEVEL3_NAM" = "Botanical_Country"))

# Define the native range categories and their corresponding colors
native_range_categories <- c("Neotropics", "Afrotropics", "Indo-Malay region")
category_colors <- c("green", "red", "blue")

# Plot the map with colors based on the native range category
ggplot() +
  geom_sf(data = map_data, aes(fill = Native_Range_Category), color = "black") +
  scale_fill_manual(values = category_colors, 
                    breaks = native_range_categories,
                    labels = native_range_categories) +
  theme_minimal()



library(writexl)
write_xlsx(Moraceae_only_species2, "Moraceae_399.xlsx")



Moraceae_nativerange <- read_excel("analysis/Moraceae_nativerange.xlsx")
View(Moraceae_nativerange)


# Get a vector of all the region values in the dataframe
regions <- unlist(Moraceae_nativerange)

# Count the occurrences of each region
region_counts <- table(regions)

# Display the region counts
print(region_counts)

# Create a new dataframe with the regions and counts
result <- as.data.frame(region_counts)

# Rename the columns
colnames(result) <- c("Region", "Count")





# Load the required packages
library(sf)
library(ggplot2)

# Set the path to the Shapefile (without file extension)
shapefile_path <- "analysis/wgsrpd-master/level3/level3.shp"

# Read the Shapefile
map_data <- st_read(dsn = shapefile_path)

# Calculate the count of each region
region_counts <- table(map_data[["LEVEL3_NAM"]])

# Create a new column in the map_data with the count for each region
map_data$count <- region_counts[map_data[["LEVEL3_NAM"]]]
map_data$count <- ifelse(is.na(map_data$count), 0, map_data$count)

# Normalize the count values between 0 and 1
min_count <- min(map_data$count)
max_count <- max(map_data$count)
map_data$normalized_count <- (map_data$count - min_count) / (max_count - min_count)

# Plot the map with desired colors
ggplot() +
  geom_sf(data = map_data, aes(fill = count), color = "black") +
  scale_fill_gradient(low = "white", high = "#72994C", name = "Native Species") +
  theme_minimal()



