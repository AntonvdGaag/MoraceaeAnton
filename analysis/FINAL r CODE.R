library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("FSA")
library(FSA)
## interactions Moraceae with birds and mammals

load(file = "./analysis/homogenization.RData")

# Get a version of net.long with only native species, call it net.long.nat
net.long.nat <- subset(net.long, plant.native.status == "native" & animal.native.status == "native")

# Remove all non interactions (0) from the dataframe
net.long.nat2<-subset(net.long.nat, value!="0")

# Remove all non Moraceae family species
net.long.nat3<-subset(net.long.nat2, plant.family=="Moraceae")

## Load Moraceae species into dataframe
library(readxl)
Moraceae_all_species <- read_excel("analysis/Moraceae_spreadsheet_for_data_scoringv2(1).xlsx")
names(Moraceae_all_species)[names(Moraceae_all_species) == "Plant_species"] <- "plant.accepted.species"
Moraceae_all_species$plant.accepted.species <- gsub('_', 
                                                    ' ', 
                                                    Moraceae_all_species$plant.accepted.species)
Moraceae_only_species = subset(Moraceae_all_species, select = c(plant.accepted.species,fruit.size))
Moraceae_only_species <- Moraceae_only_species[-c(403,404,405,406,407,408,409,410),]

Moraceae_spatial <- subset(Moraceae_all_species, select = c(plant.accepted.species,fruit.size,`Continent 1`))
Moraceae_spatial$fruit.size <- as.numeric(Moraceae_spatial$fruit.size)
Moraceae_spatial <- Moraceae_spatial[-c(344,348,114),] #delete 3 species without values
Moraceae_spatial$`Continent 1` <- as.factor(Moraceae_spatial$`Continent 1`)# Continent 1 needs to be a factor
Moraceae_spatial <-Moraceae_spatial[-c(400,401,402,403,404,405,406,407),]


# Merge interaction dataframe with our specieslist
net.long.nat5 <- merge(Moraceae_spatial, net.long.nat3, by=c("plant.accepted.species"))


# Show fruit range per Moraceae genus
Moraceae_spatial1.1 <- Moraceae_spatial
Moraceae_spatial1.1$plant.accepted.genus <- gsub("^(\\w+).*", "\\1", Moraceae_spatial1.1$plant.accepted.species)
unique(Moraceae_spatial1.1$plant.accepted.genus)
  # 36 genera

# graph showing the 36 genera with their fruit sizes.
ggplot(Moraceae_spatial1.1, aes(x = fruit.size, y = plant.accepted.genus)) +
  geom_point(aes(color = cut(fruit.size, breaks = c(0, 25, 70, Inf))), position = "identity", alpha = 0.5) +
  labs(x = "fruit size", y = "genus", color = "fruit size category") +
  scale_color_manual(values = c("green", "blue", "red"), labels = c("small", "medium", "large")) +
  scale_x_continuous(breaks = seq(0, max(Moraceae_spatial1.1$fruit.size), 25), limits = c(0, NA)) +
  theme_minimal()




# Get the unique values and their counts
unique_counts <- table(Moraceae_spatial1.1$plant.accepted.genus)

# Convert the result into a dataframe
result <- data.frame(Genus = names(unique_counts), Count = as.vector(unique_counts))

# Set the file path and name for the Excel file
excel_file <- "unique_values_with_counts.xlsx"

# Write the dataframe to the Excel file
write.xlsx(result, file = excel_file, sheetName = "Unique Values with Counts", row.names = FALSE)


# Birds measurments
McFadden <- read_excel("analysis/McFadden-et-al_Avian-frugivore-traits_V2.xlsx", sheet=2)
McFadden2 <- subset(McFadden, select = c(1,13))
names(McFadden2)[names(McFadden2) == "Species"] <- "animal.accepted.species"

#Merge data sets with gape width
Birds <- subset(net.long.nat5, animal.group == "birds")
Birds2 <- merge(Birds, McFadden2, by=c("animal.accepted.species"))
names(Birds2)[names(Birds2) == "Gape Size"] <- "gape.size"

ggplot(Birds2, aes(x = Birds2[, 43], y = fruit.size, color = Birds2[, 4])) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()

plot4 <- ggplot(Birds2[Birds2[, 4] == "AFRICA", ], aes(x = gape.size, y = fruit.size)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()

plot5 <- ggplot(Birds2[Birds2[, 4] == "AMERICAS", ], aes(x = gape.size, y = fruit.size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()

plot6 <- ggplot(Birds2[Birds2[, 4] == "ASIA", ], aes(x = gape.size, y = fruit.size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()

# Combine the three plots
combined_plot2 <- plot_grid(plot4, plot5, plot6, labels = c("Africa", "America", "Asia"), ncol = 3)

# Create a shared legend
legend2 <- get_legend(plot4)
combined_plot_with_legend2 <- plot_grid(
  combined_plot2,
  legend2,
  nrow = 2,
  rel_heights = c(8, 3)
)

print(combined_plot_with_legend2)





# Split the dataset by continent
continent_groups <- split(Birds2, Birds2[, 4])

# Create an empty list to store the linear models
linear_models <- list()

# Iterate over each continent subset and fit the linear model
for (continent in names(continent_groups)) {
  subset_data <- continent_groups[[continent]]
  linear_models[[continent]] <- lm(fruit.size ~ gape.size, data = subset_data)
}

summary(linear_models$AFRICA)
summary(linear_models$AMERICAS)
summary(linear_models$ASIA)
summary(linear_models$OCEANIA)

avg_data_birds2 <- avg_data_birds
avg_data_birds2$avg_gape2 <- avg_data_birds2$avg_gape / 2

avg_data_birds <- Birds2 %>%
  group_by(animal.accepted.species, `Continent 1`) %>%
  summarize(avg_fruit = mean(fruit.size),
            avg_gape = mean(gape.size))

plot7 <- ggplot(avg_data_birds[avg_data_birds[, 2] == "AFRICA", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds[avg_data_birds[, 2] == "AFRICA", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

plot8 <- ggplot(avg_data_birds[avg_data_birds[, 2] == "AMERICAS", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds[avg_data_birds[, 2] == "AMERICAS", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

plot9 <- ggplot(avg_data_birds[avg_data_birds[, 2] == "ASIA", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds[avg_data_birds[, 2] == "ASIA", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

# Combine the three plots
combined_plot3 <- plot_grid(plot7, plot8, plot9, labels = c("Africa", "America", "Asia"), ncol = 3)

# Create a shared legend
legend3 <- get_legend(plot4)
combined_plot_with_legend3 <- plot_grid(
  combined_plot3
)
print(combined_plot_with_legend3)


# Split the dataset by continent
continent_groups2 <- split(avg_data_birds, avg_data_birds[, 2])

# Create an empty list to store the linear models
linear_models2 <- list()

# Iterate over each continent subset and fit the linear model
for (continent in names(continent_groups2)) {
  subset_data2 <- continent_groups2[[continent]]
  linear_models2[[continent]] <- lm(avg_fruit ~ avg_gape, data = subset_data2)
}

summary(linear_models2$AFRICA)
summary(linear_models2$AMERICAS)
summary(linear_models2$ASIA)
summary(linear_models2$OCEANIA)

## Mammals interaction vs all body size
# All
mammals <- read_excel("analysis/traits_mammals_species_level(1).xlsx")
mammals <-  subset(mammals, freq_fricke!="0") 

mammals1.1 <- mammals[!duplicated(mammals$animal_accepted_species_fricke), ]
mammals1.1$Mass.g_phyl <- as.numeric(mammals1.1$Mass.g_phyl)
mammals1.1 <- subset(mammals1.1, Mass.g_phyl!="NA")

ggplot(mammals1.1, aes(x = log10(Mass.g_phyl), y = animal_group_fricke)) +
  geom_jitter(height = 0.3, width = 0.2, alpha = 0.7) +
  labs(x = "log(Mass.g_phyl)", y = "") +
  theme_bw() +
  scale_x_continuous(limits = c(min(log10(mammals1.1$Mass.g_phyl)), max(log10(mammals1.1$Mass.g_phyl))))

#Interaction
dd5.1 <- dd5
dd5.1 <- dd5.1 %>% mutate(animal.group = recode(animal.group, "primates" = "primates2", 
                                                "mamm.others" = "mamm.others2", 
                                                "mamm.herbs" = "mamm.herbs2", 
                                                "mamm.carns" = "mamm.carns2", 
                                                "bats" = "bats2"))

ggplot(dd5.1, aes(x = log10(Mass.g_phyl), y = animal.group)) +
  geom_jitter(height = 0.3, width = 0.2, alpha = 0.7) +
  labs(x = "log(Mass.g_phyl)", y = "") +
  theme_bw() +
  scale_x_continuous(limits = c(min(log10(dd5.1$Mass.g_phyl)), max(log10(dd5.1$Mass.g_phyl))))

# all vs interaction (mammals)
mammals1.1 <- mammals1.1[, c("animal_accepted_species_fricke","animal_group_fricke", "Mass.g_phyl")]
colnames(mammals1.1)[1] <- "animal.accepted.species"
colnames(mammals1.1)[2] <- "animal.group"

dd5.1 <- dd5.1[, c("animal.accepted.species", "animal.group","Mass.g_phyl")]
mammalsdd <- rbind(mammals1.1,dd5.1)
mammalsdd$animal.group <- as.factor(mammalsdd$animal.group)

ggplot(mammalsdd, aes(x = log10(Mass.g_phyl), y = animal.group)) +
  geom_jitter(height = 0.2, width = 0.1, alpha = 0.7) +
  labs(x = "log(Mass.g_phyl)", y = "") +
  geom_boxplot(alpha = 0.3, fill = "black", color = "red") +
  theme_bw() +
  scale_x_continuous(limits = c(min(log10(mammalsdd$Mass.g_phyl)), max(log10(mammalsdd$Mass.g_phyl)+0.5)))

## 399 species vs interaction Moraceae species.
# 399
Moraceae_spatial2.1 <- Moraceae_spatial
Moraceae_spatial2.1$Factor <- "all"

#interaction
Morint <- net.long.nat5
Morint <- Morint[!duplicated(Morint$plant.accepted.species), ]
Morint$Factor <- "Int"
Morint <- Morint[, c("plant.accepted.species", "fruit.size", "Continent 1", "Factor")]

# 399 vs interaction
int399 <- rbind(Moraceae_spatial2.1,Morint)
int399$Factor <- as.factor(int399$Factor)


ggplot(int399, aes(x = log(fruit.size), y = Factor)) +
  geom_jitter(height = 0.2, width = 0.1, alpha = 0.7) +
  labs(x = "fruit.size", y = "") +
  geom_boxplot(alpha = 0.3, fill = "black", color = "red") +
  theme_bw() +
  scale_x_continuous(limits = c(min(log(int399$fruit.size)), max(log(int399$fruit.size)) + 0.5))









# Create the bird species vector
birds5 <- c("Species A", "Species B", "Species C", "Species D", "Species E", "Species F", "Species G", "Species H", "Species I", "Species J")

# Create the avg_fruit vector
avg_fruit <- c(10, 20, 30, 40, 50, 60, 15, 25, 35, 45)

# Create the third column vector
column3 <- c(11, 19, 34, 42, 50, 69, 12, 20, 32, 40)

# Create the dataframe
df10 <- data.frame(Birds = birds5, Avg_Fruit = avg_fruit, Column3 = column3)


ggplot(df10, aes(x = Column3, y = Avg_Fruit)) +
  geom_point() +
  geom_smooth(data = df10, aes(x = Column3, y = Avg_Fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()

lmdf10<-lm(log(df10$Column3)~ log(df10$Avg_Fruit))
summary(lmdf10)


#IUCN check if threatened species are the largest.

IUCNlarge <- subset(mammals1.1, select=c(2,22,27))
IUCNlarge$IUCN.Status.1.2_phyl <- as.factor(IUCNlarge$IUCN.Status.1.2_phyl)

ggplot(IUCNlarge, aes(x = log10(Mass.g_phyl), y = IUCN.Status.1.2_phyl)) +
  geom_jitter(height = 0.3, width = 0.2, alpha = 0.7) +
  labs(x = "log(Mass.g_phyl)", y = "") +
  geom_boxplot(alpha = 0.3, fill = "black", color = "red")
  theme_bw() +
  scale_x_continuous(limits = c(min(log10(IUCNlarge$Mass.g_phyl)), max(log10(IUCNlarge$Mass.g_phyl))))

# Birds IUCN status
  library(taxize)
  BirdsIUCN <- subset(Birds2, !duplicated(animal.accepted.species))
  BirdsIUCN <- subset(BirdsIUCN, select = c(animal.accepted.species)) 
  
   API= "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
   BirdsIUCN2 <- iucn_summary(BirdsIUCN$animal.accepted.species, distr_detail = F, key = API)
   BirdsIUCN3 <- iucn_status(BirdsIUCN2) %>% as.data.frame() %>% 
    rownames_to_column(var = "animal.accepted.species") %>% set_names(c("animal.accepted.species", "IUCN"))
   BirdsIUCN3[49, 2] <- "LC"
   BirdsIUCN3[71, 2] <- "NT"
   BirdsIUCN3[97, 2] <- "NT"
   BirdsIUCN3[107, 2] <- "LC"
   BirdsIUCN3[128, 2] <- "LC"
   BirdsIUCN3[129, 2] <- "NA"
   BirdsIUCN3[132, 2] <- "NT"
   BirdsIUCN3[133, 2] <- "LC"
   BirdsIUCN3[134, 2] <- "NT"
   BirdsIUCN3[138, 2] <- "NT"
   BirdsIUCN3[142, 2] <- "NT"
   BirdsIUCN3[179, 2] <- "LC"
   BirdsIUCN3[180, 2] <- "NA"
   BirdsIUCN3[181, 2] <- "LC"
   BirdsIUCN3[182, 2] <- "LC"
   BirdsIUCN3[183, 2] <- "NA"
   BirdsIUCN3[184, 2] <- "LC"
   BirdsIUCN3[185, 2] <- "LC"
   
   
## Combining IUCN status of mammals and birds wit Moraceae for interactions
# First combine IUCN stats of mammals and birds
mammals1.2 <- mammals1.1[, c("animal_accepted_species_fricke", 
                             "IUCN.Status.1.2_phyl")]
colnames(mammals1.2)[1] <- "animal.accepted.species"     
colnames(mammals1.2)[2] <- "IUCN"  
BM.IUCN <- rbind(BirdsIUCN3,mammals1.2)
net.long.nat5.5 <- merge(net.long.nat5,BM.IUCN, by=c("animal.accepted.species"))
net.long.nat5.5 <- net.long.nat5.5[, c("animal.accepted.species", 
                             "plant.accepted.species",
                             "fruit.size",
                             "Continent 1",
                             "IUCN"
                             )]
net.long.nat5.5$IUCN <- as.factor(net.long.nat5.5$IUCN)


net.long.nat5.5 <- net.long.nat5.5[!duplicated(net.long.nat5.5[c("plant.accepted.species", "animal.accepted.species")]), ]

net.long.nat5.5$AllInteraction <- ave(net.long.nat5.5$animal.accepted.species, 
                                            net.long.nat5.5$plant.accepted.species, 
                                            FUN = function(x) length(unique(x))
                                            )
net.long.nat5.5$ExtInteraction <- ave(ifelse(net.long.nat5.5$IUCN %in% c("LC", "NT"), 1, 0), 
                                           net.long.nat5.5$plant.accepted.species, 
                                           FUN = sum
                                           )
IUCNmodel <- lm(AllInteraction ~ fruit.size, data = net.long.nat5.5)
IUCNmodel2 <- lm(ExtInteraction ~ fruit.size, data = net.long.nat5.5)
summary(IUCNmodel) 
summary(IUCNmodel2)

plot(net.long.nat5.5$fruit.size, net.long.nat5.5$AllInteraction, xlab = "Fruit Size", 
     ylab = "Interactions", pch = 16, 
     col = "black"
       )
abline(IUCNmodel, 
       col = "red"
         )

plot(net.long.nat5.5$fruit.size, net.long.nat5.5$ExtInteraction, 
     xlab = "Fruit Diameter", 
     ylab = "Residuals", 
     pch = 16, 
     col = "black"
       )
abline(IUCNmodel2, 
       col = "red"
)

# Plot Interactions vs. Fruit Size
plot(net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]$fruit.size, 
     net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]$AllInteraction,
     xlab = "Fruit Size", ylab = "Interactions", pch = 16, col = "darkblue",
     main = "All Interactions vs Extinction",
     xlim = c(0,45),
     ylim= c(0,35)
     )
points(net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]$fruit.size, 
       net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]$ExtInteraction, 
       pch = 16, col = "blue")

plot(net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]$fruit.size, 
     net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]$AllInteraction,
     xlab = "Fruit Size", ylab = "Interactions", pch = 16, col = "darkred",
     main = "All Interactions vs Extinction",
     xlim = c(0,45),
     ylim= c(0,35)
)

points(net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]$fruit.size, 
       net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]$ExtInteraction, 
       pch = 16, col = "red")

plot(net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]$fruit.size, 
     net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]$AllInteraction,
     xlab = "Fruit Size", ylab = "Interactions", pch = 16, col = "darkgreen",
     main = "All Interactions vs Extinction",
     xlim = c(0,45),
     ylim= c(0,35)
)

points(net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]$fruit.size, 
       net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]$ExtInteraction, 
       pch = 16, col = "green")





IUCNmodel <- lm(AllInteraction ~ fruit.size, 
                data = net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]
                )
IUCNmodel2 <- lm(AllInteraction ~ fruit.size, 
                 data = net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]
                 )
IUCNmodel3 <- lm(AllInteraction ~ fruit.size, 
                 data = net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]
)
summary(IUCNmodel)
summary(IUCNmodel2)
summary(IUCNmodel3)
abline(IUCNmodel, 
       col = "darkblue"
)
abline(IUCNmodel2, 
       col = "darkred"
)
abline(IUCNmodel3, 
       col = "darkgreen"
)

IUCNmodel4 <- lm(ExtInteraction ~ fruit.size, 
                data = net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ]
)
IUCNmodel5 <- lm(ExtInteraction ~ fruit.size, 
                 data = net.long.nat5.5[net.long.nat5.5[, 4] == "AMERICAS", ]
)
IUCNmodel6 <- lm(ExtInteraction ~ fruit.size, 
                 data = net.long.nat5.5[net.long.nat5.5[, 4] == "ASIA", ]
)
summary(IUCNmodel4)
summary(IUCNmodel5)
summary(IUCNmodel6)
abline(IUCNmodel4, 
       col = "blue"
)
abline(IUCNmodel5, 
       col = "red"
)
abline(IUCNmodel6, 
       col = "green"
)





net.long.nat5.5$fruit_category <- cut(net.long.nat5.5$fruit.size, breaks = c(0, 24, 74, 100),
                           labels = c("small", "medium", "large"),
                           include.lowest = TRUE)


plot4 <- ggplot(net.long.nat5.5[net.long.nat5.5[, 4] == "AFRICA", ], aes(x = gape.size, y = fruit.size)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal()




Moraceaetest <- merge(MoraceaeIUCN5,Moraceae_only_species, by = c("plant.accepted.species"))
Moraceaetest$IUCN <- as.factor(Moraceaetest$IUCN)
Moraceaetest$IUCN[Moraceaetest$IUCN == "LR/nt"] <- "NT"
Moraceaetest$IUCN[Moraceaetest$IUCN == "LR/lc"] <- "LC"
Moraceaetest$IUCN[Moraceaetest$IUCN == "NA"] <- "NA"

ggplot(Moraceaetest, aes(x = log(fruit.size), y = IUCN)) +
  geom_jitter(height = 0.3, width = 0.2, alpha = 0.7) +
  geom_boxplot(alpha = 0.3, fill = "black", color = "red") +
  labs(x = "fruit size", y = "") +
  theme_bw() 
table(Moraceaetest$IUCN)
#CR    DD    EN    LC    NT    VU    NA
#1     2     7     233   4     8     144

### Birds extinction event, new model
Birds3 <- merge(Birds2,BirdsIUCN3, by = "animal.accepted.species")

avg_data_birds3 <- Birds3 %>%
  group_by(animal.accepted.species, `Continent 1`, IUCN) %>%
  summarize(avg_fruit = mean(fruit.size),
            avg_gape = mean(gape.size))
avg_data_birds3 <- avg_data_birds3[avg_data_birds3$IUCN == "LC", ]


plot20 <- ggplot(avg_data_birds3[avg_data_birds3[, 2] == "AFRICA", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds3[avg_data_birds3[, 2] == "AFRICA", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

plot21 <- ggplot(avg_data_birds3[avg_data_birds3[, 2] == "AMERICAS", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds3[avg_data_birds3[, 2] == "AMERICAS", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

plot22 <- ggplot(avg_data_birds3[avg_data_birds3[, 2] == "ASIA", ], aes(x = avg_gape, y = avg_fruit)) +
  geom_point() +
  geom_smooth(data = avg_data_birds3[avg_data_birds3[, 2] == "ASIA", ], aes(x = avg_gape, y = avg_fruit),
              method = 'lm', se = FALSE, color = "blue") +
  labs(x = "Gape Size", y = "Fruit Size") +
  theme_minimal() +
  ylim(0, 32) +
  xlim(0, 60)

# Combine the three plots
combined_plot4 <- plot_grid(plot20, plot21, plot22, labels = c("Africa", "America", "Asia"), ncol = 3)

# Create a shared legend
legend3 <- get_legend(plot4)
combined_plot_with_legend4 <- plot_grid(
  combined_plot4
)
print(combined_plot_with_legend4)


# Split the dataset by continent
continent_groups2 <- split(avg_data_birds, avg_data_birds[, 2])

# Create an empty list to store the linear models
linear_models2 <- list()

# Iterate over each continent subset and fit the linear model
for (continent in names(continent_groups2)) {
  subset_data2 <- continent_groups2[[continent]]
  linear_models2[[continent]] <- lm(avg_fruit ~ avg_gape, data = subset_data2)
}

summary(linear_models2$AFRICA)
summary(linear_models2$AMERICAS)
summary(linear_models2$ASIA)
summary(linear_models2$OCEANIA)


################ Kurkal Wallis
# Install and load the "dunn.test" package for Dunn test
install.packages("dunn.test")
library(dunn.test)

# Perform Kruskal-Wallis test
result_kruskal <- kruskal.test(fruit.size ~ `Continent 1`, data = Moraceae_spatial)

# Perform Dunn test for pairwise comparisons
result_dunn <- dunnTest(fruit.size ~ `Continent 1`, data = Moraceae_spatial, method = "bonferroni")

# Print the Kruskal-Wallis test result
print(result_kruskal)

# Print the Dunn test result
print(result_dunn)

