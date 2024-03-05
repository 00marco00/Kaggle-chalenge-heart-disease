setwd("C:/Users/marco/OneDrive/Documents/GitHub/Kaggle-chalenge-heart-disease")
data <- read.csv("heart.csv", sep = ",")
#print( head(data))


data$Sex <- ifelse(data$Sex == "M", 1, 0)
data$RestingECG <- ifelse(data$RestingECG == "Normal",1,0)
data$ChestPainType <- ifelse(data$ChestPainType == "ATA", 0,
                                    ifelse(data$ChestPainType == "NAP", 1, 2))
data$ExerciseAngina <- ifelse(data$ExerciseAngina == "Y",1,0)
data$ST_Slope <- ifelse(data$ST_Slope == "Up", 0,
                        ifelse(data$ST_Slope == "Flat", 1, 2))
#print( head(data))
pairs(data, main = "Matrice de Scatter Plots")

data_mat <- cor(data)
#print(data_mat)


###################################################################################

counts1 <- table(data$Sex)

# Renommer les étiquettes pour plus de clarté
names(counts1) <- c("Femme", "Homme")

# Créer l'histogramme
barplot(counts1, 
        main = "Répartition Homme/Femme des décès par crise cardiaque",
        xlab = "Sexe",
        ylab = "Nombre d'individus",
        col = c("pink", "lightblue"))
legend("topright", legend = counts1, fill = c("pink", "lightblue"))


modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))

data_heart_d <- subset(data, HeartDisease == 1)
# Calculer le nombre d'individus par sexe (en supposant que 1 = homme, 0 = femme)
counts <- table(data_heart_d$Sex)

# Renommer les étiquettes pour plus de clarté
names(counts) <- c("Femme", "Homme")

# Créer l'histogramme
barplot(counts, 
        main = "Répartition Homme/Femme des décès par crise cardiaque",
        xlab = "Sexe",
        ylab = "Nombre d'individus",
        col = c("pink", "lightblue"))
legend("topright", legend = counts, fill = c("pink", "lightblue"))

#####################################################################################

modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))




predictions <- predict(modele_logistique, type = "response")
class_pred <- ifelse(predictions > 0.5, 1, 0)
#print(class_pred)


res <- table(Predicted = class_pred, Actual = data$HeartDisease)
#print(res)
erreur_classification <- sum(res[1,2], res[2,1]) / sum(res)
print(erreur_classification)



######################################################################################

# Création de tranches d'âge
data$AgeGroup <- cut(data$Age, breaks = seq(from = min(data$Age), to = max(data$Age), by = 10))

# Calculer le nombre total de personnes par tranche d'âge
total_par_age <- table(data$AgeGroup)

# Filtrer pour ne garder que les personnes malades
data_malade <- subset(data, HeartDisease == 1)

# Calculer le nombre de personnes malades par tranche d'âge
malades_par_age <- table(data_malade$AgeGroup)

# Calculer le ratio de personnes malades par tranche d'âge pour 100 personnes
ratio_malades_par_100 <- (malades_par_age / total_par_age) * 100

# Créer un diagramme à barres pour ces ratios
barplot(ratio_malades_par_100, 
        main = "Pourcentage de personnes malades par tranche d'âge pour 100 personnes", 
        xlab = "Tranche d'âge", 
        ylab = "Pourcentage de malades", 
        col = "lightblue",
        las = 2) # Rotation des étiquettes de l'axe des x

# Ajuster un modèle de régression logistique avec la maladie cardiaque en fonction de l'âge
modele_logistique <- glm(HeartDisease ~ Age, data=data, family=binomial)

# Tracer les données originales (optionnel, pour visualisation)
plot(data$Age, data$HeartDisease, xlab="Age", ylab="Probabilité de maladie cardiaque", main="Courbe de tendance de maladie cardiaque par âge")

# Tracer la courbe de tendance basée sur le modèle de régression logistique
curve(predict(modele_logistique, data.frame(Age=x), type="response"), add=TRUE, col="red")

