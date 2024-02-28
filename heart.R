setwd("C:/Users/marco misseri/OneDrive/Documents/1mam4/R/KKK/Kaggle-chalenge-heart-disease")
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


modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))




predictions <- predict(modele_logistique, type = "response")
class_pred <- ifelse(predictions > 0.5, 1, 0)
#print(class_pred)


res <- table(Predicted = class_pred, Actual = data$HeartDisease)
#print(res)
erreur_classification <- sum(res[1,2], res[2,1]) / sum(res)
print(erreur_classification)