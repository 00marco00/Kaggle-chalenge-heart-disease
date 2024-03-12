#chargement et traitement des données

setwd("C:/Users/marco/OneDrive/Documents/GitHub/Kaggle-chalenge-heart-disease")
data <- read.csv("heart.csv", sep = ",")
#print( head(data))


data$Sex <- ifelse(data$Sex == "M", 1, -1)
data$RestingECG <- ifelse(data$RestingECG == "Normal",1,-1)
data$ChestPainType <- ifelse(data$ChestPainType == "ATA", -1,
                                    ifelse(data$ChestPainType == "NAP", 0, 1))
data$ExerciseAngina <- ifelse(data$ExerciseAngina == "Y",1,-1)
data$ST_Slope <- ifelse(data$ST_Slope == "Up", -1,
                        ifelse(data$ST_Slope == "Flat", 0, 1))
#print( head(data))
pairs(data, main = "Matrice de Scatter Plots")

data_mat <- cor(data)
#print(data_mat)


###################################################################################


# Nombre total d'individus par sexe
counts_total <- table(data$Sex)

# Renommer les étiquettes pour plus de clarté
names(counts_total) <- c("Femme", "Homme")

# Nombre d'individus malades par sexe
counts_malades <- table(data_heart_d$Sex)

# Renommer les étiquettes pour plus de clarté
names(counts_malades) <- c("Femme", "Homme")

# Calculer le ratio pour 100 personnes par sexe
ratio_femmes_malades_pour_100 <- (counts_malades["Femme"] / counts_total["Femme"]) * 100
ratio_hommes_malades_pour_100 <- (counts_malades["Homme"] / counts_total["Homme"]) * 100

# Afficher le ratio sous forme d'histogramme
ratios <- c(Femme = ratio_femmes_malades_pour_100, Homme = ratio_hommes_malades_pour_100)
barplot(ratios, 
        main = "Ratio de personnes malades pour 100 individus par sexe",
        xlab = "Sexe",
        ylab = "Ratio de malades pour 100 individus",
        col = c("pink", "lightblue"))


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


#####################################################################################

#modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))

modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType  + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))




predictions <- predict(modele_logistique, type = "response")
class_pred <- ifelse(predictions > 0.5, 1, 0)
#print(class_pred)


res <- table(Predicted = class_pred, Actual = data$HeartDisease)
#print(res)
erreur_classification <- sum(res[1,2], res[2,1]) / sum(res)
print(erreur_classification)


#######################################################################################
#réduction de variable
set.seed(123) # Pour la reproductibilité
indices <- sample(1:nrow(data), size = floor(0.75 * nrow(data)))
trainData <- data[indices, ]
testData <- data[-indices, ]
modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol * Age + FastingBS + MaxHR * ExerciseAngina + Oldpeak + ST_Slope,
                         family = binomial(link = 'logit'), data = trainData)
predictions <- predict(modele_logistique, newdata = testData, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
actual_classes <- testData$HeartDisease

# Calcul de l'exactitude, de la sensibilité et de la spécificité
table_mat <- table(Predicted = predicted_classes, Actual = actual_classes)
accuracy <- sum(diag(table_mat)) / sum(table_mat)
sensitivity <- table_mat[2,2] / sum(table_mat[,2])
specificity <- table_mat[1,1] / sum(table_mat[,1])

print(paste("Exactitude:", accuracy))
print(paste("Sensibilité:", sensitivity))
print(paste("Spécificité:", specificity))
roc_curve <- roc(response = actual_classes, predictor = as.numeric(predictions))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

