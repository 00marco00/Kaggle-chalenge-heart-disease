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

dt <- data

###################################################################################


# Nombre total d'individus par sexe
counts_total <- table(data$Sex)

# Renommer les étiquettes pour plus de clarté
names(counts_total) <- c("Femme", "Homme")

data_heart_d = data[data$HeartDisease == 1, ]

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
print("classification")

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

#boxplot()

#######################################################################################
#réduction de variable
print("reduction")
set.seed(123) # Pour la reproductibilité
indices <- sample(1:nrow(data), size = floor(0.75 * nrow(data)))
trainData <- data[indices, ]
testData <- data[-indices, ]
modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope,
                         family = binomial(link = 'logit'), data = trainData)
predictions <- predict(modele_logistique, newdata = testData, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
actual_classes <- testData$HeartDisease

# Calcul de la matrice de confusion
confusionMatrix <- table(Predicted = predicted_classes, True = testData$HeartDisease)

# Affichage de la matrice de confusion
print(confusionMatrix)

# Calcul de l'exactitude (Accuracy)
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)

# Calcul de l'erreur de classification
classification_error <- 1 - accuracy

# Affichage de l'erreur de classification
print(paste("Erreur de classification :", classification_error))



######################################################################################"
print("random forest")

#install.packages("randomForest")

# Chargement du package randomForest
library(randomForest)


data$HeartDisease <- as.factor(data$HeartDisease)

# Ajustement du modèle Random Forest
modele_rf <- randomForest(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, 
                          data = data, 
                          ntree = 500,  # Nombre d'arbres à générer, peut être ajusté selon les besoins
                          mtry = 3,     # Nombre de variables testées à chaque division, peut être ajusté
                          importance = TRUE)  # Pour calculer l'importance des variables

# Affichage du résumé du modèle
print(modele_rf)

# Pour voir l'importance des variables
importance(modele_rf)

############################################################################################
print("gradiant boosting")
#gradiant bosting entrainé sur toute les données

# Chargement du package xgboost
library(xgboost)

# Préparation des données pour xgboost
data_matrix <- model.matrix(HeartDisease ~ Age + Sex + ChestPainType + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope - 1, data = data)
label_vector <- as.numeric(data$HeartDisease) - 1  # Assurez-vous que les étiquettes sont 0 et 1

dtrain <- xgb.DMatrix(data = data_matrix, label = label_vector)

# Paramètres pour xgboost
params <- list(
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  nthread = 2
)

# Nombre d'itérations de boosting
nrounds <- 100

# Ajustement du modèle
modele_gb <- xgb.train(params = params, data = dtrain, nrounds = nrounds)

test_matrix <- model.matrix(~ Age + Sex + ChestPainType + Cholesterol + FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope - 1, data = testData)
dtest <- xgb.DMatrix(data = test_matrix)


# Prédictions des probabilités
predictions <- predict(modele_gb, dtest)

# Conversion des probabilités en classes prédites
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Conversion des vraies valeurs de la variable de réponse en format numérique si nécessaire
true_values <- as.numeric(testData$HeartDisease) - 1  # Assurez-vous que c'est 0 et 1

# Calcul de la matrice de confusion
confusionMatrix <- table(Predicted = predicted_classes, True = true_values)

# Affichage de la matrice de confusion
print(confusionMatrix)

# Calcul de l'exactitude (Accuracy)
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)

# Calcul de l'erreur de classification
classification_error <- 1 - accuracy

# Affichage de l'erreur de classification
print(paste("Erreur de classification :", classification_error))


############################################################################################
print("Gradient Boosting 2")
# Gradient Boosting entraîné sur 80% des données testé sur 20%

set.seed(123)  # Pour la reproductibilité
index <- sample(1:nrow(dt), size = 0.8 * nrow(dt), replace = FALSE)  # 80% pour l'entraînement
trainData <- dt[index, ]
testData <- dt[-index, ]

# Préparation des données pour xgboost
# Ensemble d'entraînement
train_matrix <- model.matrix(~ . - 1, data = trainData)  # Supprime l'intercept
train_labels <- as.numeric(trainData$HeartDisease)  # Conserve les étiquettes comme 0 et 1
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)

# Paramètres pour xgboost
params <- list(
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  nthread = 2
)

# Nombre d'itérations de boosting
nrounds <- 100

# Ajustement du modèle sur l'ensemble d'entraînement
modele_gb2 <- xgb.train(params = params, data = dtrain, nrounds = nrounds)

# Préparation de l'ensemble de test
test_matrix <- model.matrix(~ . - 1, data = testData)  # Supprime l'intercept
test_labels <- as.numeric(testData$HeartDisease)  # Conserve les étiquettes comme 0 et 1
dtest <- xgb.DMatrix(data = test_matrix)

# Prédictions sur l'ensemble de test
predictions <- predict(modele_gb2, dtest)

# Conversion des probabilités en classes prédites
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Calcul de la matrice de confusion et de l'erreur de classification
confusionMatrix <- table(Predicted = predicted_classes, True = test_labels)
print(confusionMatrix)

accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
classification_error <- 1 - accuracy
print(paste("Erreur de classification :", classification_error))


##################################################################################

#test de plot





