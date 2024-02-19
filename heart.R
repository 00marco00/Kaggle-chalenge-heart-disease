setwd("C:/Users/marco misseri/OneDrive/Documents/1mam4/R/KKK")
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


data_mat <- cor(data)
#print(data_mat)


modele_logistique <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = data, family = binomial(link = "logit"))
#print(summary(modele_logistique))


predictions <- predict(modele_logistique, type = "response")
class_pred <- ifelse(predictions > 0.5, 1, 0)
#print(class_pred)


res <- table(Predicted = class_pred, Actual = data$HeartDisease)
#print(res)
erreur_classification <- sum(res[1,2], res[2,1]) / sum(res)
print(erreur_classification)