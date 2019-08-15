# Read data
animalData <- read.csv("aac_shelter_outcomes.csv", header=TRUE, sep=",")
head(animalData)

#Extract categorical variables
catdat <-animalData[c(3,10,11,12)]
head(catdat)

###trasform categorical variables to dummy variables
# Type of animal
animalTypeDumm <-model.matrix(~animal_type -1, data=catdat)
head(animalTypeDumm)

# outcome type
outcomeTypeDumm <-model.matrix(~outcome_type -1, data=catdat)
head(outcomeTypeDumm)

# outcome subtype
outcomeSubtypeDumm <-model.matrix(~outcome_subtype -1, data=catdat)
head(outcomeSubtypeDumm)

# sex upon outcome
sexUponOutcomeDumm <-model.matrix(~sex_upon_outcome -1, data=catdat)
head(sexUponOutcomeDumm)

###drop reference category and append the remaining to dataset
animalData1 <-animalData[-c(3,10,11,12)]

# Animal type
animalData1$CatYes <- animalTypeDumm[,2]
animalData1$DogYes <- animalTypeDumm[,3]

# Outcome type
animalData1$AdoptionYes <- outcomeTypeDumm[,1]
animalData1$EuthanasiaYes <- outcomeTypeDumm[,4]
animalData1$RelocateYes <- outcomeTypeDumm[,7]
animalData1$TransferYes <- outcomeTypeDumm[,10]

# Sex upon outcome
animalData1$IntactFemaleYes <- sexUponOutcomeDumm[,1]
animalData1$IntactMaleYes <- sexUponOutcomeDumm[,2]
animalData1$NeuteredMaleYes <- sexUponOutcomeDumm[,3]
animalData1$SpayedFemaleYes <- sexUponOutcomeDumm[,5]

#Train set
set.seed(5)
n <- nrow(animalData1)
shuffled_df <- animalData1[sample(n), ]

# Data set is to big, downsize the dataset
newSize <- 1:round(0.5 * n)
shuffled_df <- shuffled_df[newSize, ]

n <- nrow(shuffled_df)

train_indices <- 1:round(0.6 * n)
train.df <- shuffled_df[train_indices, ]

#Test set
test_indices <- (round(0.6 * n) + 1):n
test.df <- shuffled_df[test_indices, ]

#run logistic regression
logistic.results.train <-glm(AdoptionYes ~., data=train.df, family=binomial(link="logit"))
