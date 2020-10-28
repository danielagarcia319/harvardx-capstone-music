# Install and Load libraries
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if (!require(caret)) install.packages('caret'); library(caret)


###### Download and Clean Data ######

# Data downloaded from http://www2.projects.science.uu.nl/memotion/emotifydata/
dl <- tempfile()
download.file("http://www2.projects.science.uu.nl/memotion/emotifydata/data.csv", dl)

# Read data from csv file into a data frame
music_data <- read.csv(dl)

# Make edits to data frame (generate "opinion" variable from "liked" and "disliked" variables)
music_opinion <- music_data %>%
  mutate(genre = as.factor(genre),
         mother.tongue = as.factor(mother.tongue),
         opinion = ifelse(liked == 0 & disliked == 0, 2, liked),
         opinion = factor(opinion, levels = c(0, 1, 2), labels = c("disliked", "liked", "no opinion"))) %>%
  select(-liked, - disliked)

# Remove unnecessary variables from global environment
rm(dl, music_data)



###### Data Exploration ######

# Determine average age and mood of listeners for each opinion
avg_age_mood <- music_opinion %>% group_by(opinion) %>%
  summarize(avg_age = mean(age), avg_mood = mean(mood))

# Plot opinion distribution per emotion
music_emotion_gather <- gather(music_opinion, key="emotion", value="value", 
                               c("amazement", "calmness", "joyful_activation", 
                                 "nostalgia", "power", "solemnity", "tenderness", "tension"))

music_emotion_plots <- music_emotion_gather %>% group_by(opinion, emotion) %>% 
  mutate(average = mean(value)) %>% ggplot(aes(opinion, average, color = opinion)) +
  geom_point() + facet_wrap(~emotion)

# Plot opinion distribution per genre
music_genre_plots <- music_opinion %>% group_by(opinion, genre) %>% 
  ggplot(aes(opinion, fill = opinion)) +
  geom_bar() + facet_wrap(~genre)

# Plot opinion distribution per gender
music_gender_plots <- music_opinion %>% group_by(opinion, gender) %>% 
  mutate(gender = ifelse(gender == 0, "female", "male")) %>%
  ggplot(aes(opinion, fill = opinion)) +
  geom_bar() + facet_wrap(~gender)

# Plot opinion distribution per mother tongue
music_language_plots <- music_opinion %>% group_by(opinion, mother.tongue) %>% 
  ggplot(aes(opinion, fill = opinion)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar() + facet_wrap(~mother.tongue) + 
  scale_y_continuous(trans = "log10") 



###### Split Data into Training and Test Sets ######

# Test set will be 10% of "music_opinion" data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = music_opinion$opinion, times = 1, p = 0.1, list = FALSE)
music_train <- music_opinion[-test_index,]
music_temp <- music_opinion[test_index,]

# Make sure track IDs, genres, ages, moods, genders 
# and mother tongues that are in testing set are also in training set
music_test <- music_temp %>% 
  semi_join(music_train, by = "track.id") %>%
  semi_join(music_train, by = "genre") %>%
  semi_join(music_train, by = "age") %>%
  semi_join(music_train, by = "mood") %>%
  semi_join(music_train, by = "gender") %>%
  semi_join(music_train, by = "mother.tongue")

# Add rows removed from testing set back into training set
removed <- anti_join(music_temp, music_test)
music_train <- rbind(music_train, removed)

# Remove unnecessary variables from global environment
rm(music_temp, test_index, removed)



###### Fit Machine Learning Models ######

# Generate a guessing model to compare other models to
set.seed(1, sample.kind = "Rounding")
y_hat_guess <- sample(c("liked", "disliked", "no opinion"), nrow(music_test), replace = T)

# Determine the accuracy of the guessing model
accuracy_guess <- mean(y_hat_guess == music_test$opinion)

# Generate 8 Classification Models (using common algorithms in caret package)
models <- c("rpart", "lda", "naive_bayes", "knn", "multinom", "Rborist", "rf", "svmLinear")

set.seed(1, sample.kind = "Rounding") 
fits <- lapply(models, function(model){ 
  print(model)
  train(opinion ~ age + mood + calmness + joyful_activation +
          tenderness + tension + genre, method = model, data = music_train)
}) 
names(fits) <- models

# Make predictions for each model
y_hat <- function(x) {
  predict(x, music_test)
}
predictions <- sapply(fits, y_hat)

# Determine accuracy of each model
accuracies <- vector(length = length(models))
for (i in 1:length(models)) {
  accuracies[i] <- mean(predictions[,i] == music_test$opinion)
}
avg_accuracy <- mean(accuracies)

# Create ensemble predictions and determine the ensemble accuracy
# Ensemble prediction is set equal to the most common prediction among the 8 models
ensemble <- vector(length = nrow(music_test))
for (i in 1:nrow(music_test)) {
  ensemble[i] <- names(which.max(table(predictions[i,])))
}
ensemble_accuracy <- mean(ensemble == music_test$opinion)

# Determine which models are more accurate than the ensemble
sum(accuracies > ensemble_accuracy)
models[which(accuracies > ensemble_accuracy)]

# Determine estimated accuracies from models (before making predictions)
accuracy_estimates <- vector(length = length(models))
for (i in 1:length(models)) {
  accuracy_estimates[i] = fits[[i]]$results$Accuracy
}
mean(accuracy_estimates)

# Generate second ensemble model 
# Only include models with an estimated accuracy greater than the mean estimated accuracy
fits_2 <- fits[which(accuracy_estimates >= mean(accuracy_estimates))]
predictions_2 <- sapply(fits_2, y_hat)
ensemble_2 <- vector(length = nrow(music_test))
for (i in 1:nrow(music_test)) {
  ensemble_2[i] <- names(which.max(table(predictions_2[i,])))
}
ensemble_accuracy_2 <- mean(ensemble_2 == music_test$opinion)

# Determine which models (if any) are more accurate than the second ensemble 
sum(accuracies > ensemble_accuracy_2)
models[which(accuracies > ensemble_accuracy_2)]



###### Save data as .Rdata for Markdown Report ######

save(avg_age_mood, file = "avg_age_mood.Rdata")
save(music_emotion_plots, file = "music_emotion_plots.Rdata")
save(music_gender_plots, file = "music_gender_plots.Rdata")
save(music_genre_plots, file = "music_genre_plots.Rdata")
save(music_language_plots, file = "music_language_plots.Rdata")
save(accuracies, file = "accuracies.Rdata")
save(models, file = "models.Rdata")

