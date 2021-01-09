## HarvardX Data Science Capstone 2: Predicting Music Preferences

This repository contains the R code and final outputs of the Music Preferences model generated for my second HarvardX Data Science Capstone.

## Installation

The final PDF report is stored as `danielagarcia-music-final-report.pdf`. 

To run the code yourself, clone the repository and open the `danielagarcia-capstone-music.Rproj` file. The `danielagarcia-music-script.R` file will re-generate the necessary `.Rdata` files to create the final Rmd report.

## About the Project

This project takes data on songs and reviews that listeners have given them and produces a model that predicts listeners' opinions regarding the songs. Listeners can either like, dislike, or have no opinion about a song. 

The predictive models I generated take into account the effects that the listener's age, gender, mother tongue, mood before listening, emotions felt whilst listening, and the song's ID and genre have on the opinion the listener has of the song in order to generate predictions. 

The models were trained on training data that provides opinions given different songs and listeners. After training on this data, I generated predictions on the testing data using each of the models. I determined the accuracy of each model and selected my final model based on the model that most accurately predicted the opinions of the testing data.
