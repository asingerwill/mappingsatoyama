# Mapping Japanese *satoyama* with socio-ecological data

## Abstract
Japanese farmers traditionally practice mosaic land use patterns, known as *satoyama* (里山), that may hold global lessons for sustainable living. Where are *satoyama* in Japan today? Identifying *satoyama* systematically across space and time poses difficulties, in part because the scholarly work lacks a transdisciplinary perspective. We compile a novel spatially-linked dataset of 200 variables across 150,000 administrative units and analyze nine case studies. Exploring various model-building techniques allows us to (1) connect the social and ecological dimensions of *satoyama* and (2) define it in contrast to urban areas and deep mountain forests. We train a final multinomial logistic regression model on our case studies, then extend it to classify all of Japan. Incomplete datasets and case studies grounded upon incommensurate interpretations of complex social phenomena limit our model. Additional qualitative research will further our understanding of *satoyama*’s relationship to global sustainability. 

## What's in this repo?
The file shuraku_ag_dataset.Rmd is an R markdown file explaining the tidying process for our dataset. The file shuraku_modeling.Rmd details the model-building process. An additional R file, shiny.R, holds our shiny dashboard.
