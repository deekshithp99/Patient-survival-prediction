Patient survival prediction model- randomForest
---
title: "Patient Survival Prediction"
author: "Deekshith Pothedar"
output:
  word_document: default
  html_document: default
---
```{r setup, include=FALSE}
library(caTools)
library(randomForest)
library(magrittr)
library(formattable)
library(knitr)
```
## Abstract

For patients who enter at the hospital as an emergency, Intensive Care Units (ICUs) usually lack solid medical histories. A troubled patient or one who is brought to emergency treatment in a disoriented or comatose state may be unable to provide any medical history information to the clinician.Due to the lack of medical records, the results of rapid laboratory tests performed on the patient upon arrival are the only means to determine the patient's health and, more significantly, survival. This research project uncovers the characteristics that are important in predicting an ICU patient's survival. These results may be useful for the medical practitioners to prioritize in obtaining the high-importance values first to improve the patient's chances of survival.

The dataset contains clinical outcome information as well as patient survival rates. Predictive analysis was performed on the dataset with 'hospital death' (Whether the patient died during this hospitalization) as the variable of interest, and a prediction model with 91.5% efficiency(approximately) has been created using random forest algorithm. This study also identified key elements that aid in predicting the patient's survival.

## Introduction

Intensive Care Units (ICUs) frequently lack reliable medical histories for patients who arrive at the hospital as an emergency. A distressed patient or one who is brought in a disoriented or unconscious state may be unable to communicate any medical history information to the doctor at emergency care. Transferring medical records may take many days, especially if the patient is coming from another medical provider or system. However, knowing the patient's medical history can help doctors make better clinical decisions about the patient's therapy. Because medical records are unavailable, the findings of fast laboratory tests done on the patient upon arrival are the only way to identify the patient's health and, more importantly, survival.This research project uncovers the characteristics that are important in predicting an ICU patient's survival. As a result, medical practitioners might try to prioritize in obtaining the high-importance values first to improve the patient's chances of survival.

The dataset contains physical characteristics of the patient, laboratory test results, APACHE ('Acute physiology and chronic health evaluation') values as well as information if the patient passed away during the hospitalization (0-No,1-Yes). The dataset contains information of 91,713 patients.

## Literature Review

Traces of 2 similar research works (Raffa, et al., 2019), (Chi, et al., 2019) of ICU patients are currently available on the internet. Although the dataset used and the complete research work is not available for free, a summary of the two studies is available on open source. Both the research papers mainly focused on the survival rate and disease severity and have focused on deducing conclusions for ICU patients battling with terminal diseases. Unlike the study that cited papers have put forward, the main objective of this research project is focused on emergency cases battling in the ICU. The dataset contains a column ‘ICU type’. The number of cases for each of the ICU type is depicted below as a pie chart.
One of the studies (Raffa, et al., 2019) focused on calculating the severity of illness score at country and region level. For this study data was taken from the Australia New Zealand Intensive Care Society (ANZIS). During this study the missing data was filled using prediction models and logistic regression was used for analysis.
The second study (Chi, et al., 2019) focused on palliative care (PC) for ICU patients. Palliative care is specialist medical care for those who are suffering from a terminal disease. A tool was created using predictive analysis to decide the unit where the patient needs to be admitted based on the survival value predicted by the model. The impact of PC consultation on outcomes was estimated using multivariate logistic regression analysis.

## Research Questions

The following points will be addressed in this research project:
1. Determine an efficient algorithm to predict the survival of the patient.
2. What are the most important elements in predicting the survival of a patient admitted to ICU?

## Theory

This paper is exploring best prediction algorithm, i.e., a high performing model to predict the variable of interest 'hospital_death'. The exploration includes finding variables that play key role in determeing the hospital_death. This study also involves predetermined APACHE ("Acute physiology and chronic health evaluation") values calculated by medical apparatus. No medical/clinical calculations are involved in this paper.

## Data

The data for this study is derived from open source:
https://www.kaggle.com/datasets/mitishaagarwal/patient/download
Variables available in the data set are mentioned below:

```{r}
#loading the .csv file and creating a data frame.
dataset <- read.csv("/Users/akshaymusuku/Downloads/R project_JP/Dataset.csv")
dictionary <- read.csv("/Users/akshaymusuku/Downloads/R project_JP/Data Dictionary.csv")
```

```{r}
str(dataset)
```

Data dictionary of the dataset is as below:
```{r}
#displaying few columns of the data dictioanry to refer the description of variable names
print(dictionary[, c(2, 5)])
```

```{r echo=FALSE, results='hide'}
#Finding sum of null values in each column
colSums(is.na(dataset))
```

Below is the indication of total number of hospital deaths (whether the patient died during hospitalization or not)- 1 did not survive and 0 as survived.

```{r echo=TRUE}
#using table function to create a categorical representation of the data
table(dataset['hospital_death'])
```

Below pie chart depicts the types of ICUs that the study includes.
```{r}
#counting labels in icu_type column
maj_icu = table(dataset['icu_type'])
#pie chart for icu_type column
labels <- c('Med-Surg ICU','MICU','Neuro ICU','CCU-CTICU','SICU','Cardiac ICU','CSICU','CTICU')
pie(maj_icu,labels)
```

As part of the data cleaning process following steps have been done. Considering the fact that undersampling of non-events as one of the most important steps in data cleaning unnecessary variables i.e., non-medical variables like patient ID, encounter ID have been eliminated and required variables have been captured.

```{r echo=TRUE}
#Input required value adding columns for analysis. Ignored columns that contained non-medical data.
input_cols <- c('hospital_id', 'age', 'bmi', 'elective_surgery', 'ethnicity', 'gender',
       'height', 'icu_admit_source', 'icu_id', 'icu_stay_type', 'icu_type',
       'pre_icu_los_days', 'weight', 'apache_2_diagnosis',
       'apache_3j_diagnosis', 'apache_post_operative', 'arf_apache',
       'gcs_eyes_apache', 'gcs_motor_apache', 'gcs_unable_apache',
       'gcs_verbal_apache', 'heart_rate_apache', 'intubated_apache',
       'map_apache', 'resprate_apache', 'temp_apache', 'ventilated_apache',
       'd1_diasbp_max', 'd1_diasbp_min', 'd1_diasbp_noninvasive_max',
       'd1_diasbp_noninvasive_min', 'd1_heartrate_max', 'd1_heartrate_min',
       'd1_mbp_max', 'd1_mbp_min', 'd1_mbp_noninvasive_max',
       'd1_mbp_noninvasive_min', 'd1_resprate_max', 'd1_resprate_min',
       'd1_spo2_max', 'd1_spo2_min', 'd1_sysbp_max', 'd1_sysbp_min',
       'd1_sysbp_noninvasive_max', 'd1_sysbp_noninvasive_min', 'd1_temp_max',
       'd1_temp_min', 'h1_diasbp_max', 'h1_diasbp_min',
       'h1_diasbp_noninvasive_max', 'h1_diasbp_noninvasive_min',
       'h1_heartrate_max', 'h1_heartrate_min', 'h1_mbp_max', 'h1_mbp_min',
       'h1_mbp_noninvasive_max', 'h1_mbp_noninvasive_min', 'h1_resprate_max',
       'h1_resprate_min', 'h1_spo2_max', 'h1_spo2_min', 'h1_sysbp_max',
       'h1_sysbp_min', 'h1_sysbp_noninvasive_max', 'h1_sysbp_noninvasive_min',
       'd1_glucose_max', 'd1_glucose_min', 'd1_potassium_max',
       'd1_potassium_min', 'apache_4a_hospital_death_prob',
       'apache_4a_icu_death_prob', 'aids', 'cirrhosis', 'diabetes_mellitus',
       'hepatic_failure', 'immunosuppression', 'leukemia', 'lymphoma',
       'solid_tumor_with_metastasis', 'apache_3j_bodysystem',
       'apache_2_bodysystem')
```

```{r echo=FALSE}
dataset <- subset(dataset, select = c(input_cols, 'hospital_death'))
```
All the missing values have been replaced with median values. In one of the studies put forward by (Raffa, et al., 2019), missing values were filled with predicted values. Median imputation has been chosen in this study as the data is skewed.

```{r echo=TRUE}
#Replacing missing data with median values
dataset <- dataset %>% 
  dplyr::mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))
```
The dataset is then split into 2 parts with split ratio 0.8 for traning and testing.
```{r echo=TRUE}
#splitting the dataset into 2 sets (80% and 20%) for training and testing
split <- sample.split(dataset, SplitRatio = 0.8)
train <- subset(dataset, split == "TRUE")
test <- subset(dataset, split == "FALSE")
```
## Methodology

Once the data is cleaned and prepared for analysis, the randomForest function is used to train the model.To improve the quality of the model, random 'ntree' values (like 3, 4, 5, 20, 40, 60, 80,..) have been picked up, the model has been tested and the OOB error (out-of-box error) has been recorded for each case. The number of trees=5 has been chosen as it has the least error.

```{r , warning=FALSE}
#Using set.seed to ensure the results are same for randomization
set.seed(120)
#Training the random Forest model.
classifier_RF = randomForest(x = subset(train, select = c(input_cols)),
                             y = train$hospital_death,
                             ntree = 5, importance=TRUE)
```

Then the prediction of test set results has been carried out in the below chunk.

```{r echo=TRUE}
#Predicting the Test set results
y_pred = predict(classifier_RF, newdata = subset(test, select = c(input_cols)))
```

Confusion matrix has been generated for each of the nodes of 5 trees. 

```{r}
#creating confusion matrix for each node of random forest
confusion_mtx = table(test$hospital_death, y_pred)
#confusion_mtx
```

The plot of the model looks as depicted below. The error of each of the trees is ranging between 9.5% to 11.5%.

```{r echo=TRUE}
#Plotting model
plot(classifier_RF)
```

Below is the importance plot of the variables. The Mean Decrease Accuracy (%IncMSE) shows the value of how much the model accuracy reduces if we leave out the respective variable and Mean Decrease Gini (IncNodePurity) is a measure of the importance of the variable based on the Gini Impurity Index that is used for calculating the randomness in the trees. Higher the value of %IncMSE or IncNotePurity , higher is the importance of the variable to the model that has been built. 

Hence, the following variables can be considered as the most important ones in predicting the hospital_dealth with the highest accuracy possible.

```{r echo=TRUE}
#Importance plot to show "importance" of variables: higher value indicates higher importance
importance(classifier_RF)
```

## Results

The importance plot below clearly depicts variables that play a key role in achieving an efficiency of ~91.5% in predicting whether a patient survives the hospitalisation or not. Important variables:  

On the basis of %IncMSE: d1_heartrate_min (9.675), temp_apache, bmi, d1_glucose_max, h1_resprate_min  apache_2_diagnosis, d1_temp_min, d1_sysbp_noninvasive_min, d1_diasbp_max, d1_spo2_max (5.98)
On the basis of IncNodePurity: apache_4a_hospital_death_prob (716.2), apache_4a_icu_death_prob, d1_heartrate_min, d1_sysbp_noninvasive_min, d1_spo2_min,  pre_icu_los_days, d1_sysbp_min, d1_glucose_min, d1_temp_min (108.22).

The factors that are of high importance in both scenarios may be considered as the top priority parameters that are essential in predicting the survival of a patient efficiently. This study may be taken as reference by medical professionals to create a checklist of the laboratory tests with importance/priority on each of the tests.

Below is the importance plot for each of the values (mean decrease accuracy & mean decrease gini).
```{r echo=TRUE}
#Variable importance plot
varImpPlot(classifier_RF)
```


## Conclusion

From this research project, random forest model with t=number of trees=5 shows high perfromnce on the dataset. It can also be deduced that variables at the top of the importance plot chart can be considered by the medical practitioner at the ICU as key parameters in deciding the distressed pateint's condition. Laboratory tests related to these values may be listed as basic and high priority tests and may be ensured that they are performed as soon as the patient arrives at the facility. This would increase the survival chances of the patient.
Extra care may be extended to patients who have already been to the ICU before as it is one of the most important variables with IncNodepurity value greater than 100, also placed among the top 10 positions of importance plot.

## Implications

This study may be used as a reference for anybody conducting medical-related analysis on predicting patient survival rates in an emergency. Medical expertise may be applied to improve the outcomes and the model's performance. As an IT student with no medical knowledge, numeric variables were prioritized during data cleaning and preparation. As a medical practitioner, analyzing this dataset or a comparable one will undoubtedly provide higher efficiency and can be approved for real-time usage.

## References

Dataset: https://www.kaggle.com/datasets/sadiaanzum/patient-survival-prediction-dataset/download (Links to an external site.)

https://journals.lww.com/ccmjournal/Citation/2019/01001/33 (Links to an external site.)

http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/123-required-r-packages-for-principal-component-methods/

Citation1: Raffa, Jesse1; Johnson, Alistair1; Celi, Leo Anthony2,,1; Pollard, Tom1; Pilcher, David3; Badawi, Omar4 33: THE GLOBAL OPEN SOURCE SEVERITY OF ILLNESS SCORE (GOSSIS), Critical Care Medicine: January 2019 - Volume 47 - Issue 1 - p 17 doi: 10.1097/01.ccm.0000550825.30295.dd

Citation2: Chi, Stephen; Buettner, Benjamin; Ma, Jessica; Pollard, Katherine; Muir, Monica; Kolekar, Charu; Al-Hammadi, Noor; Kollef, Marin; Dans, Maria 34: EARLY PALLIATIVE CARE CONSULTATION IN THE MEDICAL ICU: A CLUSTER RANDOMIZED CROSSOVER TRIAL Critical Care Medicine:January, 2019

https://journals.lww.com/ccmjournal/toc/2019/01001

Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32.

Breiman, L (2002), ``Manual On Setting Up, Using, And Understanding Random Forests V3.1'',
https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1/topics/randomForest
https://www.listendata.com/2014/11/random-forest-with-r.html
