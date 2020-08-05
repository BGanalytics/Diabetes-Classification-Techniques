# Diabetes-Classification-Techniques

Work IN PROGRESS

This dataset is a medical record from th following link: https://www.kaggle.com/vincentlugat/pima-indians-diabetes-eda-prediction-0-906/notebook

Fields description follow:
preg = Number of times pregnant
plas = Plasma glucose concentration a 2 hours in an oral glucose tolerance test
pres = Diastolic blood pressure (mm Hg)
skin = Triceps skin fold thickness (mm)
test = 2-Hour serum insulin (mu U/ml)
mass = Body mass index (weight in kg/(height in m)^2)
pedi = Diabetes pedigree function
age = Age (years)
class = Class variable (1:tested positive for diabetes, 0: tested negative for diabetes)

I chose this dataset to showcase methods for classifying patients based on some characteristics which consequently allows us to predict the risk 
of a new patient based on their charateristics.
I have done the following steps:
1. Data preprocessing
2. Creating density plots, correlation matrix and heatmap for selected variables
3. trying out clasification methods Kernel SVM and RandomForest

Results: 
Both methods were OK in classifying the patients based on body mass index and number of pregnancies. However, I believe I can find more suitable methods. 