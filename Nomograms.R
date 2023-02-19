library(rms)
library(DynNom)
library(glmnet)
library(ggplot2)

set.seed(123)

setwd("~/Desktop/AMI/")

# For MI prediction

data = read.csv("df_MI.csv")

train = read.csv("df_MI_Train.csv")

names(data)[names(data) == 'second.MI'] <- 'MI'
names(train)[names(train) == 'second.MI'] <- 'MI'

model <- xgboost::xgb.load('XGBoost.MI')

X <- train[, 0:192]
y <- train[, ncol(train)]

lr <- glmnet(as.matrix(X), y, family = binomial)
lr <- glm(MI ~ APACHE.Score + Sex + Age + Height + Weight + Hour.of.Admission + Teaching.Status + Ethnicity..African.American + Ethnicity..Asian + Ethnicity..Caucasian + Ethnicity..Hispanic + Ethnicity..Native.American + Ethnicity..Other.Unknown + CCU.CTICU + CSICU + CTICU + Cardiac.ICU + MICU + Med.Surg.ICU + Neuro.ICU + SICU + Admission.Source..Acute.Care.Floor + Admission.Source..Direct.Admit + Admission.Source..Emergency.Department + Admission.Source..Floor + Admission.Source..Operating.Room + Admission.Source..Other.Hospital + Admission.Source..PACU + Admission.Source..Recovery.Room + Admission.Source..Step.Down.Unit..SDU. + Admission.Source..misc + unitvisitnumber_1 + unitvisitnumber_2 + unitvisitnumber_3 + unitvisitnumber_misc + Admission + Readmission + Transfer + Physician.Speciality..Not.Specified + Physician.Speciality..Cardiology + Physician.Speciality..Critical.care.medicine..CCM. + Physician.Speciality..Family.practice + Physician.Speciality..Hospitalist + Physician.Speciality..Internal.medicine + Physician.Speciality..Misc + Physician.Speciality..Neurology + Physician.Speciality..Other + Physician.Speciality..Pulmonary + Physician.Speciality..Pulmonary.CCM + Physician.Speciality..Surgery.cardiac + Physician.Speciality..Surgery.general + Physician.Speciality..Surgery.neuro + Physician.Speciality..Surgery.trauma + Physician.Speciality..Surgery.vascular + Physician.Speciality..Unknown + Number.of.Beds..100...249 + Number.of.Beds..250...499 + Number.of.Beds...100 + Number.of.Beds.....500 + Region..Midwest + Region..Northeast + Region..South + Region..West + Systolic.BP_mean + Diastolic.BP_mean + Mean.BP_mean + Systolic.BP_std + Diastolic.BP_std + Mean.BP_std + Exhaled.MV_mean + Exhaled.TV..patient._mean + FiO2_mean + LPM.O2_mean + Mean.Airway.Pressure_mean + PEEP_mean + Peak.Insp..Pressure_mean + Plateau.Pressure_mean + Pressure.Support_mean + RR_mean + SaO2_mean + TV.kg.IBW_mean + Tidal.Volume_mean + Total.RR_mean + Vent.Rate_mean + Exhaled.MV_std + Exhaled.TV_std + FiO2_std + LPM.O2_std + Mean.Airway.Pressure_std + PEEP_std + Peak.Insp..Pressure_std + Plateau.Pressure_std + Pressure.Support_std + RR_std + SaO2_std + TV.kg.IBW_std + Tidal.Volume_std + Total.RR_std + Vent.Rate_std + Basos_mean + Eos_mean + Lymphocytes_mean + Monos_mean + Polys_mean + ALT_mean + AST_mean + BUN_mean + Base.Excess_mean + FiO2_mean.1 + HCO3_mean + Hct_mean + Hgb_mean + MCH_mean + MCHC_mean + MCV_mean + MPV_mean + O2.Sat_mean + PT_mean + PT...INR_mean + PTT_mean + RBC_mean + RDW_mean + WBC_mean + Albumin_mean + Alkaline.phos._mean + Anion.gap_mean + Bedside.glucose_mean + Bicarbonate_mean + Calcium_mean + Chloride_mean + Creatinine_mean + Glucose_mean + Lactate_mean + Magnesium_mean + pH_mean + paCO2_mean + paO2_mean + Phosphate_mean + Platelets_mean + Potassium_mean + Sodium_mean + Bilirubin_mean + Protein_mean + Troponin_mean + Urinary.specific.gravity_mean + Basos_std + EOS_std + Lymphocytes_std + Monos_std + Polys_std + ALT_std + AST_std + BUN_std + Base.Excess_std + FiO2_std.1 + HCO3_std + Hct_std + Hgb_std + MCH_std + MCHC_std + MCV_std + MPV_std + O2.Sat_std + PT_std + PT...INR_std + PTT_std + RBC_std + RDW_std + WBC_std + Albumin_std + Alkaline.phos._std + Anion.gap_std + Bedside.glucose_std + Bicarbonate_std + Calcium_std + Chloride_std + Creatinine_std + Glucose_std + Lactate_std + Magnesium_std + pH_std + paCO2_std + paO2_std + Phosphate_std + Platelets_std + Potassium_std + Sodium_std + Bilirubin_std + Protein_std + Troponin_std + Urinary.specific.gravity_std, data =train, family = binomial)
lr <- lrm(MI ~ Creatinine_std + Diastolic.BP_std + AST_mean + RDW_std + Systolic.BP_std + Troponin_mean + BUN_std + Time.Until.Discharge + APACHE.Score, data=train)

X <- data[, 0:192]

y_proba_lr <- predict(lr, as.matrix(X), type="response")

ddist <- datadist(train)
options(datadist='ddist')

nom <- nomogram(lr, funlabel="Risk of Secondary MI")
plot(nom)

nom <- nomogram(model, funlabel="Risk of Secondary MI")
plot(nom)

DynNom(lr, train, clevel = 0.9, m.summary = "formatted")

# For death prediction

data = read.csv("df_Death.csv")

train = read.csv("df_Death_Train.csv")

model <- xgboost::xgb.load('XGBoost.Death')

X <- train[, 0:192]
y <- train[, ncol(train)]

train$APACHE.Score[train$APACHE.Score < 0] <- 0
train$APACHE.Score <- train$APACHE.Score * 100
train$Lactate_mean[train$Lactate_mean < 0] <- 0
train$AST_mean[train$AST_mean < 0] <- 0
train$Time.Until.Discharge <- train$Time.Until.Discharge / 60
train$Lactate_mean[train$Lactate_mean < 0] <- 0
train$Tidal.Volume_mean <- train$Tidal.Volume_mean / 100
train$Tidal.Volume_mean[train$Tidal.Volume_mean < 0] <- 0
train$LPM.O2_mean[train$LPM.O2_mean < 0] <- 0

lr <- lrm(Death ~ LPM.O2_mean + Tidal.Volume_mean + AST_mean + Lactate_mean + Albumin_mean + WBC_mean + FiO2_mean + Bicarbonate_mean + Time.Until.Discharge + APACHE.Score, data=train)

ddist <- datadist(train[, cbind('LPM.O2_mean', 'Tidal.Volume_mean', 'AST_mean', 'Lactate_mean', 'Albumin_mean', 'WBC_mean', 'FiO2_mean', 'Bicarbonate_mean', 'Time.Until.Discharge', 'APACHE.Score')])
options(datadist='ddist')

nom <- nomogram(lr, funlabel="Risk of Death")
plot(nom)

nom <- nomogram(model, funlabel="Risk of Secondary MI")
plot(nom)

DynNom(lr, train, clevel = 0.9, m.summary = "formatted")

# Revised experiments with narrowed down cohort

train$RDW_mean[train$RDW_mean < 0] <- 0
train$Glucose_mean[train$Glucose_mean > 400] <- 0
train$AST_mean[train$AST_mean < 0] <- 0
train$Time.Until.Discharge <- train$Time.Until.Discharge / 60
train$Lactate_mean[train$Lactate_mean < 0] <- 0
train$Tidal.Volume_mean <- train$Tidal.Volume_mean / 100
train$Tidal.Volume_mean[train$Tidal.Volume_mean < 0] <- 0
train$LPM.O2_mean[train$LPM.O2_mean < 0] <- 0

lr <- lrm(Death ~ Age + Lactate_mean + Systolic.BP_mean + Glucose_mean + WBC_mean + BUN_mean + RDW_mean + Bicarbonate_mean, data=train)

ddist <- datadist(train[, cbind('Age', 'Lactate_mean', 'Systolic.BP_mean', 'Glucose_mean', 'WBC_mean', 'BUN_mean', 'RDW_mean', 'Bicarbonate_mean')])
options(datadist='ddist')

png(file="nomogram_death.png", res=200)
nom <- nomogram(lr, funlabel="Risk of Death")
plot(nom)
dev.off()

nom <- nomogram(model, funlabel="Risk of Secondary MI")
plot(nom)
