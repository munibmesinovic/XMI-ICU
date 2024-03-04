# XMI-ICU

The code repository for XMI-ICU, an eXplainable machine learning framework for Myocardial Infarction prediction in the ICU using pseudo-dynamic learning from time-series data. The paper can be found at https://arxiv.org/pdf/2305.06109.

The preprocessing folders contain Python scripts adapted from Rochetau for pre-processing the databases of eICU and MIMIC IV alongside postgreSQL commands. We edited some of the code for our purposes and different cohort selection criteria.

The notebooks show interactive code for how the results and plots were generated.

The .R scripts contain code for generating the decision, clinical impact curves, and the nomogram.

The pre-trained models can be found under the title XGBoost.Death and XGBoost.MI
