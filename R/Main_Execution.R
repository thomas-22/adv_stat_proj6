# --------------------------------------
# MAIN EXECTUION
# --------------------------------------




# -------------------------
# Overview & Source Everything
# -------------------------

#Preprocess
source("R/Datafusion.R")
source("R/CalcSenderPosDist_new.R")
source("R/Assign_FCMData_to_Stressors.R")

#Plots on Data
source("R/Plot_FinalData.R")

#Modelling
source("R/XGBoost_Model.R")



# -----------------------------------
# DATA PREPROCESSING
# -----------------------------------
# -------------------------
# Datafusion
# -------------------------
prepared_data <- run_datafusion()

Movement <- prepared_data$Movement
FCMStress <- prepared_data$FCMStress
HuntEventsreduced <- prepared_data$HuntEventsreduced
HuntEvents_Reduced_UTM_New <- prepared_data$HuntEvents_Reduced_UTM_New


# -------------------------
# CalcSenderPosDist_new
# -------------------------
calculated_data <- CalcSender_Position_Distance()

StressEvents_new <- calculated_data$StressEvents_new
no_t_before <- calculated_data$no_t_before
interpolated_positions <- calculated_data$interpolated_positions


# -------------------------
# Assign_FCMData_to_Stressors
# -------------------------
assigned_data <- Assign_FCMData_to_Hunts()

interesting_data <- assigned_data$interesting_data
data_cleanedup <- assigned_data$data_cleanedup
data_cleanedup_min_distance <- assigned_data$data_cleanedup_min_distance
data_cleanedup_min_timediff <- assigned_data$data_cleanedup_min_timediff
combo_min_dist_timediff <- assigned_data$combo_min_dist_timediff
combo_min_dist_timediff_no_dupes <- assigned_data$combo_min_dist_timediff_no_dupes

# -------------------------
# Plot Data
# -------------------------
plot_ng_as_func_of_dist_timediff()
plot_data_2d()
plot_data_3d

generate_hist_timediff
plot_lognorm_gamma_univar_independent
plot_collar_t_raw


# -----------------------------------
# MODELING
# -----------------------------------

# -------------------------
# XGBoost Model
# -------------------------

# Set tune = TRUE if you want to run hyperparameter tuning.
# Leave it = FALSE if you want to use the included result of the tuning. (Models/final_xgboost..)
# WARNING: Setting tune = TRUE means the function will take very long (Multiple Hours+) to execute due to many computations.
# Max_Iterations: Only relevant if tune = TRUE

xg_boost_results <- XGBoost_run_default_pipeline(data_cleanedup,
                                                 covariables = c("TimeDiff", "Distance"),
                                                 tune = FALSE,
                                                 max_iterations = 3)

xg_boost_results_transformed <- XGBoost_run_transformed_pipeline(data_cleanedup,
                                                                 tune = TRUE,
                                                                 max_iterations = 3)

