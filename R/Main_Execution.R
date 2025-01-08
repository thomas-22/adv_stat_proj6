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

#Declare Variables
ignore_distance_filter <- FALSE
distance_threshold <- 100000 #in Meters
gut_retention_time_lower <- 0 #in Hours
gut_retention_time_upper <- 1000000 #in Hours
gut_retention_mean <- (gut_retention_time_lower + gut_retention_time_upper) / 2


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

Draw_Illustration_Map()


#Example Calls for one of the datasets
plot_ng_as_func_of_dist_timediff(data_cleanedup, chosen_var = "Distance")
plot_ng_as_func_of_dist_timediff(data_cleanedup, chosen_var = "TimeDiff")

plots_2d <- plot_data_2d()
lapply(plots_2d, print)

plots_3d <- plot_data_3d()
lapply(plots_3d, print)

generate_hist_timediff()
plot_lognorm_gamma_univar_independent()
plot_collar_t_raw()


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
                                                                 tune = FALSE,
                                                                 max_iterations = 3)

#3D Figure of Model
xg_boost_results$plotly_fig
#3D Figure of Model of transformed variables
xg_boost_results_transformed$plotly_fig 

