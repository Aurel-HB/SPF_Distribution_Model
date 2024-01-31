#this project return the distribution per month of small pelagic fish in the bay of Biscay

####################################################################################
#data exploration
####################################################################################

####### prepare the global environment #######
source("C:/Users/ahebertb/Desktop/StageAurel/code/script/boot.R")
#all the path, package and object use in below script


####### prepare the data #######
source(paste(local_path, "code/script/process_data.R"))
#take the data not tranformed and prepare same format for each data

####### load the data #######
source(paste(local_path, "code/script/load_data.R"))


##############################
#exploration of the data #####
##############################
####### exploration of the vms data and especially the cpue with map #######
source(paste(local_path, "code/script/cpue_grid_plot.R"))
# use EchoR to create grid map of the CPUE of the anchovy and sardine
# this grid maps are an agglomeration of all the fishing data

####### create table for each year and each survey #######
source(paste(local_path, "code/script/table_per_year.R"))
# this split the big table in smaller table but not necesary for the work

####### data plot #######
source(paste(local_path, "code/script/data_grid_plot.R"))
# use EchoR to create grid map of the presence and biomass of the anchovy and sardine
# this grid maps are per surveys and one is from commercial data

####### advanced exploration of the vms data to resolve issue of cpue #######
source(paste(local_path, "code/script/analyse_cpue.R"))
####### Research of correlation and filter on the cpue #######
source(paste(local_path, "code/script/correlation_cpue_biomass.R"))
source(paste(local_path, "code/script/PS_PTM_biomass_correlation.R"))

####################################################################################
#model
####################################################################################

#select the appropriate gear
sard_VMS_tot <- sard_VMS_tot %>% filter(type %in% c("PS","PTM"))
anch_VMS_tot <- anch_VMS_tot %>% filter(type %in% c("PS","PTM"))
#prepare the data for input the model
source(paste(local_path, "code/script/prepare_data_model.R", sep = ""))

#show data for the model
source(paste(local_path, "code/script/show_survey_data.R", sep = ""))
source(paste(local_path, "code/script/show_fishing_data.R", sep = ""))

#run sardine model without covariate
source(paste(local_path, "code/script/boucle_model_3_saison_sard.R", sep = ""))
#run anchovy model without covariate
source(paste(local_path, "code/script/boucle_model_3_saison_anch.R", sep = ""))

#add the covariates to the data input
#(This have been realised by my supervisor.
#So it is not on my git)

#run sardine model with distance from the coast
source(paste(local_path, "code/script/boucle_model_3_saison_cov_sard.R", sep = ""))

##########################################
#Result and figure from the master report
##########################################
source(paste(local_path, "code/script/pred_3_season.R", sep = "")) #prediction
source(paste(local_path, "code/script/latent_partage.R", sep = "")) # shared random field
source(paste(local_path, "code/script/show_field.R", sep = "")) # specific random field
source(paste(local_path, "code/script/residual.R", sep = "")) # residual from the model


##########################
# other model in rmarkdow
##########################
# season specific model for sardine in spring = Printemps_sard.rmd
# season specific model for sardine in fall = Automne_sard.rmd
# season specific model for anchovy in spring = Printemps_anch.rmd
# season specific model for anchovy in fall = Automne_anch.rmd

# one model for 14 years and 3 seasons but need a really strong calculator
# = Saison.rmd
