#load the data in rds per year

assign(paste('sard_PEL_tot'), readRDS(paste(path_data,"Data_sard_PEL",".rds",sep = "")))
assign(paste('sard_EVH_tot'), readRDS(paste(path_data,"Data_sard_EVH",".rds",sep = "")))
assign(paste('sard_JUN_tot'), readRDS(paste(path_data,"Data_sard_JUN",".rds",sep = "")))
assign(paste('sard_VMS_tot'), readRDS(paste(path_data,"Data_sard_VMS",".rds",sep = "")))
assign(paste('anch_PEL_tot'), readRDS(paste(path_data,"Data_anch_PEL",".rds",sep = "")))
assign(paste('anch_EVH_tot'), readRDS(paste(path_data,"Data_anch_EVH",".rds",sep = "")))
assign(paste('anch_JUN_tot'), readRDS(paste(path_data,"Data_anch_JUN",".rds",sep = "")))
assign(paste('anch_VMS_tot'), readRDS(paste(path_data,"Data_anch_VMS",".rds",sep = "")))


