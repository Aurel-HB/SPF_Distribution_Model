#process_data

#Create big frame for a specific species per survey
#sardine####
#pelgas data 
saveRDS(extract_Pelgas_species("SARD-PIL", local_path), file = paste(path_data,'Data_sard_PEL','.rds',sep=''))

#evhoe data 
saveRDS(extract_Evhoe_species("SARD-PIL", local_path), file = paste(path_data,'Data_sard_EVH','.rds',sep=''))

#juvena data 
saveRDS(extract_Juvena_species("SARD-PIL", local_path), file = paste(path_data,'Data_sard_JUN','.rds',sep=''))

#VMS data 
saveRDS(extract_VMS_species("SARD-PIL", local_path), file = paste(path_data,'Data_sard_VMS','.rds',sep=''))

#anchois####

#pelgas data 
saveRDS(extract_Pelgas_species("ENGR-ENC", local_path), file = paste(path_data,'Data_anch_PEL','.rds',sep=''))

#evhoe data 
saveRDS(extract_Evhoe_species("ENGR-ENC", local_path), file = paste(path_data,'Data_anch_EVH','.rds',sep=''))

#juvena data 
saveRDS(extract_Juvena_species("ENGR-ENC", local_path), file = paste(path_data,'Data_anch_JUN','.rds',sep=''))

#VMS data 
saveRDS(extract_VMS_species("ENGR-ENC", local_path), file = paste(path_data,'Data_anch_VMS','.rds',sep=''))

