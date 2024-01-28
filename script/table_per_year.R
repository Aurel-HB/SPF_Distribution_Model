#Create specific frame per year per survey####
for (an in periode){
  #sardine####
  
  #pelgas data 
  saveRDS(sard_PEL_tot %>% filter (year == an), file = paste(path_data,'sard_',an,'_PEL','.rds',sep=''))
  
  #evhoe data 
  saveRDS(sard_EVH_tot %>% filter (year == an), file = paste(path_data,'sard_',an,'_EVH','.rds',sep=''))
  
  #juvena data 
  saveRDS(sard_JUN_tot %>% filter (year == an), file = paste(path_data,'sard_',an,'_JUN','.rds',sep=''))
  
  #VMS data 
  saveRDS(sard_VMS_tot %>% filter (year == an), file = paste(path_data,'sard_',an,'_VMS','.rds',sep=''))
  
}


for (an in periode){
  #anchois####
  #pelgas data 
  saveRDS(anch_PEL_tot %>% filter (year == an), file = paste(path_data,'anch_',an,'_PEL','.rds',sep=''))
  
  #evhoe data 
  saveRDS(anch_EVH_tot %>% filter (year == an), file = paste(path_data,'anch_',an,'_EVH','.rds',sep=''))
  
  #juvena data 
  #saveRDS(anch_JUN_tot %>% filter (year == an), file = paste(path_data,'anch_',an,'_JUN','.rds',sep=''))
  
  #VMS data 
  saveRDS(anch_VMS_tot %>% filter (year == an), file = paste(path_data,'anch_',an,'_VMS','.rds',sep=''))
}