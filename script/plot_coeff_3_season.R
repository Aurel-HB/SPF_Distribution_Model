### plot coeff and summary ####
# covariate model ####
z.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
z.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
z.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)
z.spring.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
y.spring.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
p.spring.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
z.summer.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
y.summer.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
p.summer.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
z.fall.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
y.fall.Dist <- data.frame(as.numeric(periode), 0, 0, 0)
p.fall.Dist <- data.frame(as.numeric(periode), 0, 0, 0)

names(z.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(z.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(z.intercept.fall) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.fall) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.fall) <- c("year", "mean", "borne1", "borne2")
names(z.spring.Dist) <- c("year", "mean", "borne1", "borne2")
names(y.spring.Dist) <- c("year", "mean", "borne1", "borne2")
names(p.spring.Dist) <- c("year", "mean", "borne1", "borne2")
names(z.summer.Dist) <- c("year", "mean", "borne1", "borne2")
names(y.summer.Dist) <- c("year", "mean", "borne1", "borne2")
names(p.summer.Dist) <- c("year", "mean", "borne1", "borne2")
names(z.fall.Dist) <- c("year", "mean", "borne1", "borne2")
names(y.fall.Dist) <- c("year", "mean", "borne1", "borne2")
names(p.fall.Dist) <- c("year", "mean", "borne1", "borne2")

for (annee in as.numeric(periode)){
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  
  z.intercept.spring <- z.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[1,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[1,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[1,5], borne2))
  
  y.intercept.spring <- y.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[2,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[2,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[2,5], borne2))
  
  p.intercept.spring <- p.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[3,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[3,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[3,5], borne2))
  
  z.intercept.summer <- z.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[4,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[4,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[4,5], borne2))
  
  y.intercept.summer <- y.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[5,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[5,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[5,5], borne2))
  
  p.intercept.summer <- p.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[6,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[6,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[6,5], borne2))
  
  z.intercept.fall <- z.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[7,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[7,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[7,5], borne2))
  
  y.intercept.fall <- y.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[8,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[8,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[8,5], borne2))
  
  p.intercept.fall <- p.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[9,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[9,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[9,5], borne2))
  
  z.spring.Dist <- z.spring.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[10,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[10,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[10,5], borne2))
  
  y.spring.Dist <- y.spring.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[11,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[11,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[11,5], borne2))
  
  p.spring.Dist <- p.spring.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[12,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[12,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[12,5], borne2))
  
  z.summer.Dist <- z.summer.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[13,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[13,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[13,5], borne2))
  
  y.summer.Dist <- y.summer.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[14,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[14,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[14,5], borne2))
  
  p.summer.Dist <- p.summer.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[15,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[15,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[15,5], borne2))
  
  z.fall.Dist <- z.fall.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[16,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[16,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[16,5], borne2))
  
  y.fall.Dist <- y.fall.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[17,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[17,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[17,5], borne2))
  
  p.fall.Dist <- p.fall.Dist %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[18,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[18,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[18,5], borne2))
}


plot.z <- ggplot()+
  geom_ribbon(data = z.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = z.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = z.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = z.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = z.intercept.fall,
            aes(x = year, y = mean, colour = "fall"), linewidth = 2)+
  geom_line(data = z.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Presence_intercept")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.y <- ggplot()+
  geom_ribbon(data = y.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = y.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = y.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = y.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = y.intercept.fall,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = y.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Intensity_intercept")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.p <- ggplot()+ 
  geom_ribbon(data = p.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = p.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_ribbon(data = p.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_line(data = p.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = p.intercept.fall,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = p.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Fishing_density_intercept")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.z.dist <- ggplot()+
  geom_ribbon(data = y.summer.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = z.spring.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = z.fall.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = z.spring.Dist,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = z.fall.Dist,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = y.summer.Dist,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Distance effect on presence")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.y.dist <- ggplot()+
  geom_ribbon(data = y.summer.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = y.spring.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = y.fall.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = y.spring.Dist,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = y.fall.Dist,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = y.summer.Dist,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Distance effect on intensity")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.p.dist <- ggplot()+
  geom_ribbon(data = p.spring.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = p.fall.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_ribbon(data = p.summer.Dist,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_line(data = p.spring.Dist,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = p.fall.Dist,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = p.summer.Dist,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Distance effect on fishing density")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))


plot_grid(plot.z,
          plot.y,
          plot.p,
          plot.z.dist,
          plot.y.dist,
          plot.p.dist
)

# null model ####
z.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.spring <- data.frame(as.numeric(periode), 0, 0, 0)
z.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.summer <- data.frame(as.numeric(periode), 0, 0, 0)
z.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)
y.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)
p.intercept.fall <- data.frame(as.numeric(periode), 0, 0, 0)

names(z.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.spring) <- c("year", "mean", "borne1", "borne2")
names(z.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.summer) <- c("year", "mean", "borne1", "borne2")
names(z.intercept.fall) <- c("year", "mean", "borne1", "borne2")
names(y.intercept.fall) <- c("year", "mean", "borne1", "borne2")
names(p.intercept.fall) <- c("year", "mean", "borne1", "borne2")

for (annee in as.numeric(periode)){
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_null_",annee,"_894",".rds", sep = ""))
  
  z.intercept.spring <- z.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[1,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[1,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[1,5], borne2))
  
  y.intercept.spring <- y.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[2,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[2,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[2,5], borne2))
  
  p.intercept.spring <- p.intercept.spring %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[3,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[3,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[3,5], borne2))
  
  z.intercept.summer <- z.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[4,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[4,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[4,5], borne2))
  
  y.intercept.summer <- y.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[5,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[5,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[5,5], borne2))
  
  p.intercept.summer <- p.intercept.summer %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[6,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[6,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[6,5], borne2))
  
  z.intercept.fall <- z.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[7,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[7,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[7,5], borne2))
  
  y.intercept.fall <- y.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[8,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[8,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[8,5], borne2))
  
  p.intercept.fall <- p.intercept.fall %>% 
    mutate(mean = ifelse( year == annee, summary.res$fixed[9,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$fixed[9,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$fixed[9,5], borne2))
}

plot.z.null <- ggplot()+
  geom_ribbon(data = z.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = z.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = z.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = z.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = z.intercept.fall,
            aes(x = year, y = mean, colour = "fall"), linewidth = 2)+
  geom_line(data = z.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Presence_intercept without covariable")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.y.null <- ggplot()+
  geom_ribbon(data = y.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_ribbon(data = y.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = y.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_line(data = y.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = y.intercept.fall,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = y.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Intensity_intercept without covariable")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))




plot.p.null <- ggplot()+ 
  geom_ribbon(data = p.intercept.spring,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#33BB66AA") +
  geom_ribbon(data = p.intercept.fall,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#FFBB55AA") +
  geom_ribbon(data = p.intercept.summer,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#BBFFFFAA") +
  geom_line(data = p.intercept.spring,
            aes(x = year, y = mean, colour = "spring"),linewidth = 2)+
  geom_line(data = p.intercept.fall,
            aes(x = year, y = mean, colour = "fall"),linewidth = 2)+
  geom_line(data = p.intercept.summer,
            aes(x = year, y = mean, colour = "summer"),linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "Fishing_density_intercept without covariable")+
  scale_color_manual(name = "Season", values = c("spring" = "springgreen4", 
                                                 "fall" = "orangered3",
                                                 "summer" = "royalblue"))

plot_grid(plot.z.null,
                    plot.y.null,
                    plot.p.null,
          plot.z,
          plot.y,
          plot.p)



## WAIC and convergence ####
WAIC.null <- c()
WAIC.dist <- c()
Ok.null <- c()
Ok.dist <- c()
mode.status.null <- c()
mode.status.dist <- c()
p.eff.null <- c()
p.eff.dist <- c()

for (annee in as.numeric(periode)){
  
  res.null <- readRDS(paste(local_path,"/code/output/result/result_summer_null_",annee,"_894",".rds", sep = ""))
  res.dist <- readRDS(paste(local_path,"/code/output/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  
  
  
  WAIC.null <- c(WAIC.null, summary(res.null)$waic[[1]])
  WAIC.dist <- c(WAIC.dist, summary(res.dist)$waic[[1]])
  Ok.null <- c(Ok.null, res.null[["ok"]])
  Ok.dist <- c(Ok.dist, res.dist[["ok"]])
  mode.status.null <- c(mode.status.null, res.null$mode$mode.status)
  mode.status.dist <- c(mode.status.dist, res.dist$mode$mode.status)
  p.eff.null <- c(p.eff.null, summary(res.null)$waic[[2]])
  p.eff.dist <- c(p.eff.dist, summary(res.dist)$waic[[2]])
}

year <- as.numeric(periode)

check.model <- data.frame(year, WAIC.null, Ok.null, mode.status.null,
                          p.eff.null, WAIC.dist, Ok.dist, mode.status.dist, p.eff.dist)
saveRDS(object = check.model, file = paste(path.output, "coeff/3_table_indic_conv.rds", sep = ""))
write.csv(check.model, file = paste(path.output, "coeff/3_table_indic_conv.csv", sep = ""))

# graph illisible
#plot.waic <- ggplot()+
#  geom_line(data = check.model,
#            aes(x = year, y = WAIC.null, colour = "Model_null"),linewidth = 2)+
#  geom_line(data = check.model,
#            aes(x = year, y = WAIC.dist, colour = "Model_covariate"),linewidth = 2)+
#  labs(x = "Year", y = "Mean", title = "WAIC")+
#  scale_color_manual(name = "Season", values = c("Model_null" = "black",
#                                                 "Model_covariate" = "blue"))+ 
#                       theme_bw()
                
plot.waic <- ggplot()+
  geom_line(data = check.model,
             aes(x = year, y = (WAIC.null - WAIC.dist)), color = "black", linewidth = 1)+
  geom_line(data = check.model,
            aes(x = year, y = 0),  color = "blue")+
  labs(x = "Year", y = "", title = "Difference between WAIC of Models without covariate by WAIC of covariate Models")+
  theme_light()




### plot xc range and rho ####

xc.range <- data.frame(as.numeric(periode), 0, 0, 0)
xc.stdev <- data.frame(as.numeric(periode), 0, 0, 0)
xc.Rho <- data.frame(as.numeric(periode), 0, 0, 0)

names(xc.range) <- c("year", "mean", "borne1", "borne2")
names(xc.stdev) <- c("year", "mean", "borne1", "borne2")
names(xc.Rho) <- c("year", "mean", "borne1", "borne2")

for (annee in as.numeric(periode)){
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  
  xc.range <- xc.range %>% 
    mutate(mean = ifelse( year == annee, summary.res$hyperpar[5,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$hyperpar[5,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$hyperpar[5,5], borne2))
  
  xc.stdev <- xc.stdev %>% 
    mutate(mean = ifelse( year == annee, summary.res$hyperpar[6,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$hyperpar[6,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$hyperpar[6,5], borne2))
  
  xc.Rho <- xc.Rho %>% 
    mutate(mean = ifelse( year == annee, summary.res$hyperpar[7,1], mean)) %>%
    mutate(borne1 = ifelse( year == annee, summary.res$hyperpar[7,3], borne1)) %>%
    mutate(borne2 = ifelse( year == annee, summary.res$hyperpar[7,5], borne2))
  
}


plot.xc.range <- ggplot()+
  geom_ribbon(data = xc.range,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#999999AA") +
  geom_line(data = xc.range,
            aes(x = year, y = mean), color = "black",linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "xc.range")

plot.xc.stdev <- ggplot()+
  geom_ribbon(data = xc.stdev,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#999999AA") +
  geom_line(data = xc.stdev,
            aes(x = year, y = mean), color = "black",linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "xc.stdev")

plot.xc.Rho <- ggplot()+
  geom_ribbon(data = xc.Rho,
              aes(x=year, ymin = borne1, ymax = borne2), fill = "#999999AA") +
  geom_line(data = xc.Rho,
            aes(x = year, y = mean), color = "black",linewidth = 2)+
  labs(x = "Year", y = "Mean", title = "xc.Rho")


plot_grid(plot.xc.range,
          plot.xc.stdev,
          plot.xc.Rho
)
