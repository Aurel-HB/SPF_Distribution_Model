
bb <- st_bbox(study_domain_sf)
x <- seq(bb$xmin - 1, bb$xmax + 1, length.out = 100)
y <- seq(bb$ymin - 1, bb$ymax + 1, length.out = 100)
dp <- as.matrix(expand.grid(x, y))
plot(dp, asp = 1)

p <- st_as_sf(data.frame(x = dp[, 1], y = dp[, 2]),
              coords = c("x", "y")
) %>% st_set_crs(., 4326)


p <- p %>% mutate(inside = in_out_domain(., study_domain_sf))

p <- p %>% filter( inside == 1)

p <- p %>% mutate(Xgd = st_coordinates(.) %>%
                    as.data.frame() %>%
                    pull(X),
                  Ygd = st_coordinates(.)%>%
                    as.data.frame() %>%
                    pull(Y)) %>%  as.matrix(.)

p <- p[,3:4] %>% as.data.frame(.)

proj_mat <- data.frame()
an <- c()

for (variable in 2009:(2009+timesn-1)) {
  an <- c(an,rep(variable,nrow(p)))
  proj_mat <- rbind(proj_mat,p)
}

proj_mat <- data.frame(proj_mat,an)
names(proj_mat) <- c("x", "y", "time")

proj_mat$pred_mean <-  res.jo$summary.random$xc [, "mean"]


















