job::job({
  # exp(U.sigma) is upper limit for point intensity in each cell explained by
  #   random component rather than covariates
  U.sigma <- 0.1
  alpha.sigma <- 0.01
  U.phi <- 0.5
  alpha.phi <- 0.01

  mm_7 <- inla(n_pos ~
                 f(transmitter, model = 'iid') +
                 f(id, model = 'rw2diid',
                         # scale.model = T,
                       # Needs the number of rows and columns
                       #   Using the bounding box divided by grid size
                       ncol = (st_bbox(grid)$xmax - st_bbox(grid)$xmin) / 10,
                       nrow = (st_bbox(grid)$ymax - st_bbox(grid)$ymin) / 10,
                       hyper = list(prec = list(prior = 'pc.prec',
                                                param = c(U.sigma, alpha.sigma)),
                                    phi = list(prior = 'pc',
                                               param = c(U.phi, alpha.phi)))),
  family = "poisson", data = mod_data10,
  E = area,
  control.compute = list(waic = TRUE,
                         dic = TRUE))

},
import = c(mod_data10, grid),
packages = c('INLA', 'sf')
)

pf <- function(dd){
  md <- mod_data10 %>%
    mutate(re_spat = dd$summary.random$id[mod_data10$id, "0.5quant"],
           resp_re_spat = exp(re_spat))


  ggplot() +
    geom_sf(data = distinct(md, id, .keep_all = T),
            aes(fill = resp_re_spat), color = NA) +
    geom_sf(data = mh_shape, fill = NA, size = 1, color = 'black') +
    labs(fill = 'N Positions') +
    scale_fill_viridis_c() +
    theme_minimal() +
    theme(legend.position = c(0.8, 0.7),
          legend.background = element_rect(fill = 'white'),
          axis.text = element_text(size = 15),
          axis.text.y = element_text(angle = 45))
}

pf(mm_5_3_g)

job::job({
  # md <- dplyr::mutate(mod_data10, id2 = id)

  mm2 <- inla(n_pos ~
                Group +
                 f(transmitter, model = 'iid') +
                 f(id, model = 'rw2d',
                   scale.model = T,
                   # Needs the number of rows and columns
                   #   Using the bounding box divided by grid size
                   ncol = (st_bbox(grid)$xmax - st_bbox(grid)$xmin) / 10,
                   nrow = (st_bbox(grid)$ymax - st_bbox(grid)$ymin) / 10,
                   hyper = list(prec = list(param = c(20, 0.001)))),
               family = "poisson", data = mod_data10,
               E = area,
               control.compute = list(dic = TRUE))

},
import = c(mod_data10, grid),
packages = c('INLA', 'sf')
)

