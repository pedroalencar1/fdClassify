#plot events and variables for a specific Date interval


plot_series <- function(years, complete_series){

  import::from(ggplot2,.into="", .all = T)

      # years <- c(2001,2014)
  sp_ratio <- 90*(diff(years) + 1) #90 per added year

  mo_df<- complete_series[,c(1,2,8,10:12)]
  mo_df <- mo_df[(lubridate::year(mo_df$Date) >= years[1]  & lubridate::year(mo_df$Date) <= years[2]),]
  mo_df <- tidyr::gather(mo_df, key ='variable', value = 'value', 3:6)
  mo_df$Date <- as.Date(mo_df$Date)
  mo_df <- tidyr::gather(mo_df, key ='model', value = 'is.fd', 2)

  ford_df<- complete_series[,c(1,3,8,13)]
  ford_df <-ford_df[(lubridate::year(ford_df$Date) >= years[1]  & lubridate::year(ford_df$Date) <= years[2]),]
  ford_df <- tidyr::gather(ford_df, key ='variable', value = 'value', 3:4)
  ford_df$Date <- as.Date(ford_df$Date)
  ford_df <- tidyr::gather(ford_df, key ='model', value = 'is.fd', 2)

  pendergrass_df<- complete_series[,c(1,4,9)]
  pendergrass_df <-pendergrass_df[(lubridate::year(pendergrass_df$Date) >= years[1]  & lubridate::year(pendergrass_df$Date) <= years[2]),]
  pendergrass_df <- tidyr::gather(pendergrass_df, key ='variable', value = 'value', 3)
  pendergrass_df <- tidyr::gather(pendergrass_df, key ='model', value = 'is.fd', 2)

  noguera_df<- complete_series[,c(1,5,9,12,14)]
  noguera_df$precipitation <- noguera_df$precipitation/10
  noguera_df$et0 <- noguera_df$et0/10
  noguera_df <-noguera_df[(lubridate::year(noguera_df$Date) >= years[1]  & lubridate::year(noguera_df$Date) <= years[2]),]
  noguera_df <- tidyr::gather(noguera_df, key ='variable', value = 'value', 3:5)
  noguera_df <- tidyr::gather(noguera_df, key ='model', value = 'is.fd', 2)

  chistian_df<- complete_series[,c(1,6,9,10,15)]
  chistian_df <-chistian_df[(lubridate::year(chistian_df$Date) >= years[1]  & lubridate::year(chistian_df$Date) <= years[2]),]
  chistian_df <- tidyr::gather(chistian_df, key ='variable', value = 'value', 3:5)
  chistian_df <- tidyr::gather(chistian_df, key ='model', value = 'is.fd', 2)

  osman_df<- complete_series[,c(1,7,8,13)]
  osman_df <-osman_df[(lubridate::year(osman_df$Date) >= years[1]  & lubridate::year(osman_df$Date) <= years[2]),]
  osman_df <- tidyr::gather(osman_df, key ='variable', value = 'value', 3:4)
  osman_df <- tidyr::gather(osman_df, key ='model', value = 'is.fd', 2)
  osman_df$Date <- as.Date(osman_df$Date)


  p_mo_df <-
    ggplot2::ggplot(data = mo_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= (value+15)/45, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= c(0.2,0.4,0.6,.8),
                       sec.axis = sec_axis(trans = ~ (.*45 - 15),
                                           name = '(mm; %; ºC)'))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = '', y = '')+
    scale_colour_discrete(name  ="Legend", labels = c('ETa', 'P', 'SWC', 'T'))+
    theme(legend.position =  'right',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.title.y.left = element_text(size = 11, margin = margin(r = 10)),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_mo_df

  ############## Ford and Labosier
  p_ford_df <-
    ggplot(data = ford_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= value/40, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= c(0.25,0.50,0.75),
                       sec.axis = sec_axis(trans = ~ (.*40),
                                           name = '(%)'))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = ' ', y = ' ')+
    scale_colour_discrete(name  ="Legend", labels = c('SWC','20th P.'))+
    theme(legend.position =  'right',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_ford_df

  ############## Pendergrass

  p_pendergrass_df <-
    ggplot(data = pendergrass_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= value/6.8, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= c(0.2,0.4,0.6,0.8),
                       sec.axis = sec_axis(trans = ~ (.*6.8),
                                           name = '(mm)'))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = '', y = '')+
    scale_colour_discrete(name  ="Legend", labels = c('ET0'))+
    scale_x_date(date_labels = "%m-%Y")+
    theme(legend.position =  'right',
          axis.text.x = element_text(size = 10),
          # axis.title.x = element_text(size = 11, margin = margin(t = 10)),
          axis.ticks.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_pendergrass_df
  ############## Noguera
  p_noguera_df <-
    ggplot(data = noguera_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= (value+3)/6, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= c(0.25,0.50,0.75),
                       sec.axis = sec_axis(trans = ~ (.*6 - 3),
                                           name = '(cm)'))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = '', y = '')+
    scale_colour_discrete(name  ="Legend", labels = c('ET0','P', 'SPEI'))+
    theme(legend.position =  'right',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.ticks.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_noguera_df

  ############## Christian
  p_chistian_df <-
    ggplot(data = chistian_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= (value +3)/9, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= seq(1/6,5/6,by=1/6),
                       sec.axis = sec_axis(trans = ~ (.*9 - 3),
                                           name = '(mm)',
                                           breaks = c(-3,0,3,6)
                       ))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = '', y = '')+
    scale_colour_discrete(name  ="Legend", labels = c('ET0', 'ETa', 'SESR'))+
    theme(legend.position =  'right',
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.text.y.left = element_blank(),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_chistian_df

  ##############Osman et al
  p_osman_df <-
    ggplot(data = osman_df, aes(x = Date))+
    theme_bw()+
    facet_wrap(facets = vars(model),nrow = 6,strip.position = "left")+
    scale_colour_brewer(type = "div", palette = 'Spectral') +
    geom_line(aes(y= value/40, colour = variable),size=1)+
    scale_y_continuous(breaks = c(0,1),minor_breaks	= c(0.25,0.50,0.75),
                       sec.axis = sec_axis(trans = ~ (.*40),
                                           name = '(%)'))+
    geom_bar(aes(weight = is.fd), alpha = 0.7, width = 1)+
    labs(x = '', y = '')+
    scale_colour_discrete(name  ="Legend", labels = c('SWC','20th P.'))+
    theme(legend.position =  'right',
          axis.text.x = element_blank(),
          axis.text.y.right = element_text(size = 10),
          axis.ticks.x = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.title.y.right = element_text(size = 11, margin = margin(l = 10)),
          strip.background = element_rect(colour = 'black', fill = 'white'),
          strip.text = element_text(size = 10, face = 'bold'))+
    coord_fixed(ratio = sp_ratio,ylim = c(0,1),expand = F)

  # p_osman_df

  g1 <- ggplotGrob(p_chistian_df)
  g2 <- ggplotGrob(p_ford_df)
  g3 <- ggplotGrob(p_mo_df)
  g4 <- ggplotGrob(p_noguera_df)
  g5 <- ggplotGrob(p_osman_df)
  g6 <- ggplotGrob(p_pendergrass_df)


  g2$widths <- g1$widths
  g3$widths <- g1$widths
  g4$widths <- g1$widths
  g5$widths <- g1$widths
  g6$widths <- g1$widths

  g2$heights <- g1$heights
  g3$heights <- g1$heights
  g4$heights <- g1$heights
  g5$heights <- g1$heights
  g6$heights <- g1$heights

  grob1 <- gridExtra::arrangeGrob(grobs = list(g1, g2,g3,g4,g5,g6), nrow = 6)
  plot1 <- gridExtra::grid.arrange(grob1)

  ggsave(filename = paste(years[1],'-',years[2],'.png', sep = ''), plot = plot1, units = 'cm', width = 21, height = 29.7, dpi = 600)

}

