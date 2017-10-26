#' 
#'  Airport delays.
#' 
#'  @return A plot of airport delays.
#' @export
#' 

visualize_airport_delays<- function() {
  requireNamespace(dplyr)
  requireNamespace(nycflights13)
  requireNamespace(ggplot2)
  flights_df<-tbl_df(flights)
  airports_df<-tbl_df(airports)
  
  delays<-flights_df %>%    
    group_by(dest) %>%
    summarise(avg = mean(na.omit(arr_delay))) %>%
    arrange(avg)
  
  airports_df$dest<-airports_df$faa
  
  delays_coor<-left_join(delays,airports_df,by="dest")
  
  p <- ggplot() + 
    aes(x = na.omit(delays_coor$lat[1:104]),
        y = na.omit(delays_coor$lon[1:104]),
        color=na.omit(delays_coor$avg[-c(4,18,49,54)])) + 
    
    geom_point(size=3)+
    scale_colour_gradient(low="white", high="blue",
                          limits=c(min(na.omit(delays_coor$avg)),
                                   max(na.omit(delays_coor$avg))))+
    ggtitle("Average arrival delays") + 
    ylab("Longitude") +
    xlab("Latitude") +
    theme_bw() +
    theme(axis.title.y = element_text(angle = 0, hjust = 1))+
    labs(colour = "AVG")
  return(p)
  
}

