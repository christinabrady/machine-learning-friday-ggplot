library(ggplot)
library(dplyr)
library(ggvis)


### fake data set for ggplot, ggvis, shiny presentation
fk_dat <- data.frame(let = sample(LETTERS[1:5], 500, replace = TRUE,
                                  prob=c(0.37, 0.25, 0.13, 0.10, 0.15)), 
                     num = sample(seq(0, 5, 0.5), 500, replace= TRUE), 
                     grk = sample(c("alpha", "beta", "gamma", "delta", "epsilon", "zeta"), 
                                  500, replace=TRUE, prob=c(0.2, 0.37, 0.13, 0.10, 0.15, 0.05))
                     )

## basic histogram
g_hist <- ggplot(fk_dat, aes(x=let)) + geom_histogram()

## histogram broken down by grk
g_hist_col <- ggplot(fk_dat, aes(x=let, fill=grk)) + geom_histogram()

## change the order of the fill variable
fk_dat$grk <- factor(fk_dat$grk, levels = c("zeta", "epsilon", "delta", "gamma", "beta", "alpha"))
g_hist_col2 <- ggplot(fk_dat, aes(x=let, fill=grk)) + geom_histogram()

## calculate the totals of num for each letter and create a bar graph
fk_dat <- fk_dat %>%
  group_by(grk) %>%
  mutate(tot_num = sum(num))

g_bar <- ggplot(fk_dat, aes(x=let, y=tot_num)) + geom_bar(stat='identity')
g_bar_col <- ggplot(fk_dat, aes(x=let, y=tot_num, fill=grk)) + geom_bar(stat='identity') ## colors are all over the place

## order the fill variable
g_bar_col <- ggplot(fk_dat, aes(x=let, y=tot_num, fill=grk, order=grk)) + geom_bar(stat='identity')

### unstack
g_bar_unstack <- ggplot(fk_dat, aes(x=let, y=tot_num, fill=grk)) + 
  geom_bar(stat='identity', position="dodge")


### get port traffic data from World Bank API  ####
### Country codes: China (CHN), US (USA), Singapore (SGP), Korea (KOR), Hong Kong (HKG), Malasia (MYS), Japan (JPN), UAE (ARE), Germany (DEU), Spain (ESP), Panama (PAN)
### basic call structure: http://api.worldbank.org/countries/all/indicators/IS.SHP.GOOD.TU?date=2003:2013

library(XML)
library(httr)

get_port_traffic <- function(){
  ports_xml <- xmlToList(xmlParse(GET("http://api.worldbank.org/countries/CHN;USA;SGP;KOR;MYS;JPN;ARE;DEU;ESP;PAN/indicators/IS.SHP.GOOD.TU?date=2003:2013&per_page=121")))
  
  country <- unname(unlist(lapply(ports_xml, function(x){
    x[[2]][1]
  })))
  
  containers <- as.numeric(unname(unlist(lapply(ports_xml, function(x){
    if(!('value' %in% names(x))) return(NA)
    else return(x$value)
  }))))
  
  year <- as.Date(unname(unlist(lapply(ports_xml, function(x){
    x[3]
  }))), "%Y")
  
  df <- data.frame(country = country, 
                   containers = containers,
                   year = year)
  return(df)
}

ports <- get_port_traffic() 
ports <- ports[-111, ]

g_lines <- ggplot(ports, x=year, y=containers) + geom_line()  ## missing env error

g_lines <- ggplot(ports, aes(x=year, y=containers)) + geom_line()  ## un grouped lines

g_lines <- ggplot(ports, aes(x=year, y=containers, group=country)) + geom_line()

g_lines_col <- ggplot(ports, aes(x=year, y=containers, color=country)) + geom_line() + geom_point()

### customization

g_lines_col <- g_lines_col + theme_bw() 

g_lines_col <- g_lines_col + theme(axis.ticks.x = element_line(seq(2003, 2013, 2)),
                                   axis.text.x = element_text(angle=45))

### facets
ports$country <- factor(ports$country)
g_lines_fac <- ggplot(subset(ports, country=c("China", "Japan", "Panama", "Spain")), aes(x=year, y=containers)) +
  facet_grid(.~country)  ## no layers error

g_lines_fac <- ggplot(ports[country %in% c("China", "Japan", "Panama", "Spain"),], aes(x=year, y=containers)) +
  geom_line() + facet_grid(.~country)  


## ggvis
ports %>%
  ggvis(~year, ~containers, fill = ~ country) %>%
  layer_points() %>%
  group_by(country) %>%
  layer_paths()

cont_values <- function(){
  if(is.null(x)) return(NULL)
  row <- ports[ports$id == x$id,]
  paste0(row, "containers", collapse = " ")
}

ports %>%
  ggvis(~year, ~containers, stroke = ~ country) %>%
  layer_points() %>%
  group_by(country) %>%
  layer_paths() %>%
  add_tooltip(cont_values, "click")
  
  
  
  
  
  
  
  
  
  
  