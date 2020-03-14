library(readxl)
library(dplyr)
library(ggpubr)
theme_set(theme_pubr())

show_summary <- function(x) {
  summary(x)
}

plot_distribution <- function(x) {
  
  h<-hist(x, breaks=10, col="red", xlab="Prices", 
          main="Prices distribution") 
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h$mids[1:2])*length(x) 
  lines(xfit, yfit, col="blue", lwd=2)
  
}

plot_density <- function(x) {
  
  d <- density(x) 
  plot(d) 
}


plot_bar_count <- function (dataframe ,x,y) {
  
  ggplot(dataframe, aes_string(x = x , y = y))  +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) + 
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
}

plot_pie <- function(df,x) {
  df <- df %>%
    arrange(desc(df[[x]])) %>%
    mutate(prop = round(counts*100/sum(counts), 1),
           lab.ypos = cumsum(prop) - 0.5*prop)
  ggplot(region1, aes(x = "", y = .data[["prop"]], fill = .data[[x]])) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    coord_polar("y", start = 0)+
    ggpubr::fill_palette("jco")+
    theme_void()
}


JMMI_data = read_excel(path = "./input/March_data.xlsx",sheet = 2)



glass_prices <- filter(JMMI_data, !is.na(q_square_meter_of_glass_price)) %>% select("_index",q_square_meter_of_glass_price)
iron_prices <- filter(JMMI_data, !is.na(q_corrugated_iron_sheet_price_per_1)) %>% select("_index",q_corrugated_iron_sheet_price_per_1)
district <- filter(JMMI_data, !is.na(q_district)) %>% select("_index",q_district)

show_summary(glass_prices$q_square_meter_of_glass_price)
plot_distribution(glass_prices$q_square_meter_of_glass_price)
plot_density(glass_prices$q_square_meter_of_glass_price)
plot_density(iron_prices$q_corrugated_iron_sheet_price_per_1)

plot_distribution(iron_prices$q_corrugated_iron_sheet_price_per_1)

ggplot(district, aes(q_district)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

district_list <- JMMI_data %>%
  group_by(q_district) %>%
  summarise(counts = n())


city_list <- JMMI_data %>%
  group_by(City) %>%
  summarise(counts = n())

plot_bar_count(city_list,"City","counts")


region <- JMMI_data %>%
  group_by(q_region) %>%
  summarise(counts = n())

plot_pie(region,"q_region")


plot_bar_count(region,"q_region","counts")
plot_bar_count(city_list,"City","counts")
plot_bar_count(district_list,"q_district","counts")


