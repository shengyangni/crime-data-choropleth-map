#prepare needed packages

library(dplyr)
library(ggplot2)
library(choroplethrMaps)
library(choroplethr)
library(ggmap)
library(leaflet)
library(KernSmooth)
library(maps)
#GOOGLE API register
#register_google(key = "", write = TRUE)

#data exploration
crime<-read.csv("crime.csv")
crime$county<-substr(crime$FIPS,1,4)#first 4 number is county code
unemployment<-read.csv("unemployment.csv")

#exploration by state
state<-crime%>%filter(str_detect(`Measure.Type`,'Rate'))%>%
  filter(county==9)%>%
  filter(str_detect(`Crime.Type`,'Total',negate = TRUE))
  
ggplot(data=state, aes(x=Year, y=Value,fill=Crime.Type)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")+
  geom_text(aes(label=Value), vjust=-0.3, color="black", size=2)+
  facet_wrap(.~Crime.Type)


#best and worst town 
town10<-crime%>%filter(str_detect(`Measure.Type`,'Rate'))%>%
  filter(str_detect(`Crime.Type`,'Total Crime'))%>%
  filter(str_detect(Year,'2010'))%>%
  arrange(Value)%>%
  select(c(Town,Value,county))
head(town10)
tail(town10)

#scatter plot of safe and dangerous town
town10<-rbind(head(town10),tail(town10))
town<-town10$Town
paste_town<-function(town){
  paste(town,',CT,US',sep = '')
}
whole_loc<-lapply(town,paste_town)
lon_lat<-lapply(whole_loc,geocode)
lon_lat<-unlist(lon_lat)
lon_lat<-as.vector(lon_lat)
Matrix_ll<-matrix(lon_lat,ncol = 2,byrow = T)
town_geo<-data.frame(town,Lon=Matrix_ll[,1],Lat=Matrix_ll[,2])
town_geo$Situation<-c(rep('Good',6),rep('Bad',6))
CTmap<-qmap('Connecticut',zoom=8,color='bw',legend='topleft')
CT_scatter<-CTmap+
  geom_point(aes(x=Lon,y=Lat,colour=Situation),
             data=town_geo)+
  ggtitle("The Scatter Plot of Safe Town and Dangerous Town in 2010") +
  theme(legend.position="bottom")
CT_scatter


#total violent crime 

#2010 total violent crime rate
vio10<-crime%>%filter(str_detect(`Crime.Type`,'Total Violent Crime'))%>%
  filter(str_detect(`Measure.Type`,'Rate'))%>%
  filter(str_detect(Year,'2010'))%>%
  rename(region=county,value=Value)%>%
  select('region','value')%>%
  group_by(region)%>%
  summarise(value=mean(value))

vio10<-vio10[-1,]
vio10$region<-as.numeric(vio10$region)

county_choropleth(vio10,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Oranges")+
  labs(title = "Total Violent Crime rate in 2010 CT",
       fill="rate per 100,000")


#diff total violent crime rate
diffvio<-vio10
diffvio$value<-vio10$value-vio17$value

county_choropleth(diffvio,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Oranges")+
  labs(title = "Difference in Violent Crime rate from 2010-2017 CT",
       fill="rate per 100,000")

#unemployment rate
unemp10<-unemployment%>%filter(str_detect(year,'2010'))
unempdiff<-unemp10[,-3]
unempdiff$value<-unemp10$value-unemp17$value

county_choropleth(unemp10,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Oranges")+
  labs(title = "Unemployment rate in 2010 CT",
       fill="rate")


county_choropleth(unempdiff,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Oranges")+
  labs(title = "Difference of Unemployment rate from 2010-2017 CT",
       fill="rate")


#total property crime 
pro10<-crime%>%filter(str_detect(`Crime.Type`,'Total Property Crime'))%>%
  filter(str_detect(`Measure.Type`,'Rate'))%>%
  filter(str_detect(Year,'2010'))%>%
  rename(region=county,value=Value)%>%
  select('region','value')%>%
  group_by(region)%>%
  summarise(value=mean(value))

pro10<-pro10[-1,]
pro10$region<-as.numeric(pro10$region)

county_choropleth(pro10,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Total Property Crime rate in 2010 CT",
       fill="rate per 100,000")


#difference in total property crime rate in 2010-2017
diffpro<-pro10
diffpro$value<-pro10$value-pro17$value

county_choropleth(diffpro,state_zoom = 'connecticut',
                  num_colors = 8,
                  reference_map = T)+
  scale_fill_brewer(palette = "Blues")+
  labs(title = "Difference in Property Crime rate from 2010-2017 CT",
       fill="rate per 100,000")




