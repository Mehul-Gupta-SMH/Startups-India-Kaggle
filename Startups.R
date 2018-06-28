library(ggplot2)
library(ggthemes)
library(ggmap)
library(plotGoogleMaps)
library(dplyr)
library(plotly)
library(stringr)

startups <- read.csv('startup_funding.csv' , stringsAsFactors = FALSE)

pl <- ggplot(startups , aes(x = InvestmentType)) + geom_bar(aes(fill = InvestmentType) , color = "black")
print(pl + theme_economist_white() + theme(axis.text.x = element_text(angle=90, hjust=1)))

################################################
####   Data Cleaning for Investment Type   #####
################################################

correction <- function(x){
  if(x %in% c('Private Equity' , 'PrivateEquity'))
    return("Private Equity")
  if(x %in% c('Seed Funding' , 'SeedFunding'))
    return("Seed Funding")
  if(x %in% c('Crowd Funding' , 'Crowd funding'))
    return("Crowd Funding")
  else
    return("Not Mentioned")
}

startups$InvestmentType <- sapply(startups$InvestmentType , as.character)
startups$InvestmentType <- sapply(startups$InvestmentType , correction)
startups$InvestmentType <- factor(startups$InvestmentType)

pl <- ggplot(startups , aes(x = InvestmentType)) + geom_bar(aes(fill = InvestmentType) , color = "black")
print(pl + theme_economist_white() + theme(axis.text.x = element_text(angle=90, hjust=1)))


startups.non.conv.funded <- subset(startups , startups$InvestmentType %in% c('Crowd Funding' , 'Not Mentioned'))
startups <- subset(startups , startups$InvestmentType %in% c('Private Equity' , 'Seed Funding'))

################################################
#######   Data Cleaning for Remarks   ##########
################################################

startups$Remarks <- sapply(startups$Remarks , as.character)

correction1 <- function(x){
  if(x %in% c('More details' , 'More Details'))
    return('More Details')
  if(x %in% c('pre-series A' , 'pre-Series A' , 'Pre-Series A' , 'pre Series-A' , 'Pre Series-A' , 'pre series A'))
    return('Pre-Series A')
  if(x %in% c('Pre-Series A bridge round' , 'Pre-Series A Bridge round'))
    return('Pre-Series A Bridge round')
  if(x %in% c('Bridge funding' , 'Bridge Funding'))
    return('Bridge Funding')
  if(x %in% c('Bridge round' , 'Bridge Round'))
    return('Bridge Round')
  else
    return(x)
}

startups$Remarks <- sapply(startups$Remarks , correction1)

################################################
#######   Data Cleaning for Industry   #########
################################################

industry.type <- as.data.frame(table(startups$IndustryVertical))

startups$IndustryVertical <- sapply(startups$IndustryVertical , as.character)

correction2 <- function(x){
  if(x %in% c('ecommerce' , 'eCommerce' , 'Ecommerce' , 'ECommerce'))
    return('Ecommerce')
  if(x %in% c('Electric Bike Manufacturers','Electric Scooter manufacturer','Electric Scooter Manufacturer'))
    return('Electric Two-Wheelers Manufacturer')
  if(x %in% c('Financial Services Platform','Financial Services Portal'))
    return('Financial Services Platform')
  if(x %in% c('Food & Beverage','Food & Beverage'))
    return('Food and Beverages')
  if(x %in% c('Fund Raising Platform' , 'fund raising platform for startups'))
    return('Fund Raising Platform')
  if(x %in% c('hyperlocal delivery platform' , 'Hyperlocal Delivery Platform' , 'Hyperlocal Delivery Services'))
    return('HyperLlocal Delivery Platform')
  if(x %in% c('Logistics service platform','Logistics Service Provider Marketplace','Logistics Services Provider','Logistics Solution Provider'))
    return('Logistics Service Provider')
  else
    return(x)
}

startups$IndustryVertical <- sapply(startups$IndustryVertical , correction2)
startups$IndustryVertical<- factor(startups$IndustryVertical)

################################################
#######   Data Sorting for Industry   ##########
################################################
###SaaS Software as a service
###LOHAS Health and Wellness and Healthtech
###Mobile
###Ecommerce
###Lifescience
###Finance and Fintech
###Manufacturing
###Food and beverages
###Logistics and supply chain

filling.blankspaces <- function(x){
  if(x == "")
    return('Not-Mentioned')
  else
    return(x)
}

sorting <- function(x){
  x <- strsplit(x , " ")
  
  if(grepl(paste(c('HEALTH', 'HEALTHCARE' , 'FITNEES' , 'WELLNESS' , 'HEALTHTECH'), collapse="|"), x))
    return('LOHAS,Wellness,Healthtech')
  
  if(grepl(paste(c('LOGISTICS' , 'DELIVERY' , 'OPERATIONS'), collapse="|"), x))
    return('Logistics and Delivery')
  
  if(grepl(paste(c('SOFTWARE' , 'IT' , 'PLATFORM'), collapse="|"), x))
    return('SaaS')
  
  if(grepl(paste(c('BUY' , 'ECOMMERCE', 'MARKETPLACE'), collapse="|"), x))
    return('Ecommerce')
  
  if(grepl(paste(c('FOOD' ,'BEVERAGE' , 'PROBIOTIC' ), collapse="|"), x))
    return('FOOD and Beverage')
  
  if(grepl(paste(c('STREAMING' , 'SOCIAL' , 'MOBILE' , 'APP' , 'ONLINE' , 'PORTAL' , 'SEARCH' , 'WEB' , 'COMPARISON'), collapse="|"), x))
    return('Mobile and Online Services')
  
  if(grepl(paste(c('DATA' , 'AI' , 'INTELLIGENCE' , 'ANALYSIS' , 'MANAGEMENT' , 'ANALYTICS'), collapse="|"), x))
    return('Data Analytics and Management')
  
  if(grepl(paste(c('FASHION' , 'JEWELLERY'), collapse = '|') , x))
    return('Fashion and Jewellery')
  
  if(grepl(paste(c('LEARNING' , 'EDUCATION' , 'EDU-TECH' , 'EDUTECH' , 'ED-TECH' , 'E-LEARNING') , collapse = "|") , x))
    return('Education and Technology')
  
  if(grepl(paste(c('CAR' , 'TWO-WHEELER' , 'VEHICLE', 'RENTAL' , 'CAB' , 'HOTELS', 'HOTEL' , 'ACCOMODATION' , 'LUNCH' , 'RESTURANT' , 'RESTURANTS') , collapse = "|" ) , x))
    return('Cars , Hotels and Utilities')
  
  if(grepl(paste(c('CONSUMER' , 'JOB' , 'LICENSING' , 'SERVICES' , 'SOLUTIONS' ,'SERVICE' , 'INTERNET' , 'SOLUTION') , collapse = '|') , x))
    return('Service providers')
  
  if(grepl(paste(c('MAKER' , 'DEVICES' , 'MANUFACTURER' , 'INNOVATIONS' , 'INNOVATIONS' , 'ENGINEERING' , 'MATERIAL') , collapse = '|'), x))
    return('Manufacturing and Product Innovators')
  
  if(grepl(paste(c('ESTATE' , 'FINANCING' ,'FINANCE' , 'FIN-TECH' , 'FINANCIAL' , 'MARKET' , 'BUDGET') , collapse = '|') , x))
    return('Finacial services provider')
  
  else{
    x <- as.character(x)
    x <- paste(x, collapse = ' ')
    return(x)
  }
}

startups$IndustryVertical <- sapply(startups$IndustryVertical , toupper)
startups$IndustryVertical <- sapply(startups$IndustryVertical , toupper)
startups$Category <- sapply(startups$IndustryVertical , sorting)
startups$Category <- factor(startups$Category)

industry.type <- as.data.frame(table(startups$Category))
industry.type <- subset(industry.type , industry.type$Freq > 5)

###############################################
####### Seprating Conventional startups #######
###############################################

startups.conv <- subset(startups , grepl(paste(industry.type$Var1 , collapse = '|') , startups$Category))
remove(industry.type)

startups.conv$Date <- as.Date(startups.conv$Date , format('%d/%m/%Y'))

##############################################
######## Yearwise Categories scenario ########
##############################################

pl <- ggplot(data = startups.conv , aes(x = Date , y = Category)) + geom_point(aes(color = InvestmentType) , size = 2 , alpha = .4)
print(pl +theme_economist_white()  +theme(axis.text.y = element_text(size = 8)))

pl <- ggplot(data = startups.conv , aes(x = Date , y = Category)) + geom_jitter(aes(color = InvestmentType), alpha = .4 , size = 2)
pl <- pl + theme_economist_white() + theme(axis.text.y = element_text(size = 8))
print(pl )      

######### PIE CHART ##########

pl <- ggplot(data = startups.conv , aes(x = factor(1) , fill = Category)) + geom_bar(width = 1 , color = 'White')
pl <- pl + theme_few() + theme(axis.text.x = element_text(hjust=1) , axis.text.y = element_text(size = 8) , legend.key.size = unit(2,"line"))
pl <- pl + coord_polar(theta = "y" , direction = 1)
print(pl)
#################################################################################
######## Distribution of startups on investment basis in successive years #######
#################################################################################

pl <- ggplot(data = startups.conv , aes(x = Date , y = InvestmentType)) + geom_jitter(aes(color = Category) , size = 2 , alpha = .4)
print(pl +theme_economist_white()  +theme(axis.text.y = element_text(size = 8) , legend.key.size = unit(.1,"line")))

states.name <- c('Agra' , 'Ahmedabad' , 'Bangalore' , 'Belgaun' , 'Bhopal' 
                 , 'Chandigarh' , 'Chennai' , 'Coimbatore' ,
                 'Hyderabad' , 'Delhi' , 'Goa' , 'Gurgaon' , 'Hubli' ,
                 'indore' , 'jaipur' , 'jodhpur' ,'Kanpur' , 'Karur' , 
                 'Kerla' , 'Kochi' , 'Kolkata' , 'Kozhikode' , 'Lukhnow' , 
                 'Missourie' , 'Mumbai' , 'New Delhi' , 'Nagpur' , 'Noida'  
                 , 'Panaji' , 'Pune' , 'Siliguri' , 'Surat' , 'Trivandram' 
                 , 'Udaipur' , 'Uduipi' , 'Vadodara' , 'Varanasi')

states.name <- as.vector(toupper(states.name))
startups.conv$CityLocation <- sapply(startups.conv$CityLocation , toupper)

correction3 <- function(x){
  if(is.na(x))
    return('BANGALORE')
  
 x <- strsplit(x,'/')
  
 if(is.list(x))
   x <- as.vector(x[[1]])
 else
   x <- as.vector(x)
 
  for(i in 1:length(x))
    if(grepl(paste(states.name , collapse = '|') , x[i]))
      return(x[i])
    else
        return('BANGALORE')
 
 }

startups.conv$CityLocation <- sapply(startups.conv$CityLocation , as.character)
startups.conv$CityLocation <- sapply(startups.conv$CityLocation , correction3)
state.info <- as.data.frame(table(startups.conv$CityLocation))

for(i in 1:nrow(state.info))
{
  result <- geocode(as.character(state.info$Var1[i]), output = "latlona", source = "google",override_limit=TRUE)
  state.info$lon[i] <- as.numeric(result[1])
  state.info$lat[i] <- as.numeric(result[2])
  remove(result)
}

while(any(is.na(state.info$lon)))
  {
  for(i in 1:nrow(state.info))
    {
    if(is.na(state.info$lon[i]))
      {
      result <- geocode(as.character(state.info$Var1[i]), output = "latlona", source = "google",override_limit=TRUE)
      state.info$lon[i] <- as.numeric(result[1])
      state.info$lat[i] <- as.numeric(result[2])
      }
    remove(result)
    }
  }
remove(i)
indpos <- geocode('India')
indmap <- get_map(location = indpos , zoom = 5 , scale = 2)
ggmap(indmap) +  geom_point(aes(x=lon, y=lat , color = Var1), data=state.info , alpha = .4 ,size = state.info$Freq*.1) + scale_size_continuous(range=range(state.info$Freq)) + theme_few() + theme(legend.position="none")
