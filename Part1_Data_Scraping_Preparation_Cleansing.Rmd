---
title: "Algerian Housing Price Analysis"
output: html_notebook
---

<style type="text/css">

body{ /* Normal  */
      font-size: 10px;
  }
td {  /* Table  */
  font-size: 8px;
}
h1.title {
  font-size: 38px;
  color: DarkRed;
}
h1 { /* Header 1 */
  font-size: 28px;
  color: DarkBlue;
}
h2 { /* Header 2 */
    font-size: 22px;
  color: DarkBlue;
}
h3 { /* Header 3 */
  font-size: 18px;
  font-family: "Times New Roman", Times, serif;
  color: DarkBlue;
}
code.r{ /* Code block */
    font-size: 10px;
}
pre { /* Code block - determines code spacing between lines */
    font-size: 8px;
}
</style>


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

![](meetup_logo.jpg) 


<br><br>

####This is ouedkniss.com the top Algerian e-commerce website where usually real estate agencies and property owners list their properties  
####We will start our journey by scraping the properties listing for Algiers for one year history 
![](ouedkniss_masterlink.gif)

![](ouedkniss_master.gif)  

<br><br>

####To do this we are going to use rvest library for webscraping and puuur package for functional programming  
####For all the rest we are going to use the tidyverse packages  
####The tidyverse is an opinionated collection of R packages designed for data science.  
####All packages share an underlying design philosophy, grammar, and data structures  
  
  


![](tidyverse1.gif)

![](tidyverse2.gif)

![](tidyverse3.gif)

<br><br>  

####The information that we are looking for is hiden into the website's html, we are going to use Chrome  
####SelectorGadget extension to catch the CssSelector of the elements that we want to extract.  
![](ouedkniss_details.gif)


<br><br>  

####let's first have a look on how rvest and purrr work  
  
  
![](Rvest.gif)


<br><br>  

#### Purrrr cheat sheet from Rstudio  
![](Purrr1.gif) 

![](Purrr2.gif)

<br><br>
<br><br>
  
  
## Let's start by loading the needed packages
```{r, message=FALSE, warning=FALSE}
library(tidyverse)              #For Almost everything
library(rvest)                  #For Web Scraping
library(DT)                     #For formating tables plotting in html
library(lubridate)              #For Date and Times manipulation
library(tictoc)                 #For calculating time execution 
library(maptools)               #For reading shape files
library(ggmap)                  #Get the geolocation
```


#Starting the Data Scraping {.tabset .tabset-fade .tabset-pills}  

##scraping one page
```{r fig.width=16}
Sys.setlocale("LC_CTYPE","Arabic_Saudi Arabia") #change the locale system to get proper arabic 

# Store web url
immo_url <- ("https://www.ouedkniss.com/annonces/index.php?c=immobilier&sc=vente&sc2=appartement&wilaya=%2Calger&prix=1&prix_unite=2&p=1")

immo_url

Links <- immo_url %>% read_html(.,encoding = "UTF-8") %>%          #Read the html of the url
                               html_nodes(".button_details") %>%   #Select button_details element
                               html_attr("href")                   #Extract all hyperlinks text       

Links # print the content of Links

Links <- Links %>%
        unlist() %>%                                               #transform Links to a vector
        paste("https://www.ouedkniss.com/", ., sep = "")           #concatenate the website root address with the links
                                                                                                        

Links

# we will show  how it works for one link

Page <-  read_html(Links[1],encoding = "UTF-8") 

Page #print the content of Page



#Let's extract the Title
Title <- Page %>%                                        #Is our Html document
         html_nodes("#Title") %>%                        #We are selecting the Title element 
         html_text()                                     #Extracting the text from the Title element in the Html document
        
Title


#Getting the Description

Description <- Page %>% 
        html_nodes("#GetDescription") %>% 
        html_text()
            
Description


#Geting the Price with the details

Details <- Page %>% 
        html_nodes("#Prix,#Description p") %>% 
        html_text()
Details



#Getting the store name
Store.Name <-  html_nodes(Page, "#store_name") %>%      #Yes, it can be written also like this
        html_text()                                        
       
Store.Name

#Getting the store address
Store.Addresse<- Page %>% 
        html_nodes("#store_adresse") %>% 
        html_text()

Store.Addresse 


#Getting the pseudo for the owners

Pseudo <- Page %>% 
        html_nodes(".Pseudo") %>% 
        html_text()

Pseudo <- NA

#Now we need to group all the elements extracted in one table  
#We have different ttype of elements with different length
map_dbl(list(Links[1],Title, Description,Details,Store.Name, Pseudo ), length)
#we can use a list to group all elements but better is to use a tibble, a tibble is a new enhanced data frame structure  
#that accepts the columns list


one_link_data <- tibble(Links[1],Title, Description,Details,Store.Name, Pseudo) 
one_link_data

# Replace empty details with NA
one_link_data <-   one_link_data %>%
               mutate(Title = as.character(ifelse(Title=="character(0)", NA, Title)),
               Description = as.character(ifelse(Description=="character(0)", NA, Description)),
               Details = ifelse(Details=="character(0)", NA, Details),
               Store.Name = as.character(ifelse(Store.Name=="character(0)", NA, Store.Name)),
               Store.Addresse = as.character(ifelse(Store.Addresse=="character(0)", NA, Store.Addresse)),
               Pseudo = as.character(ifelse(Pseudo=="character(0)", NA, Pseudo)),
               # Extract details elements and values from post Description
               Det = map(Details, function(x) str_trim(str_split_fixed(string = x[], pattern = " : ", n=2)[,1])),
               Values = map(Details, function(x) str_trim(str_split_fixed(string = x[], pattern = " : ", n=2)[,2]))
               # Det and Values are list columns , we have nested data frame now
               ) %>%
        select(-Details)
one_link_data

# Get rid of nested columns by unnesting the data
one_link_data <- one_link_data %>%  unnest() 
one_link_data
# Do some cleansing
one_link_data <- one_link_data %>%
        filter(Det != "")%>%                                                            # remove white space
        mutate(Det =  case_when(                                                        # replace with correct spelling
                .$Det == "Nombre de piÃ¨ces" ~ "Nombre de pièces",
                .$Det == "SpÃ©cifications" ~ "Spécifications",
                .$Det == "Nombre d'Ã©tages / Ã©tage" ~ "Nombre d'étages / étage",
                .$Det == "DÃ©posÃ©e le" ~ "Déposée le",
                .$Det == "NumÃ©ro" ~ "Numéro",
                TRUE ~ as.character(.$Det)
                )) %>% 
        #The next three lines to remove some duplicates
        #Choose the record with the highest Values and drop the other duplicates
        group_by(Links[1], Det) %>%
        arrange(desc(Values)) %>%
        slice(1) 
one_link_data


#Tidying the data, now we will have 1 row per housing listing and all other details on columns
one_link_data <- one_link_data %>% spread(Det,value = Values) 
one_link_data
                
```


## Creating the scraper function 

```{r, eval=FALSE, message=FALSE, warning=FALSE}
scrape_houses <- function(nb.page = 1){

Sys.setlocale("LC_CTYPE","Arabic_Saudi Arabia") #change the locale system to get proper arabic text

# Store web url
immo_url <- ("https://www.ouedkniss.com/annonces/index.php?c=immobilier&sc=vente&sc2=appartement&wilaya=%2Calger&prix=1&prix_unite=2&p=") %>%
             paste (. , nb.page, sep = "")

# Scrape the post links from the page urls

#Getting all the links of the root page from the details button
#We are going to use possibly which replace any errors in the scraping with a lign of NA's, in order to avoid  
#the early stopping of the scraping due to the errors
tic("links") # Encapsuling each step bewteen tic toc commandes to calculate the execution time

Links <- map(immo_url, possibly(.%>%read_html(.,encoding = "UTF-8") %>% 
                               html_nodes(".button_details") %>% 
                               html_attr("href")
             ,NA_real_)) %>%
        unlist() %>% 
        paste("https://www.ouedkniss.com/", ., sep = "")
toc()


tic("pages")
#Getting all the pages of the links 
#In order to do it for all tha links we need to use map function 
Pages <- map(Links,                                      #First argument of the map function is our data
             possibly(                                   # We are using possibly to change the behavior of any error message
                     read_html(.,encoding = "UTF-8")  ,  #the second argument of the map function is the function that we want to apply
                      NA_real_))                         #We are going to fill our data with NA's if any error ocuur

toc()

# Getting  the post title
tic("titles")
Title <- map(Pages,possibly(. %>% 
                         html_nodes("#Title") %>% 
                         html_text()
                 ,NA_real_))
toc()

#Getting the Description
tic("description")
Description <- map(Pages,possibly(. %>% 
                         html_nodes("#GetDescription") %>% 
                         html_text()
                 ,NA_real_))
toc()

#Geting the Price with the details
tic("details")
Details <- map(Pages,possibly(. %>% 
                         html_nodes("#Prix,#Description p") %>% 
                         html_text()
                 ,NA_real_))
toc()

#Getting the store name
tic("store.name")
Store.Name<- map(Pages,possibly(. %>% 
                         html_nodes("#store_name") %>% 
                         html_text()
                 ,NA_real_)) 
toc()

#Getting the store address
tic("store.addresse")
Store.Addresse<- map(Pages,possibly(. %>% 
                         html_nodes("#store_adresse") %>% 
                         html_text()
                  ,NA_real_))
toc()

#Getting the pseudo for the owners
tic("pseudo")
Pseudo <- map(Pages,possibly(. %>% 
                         html_nodes(".Pseudo") %>% 
                         html_text()
              ,NA_real_)) 
toc() 

# adding everything to a tibble and reshaping the data 
tic("constructing tibble")        
housing_data <- tibble(Links,Title, Description,Details,Store.Name, Pseudo) %>%  
        # replace empty details with NA
        mutate(Title = as.character(ifelse(Title=="character(0)", NA, Title)),
               Description = as.character(ifelse(Description=="character(0)", NA, Description)),
               Details = ifelse(Details=="character(0)", NA, Details),
               Store.Name = as.character(ifelse(Store.Name=="character(0)", NA, Store.Name)),
               Store.Addresse = as.character(ifelse(Store.Addresse=="character(0)", NA, Store.Addresse)),
               Pseudo = as.character(ifelse(Pseudo=="character(0)", NA, Pseudo)),
               # Extract details elements and values from post Description
               Det = map(Details, function(x) str_trim(str_split_fixed(string = x[], pattern = " : ", n=2)[,1])),
               Values = map(Details, function(x) str_trim(str_split_fixed(string = x[], pattern = " : ", n=2)[,2]))
               # Det and Values are list columns , we have nested data frame now
               ) %>%
        select(-Details) %>%
        # Get rid of nested columns by unnesting the data
        unnest() %>%
        # Do some cleansing
        filter(Det != "")%>%
        mutate(Det =  case_when(
                .$Det == "Nombre de piÃ¨ces" ~ "Nombre de pièces",
                .$Det == "SpÃ©cifications" ~ "Spécifications",
                .$Det == "Nombre d'Ã©tages / Ã©tage" ~ "Nombre d'étages / étage",
                .$Det == "DÃ©posÃ©e le" ~ "Déposée le",
                .$Det == "NumÃ©ro" ~ "Numéro",
                TRUE ~ as.character(.$Det)
                )) %>% 
        # the next three lines to remove some duplicates
        group_by(Links, Det) %>%
        arrange(desc(Values)) %>%
        slice(1) %>% # choose the record with the highest Values and drop the other duplicates
        # tidying the data, now we will have 1 row per housing listing and all other details on columns
        spread(Det,value = Values) 
 toc()
 tic("return")
return(housing_data)
 toc()
}
```
#
## Scrape the housing data and save it to the disk

```{r}
#Create a gentle scraping function
Asber_Chouia <- function (periods = c(1,1.5)) {
        SleepCalls <-  runif(1, periods[1],periods[2]) # generate a uniform random value  between period 1 and period 2
        # some prinitng to seperate the execution
        cat(paste("----------------------------------------------------------------------------------------","",sep = "\n")) 
        cat(paste0(Sys.time()),"Rani Saber", round(SleepCalls,2), "Seconds\n")
        cat(paste("","", sep = "\n"))
        Sys.sleep(SleepCalls) #Cause the sytem to sleep before continue the script execution
}

#Wrap everything into a function
GentleScraping <- function(Start_Sleep=1, Finish_Sleep=1.5, Page){
        Asber_Chouia (c(Start_Sleep, Finish_Sleep))
        Algiers_SalesAppartments <- scrape_houses(Page)
        return(Algiers_SalesAppartments)
}


starttime<- Sys.time() # To count execution time
#Apply the scraping function for 100 pages (that correspond to one year listing)
data_scraped <-  vector("list",100)  #First crate an empty list of 100 element
for (i in 1:100)
{
        cat(paste("","", sep = "\n"))
        cat(paste0("Scraping Page"," ",i))
        cat(paste("","", sep = "\n"))
        # iterate through  all the pages, in each page there is 30 links to listing houses  
        # for each 30 links scraped the scraping will pause for a period of few seconds  
        # before continuing the scraping
        data_scraped[[i]]<- GentleScraping (Start_Sleep =1 ,Finish_Sleep =1.5 ,Page = i)  # iterate through    
}
endtime<- Sys.time()
endtime - starttime # Give the scraping time

Algiers_SaleAppartment <- bind_rows(data_scraped) # append all the element of the list into one big data frame
#Save the data to the disk
write.csv(Algiers_SaleAppartment, "Algiers_SalesAppartments26072018.csv", row.names = FALSE, fileEncoding = "UTF-8") 
```
# Data Preparation and cleansing
```{r echo=TRUE}
housing <- read.csv("Algiers_SalesAppartments26072018.csv", fileEncoding = "UTF-8") ### Reading the data 
names(housing) [7:15]<- c("Date", "Nb.Floor", "Nb.Room", "Nb.Views", "ID.Offer", "Price", "District", "Specifics", "Area") ### Rename some variables


#Create some new variables based on the information found on the existing ones
housing  <- housing %>%
        mutate(Price.value = as.numeric(str_split_fixed(str_trim(housing$Price), " ",3)[,1]),
               Price.unit= factor(str_split_fixed(str_trim(housing$Price), " ",3)[,2]),
               Price.desc= factor(str_split_fixed(str_trim(housing$Price), " ",3)[,3]),
               Area = as.numeric(str_split_fixed(str_trim(housing$Area), " ",2)[,1])
        ) %>%
        mutate(Price.value.dzd = # Create some rules to clean the price
                       case_when(.$Price.unit == "Milliards"  & .$Price.value <=10
                                 ~ .$Price.value *10000000,
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value <= 35 
                                 ~ .$Price.value * .$Area*10000,
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value > 35 & .$Price.value <10000 
                                 ~ .$Price.value *10000,
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value >10000 
                                 ~ .$Price.value * .$Area,
                                 .$Price.unit == "Millions"& .$Price.value <=10 
                                 ~ .$Price.value * 10000000,
                                 .$Price.unit == "Millions"& .$Price.value >100 
                                 ~ .$Price.value * 10000,
                                 .$Price.unit == "Millions"& .$Price.value >10 & .$Price.value <100 
                                 ~.$Price.value * 1000000,
                                 TRUE ~ .$Price.value
                                  ),
         # Create a variable that contain nominal rules applied to Price variable, so that we can use it in our sanity check
               Price.rules = 
                       case_when(.$Price.unit == "Milliards"  & .$Price.value <=10
                                 ~ "Price.value *10000000",
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value <= 35 
                                 ~ "Price.value * Area*10000",
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value > 35 & .$Price.value <10000 
                                 ~ "Price.value *10000",
                                 str_detect(.$Price.desc, "m²") == TRUE & .$Price.value >10000 
                                 ~ "Price.value * Area",
                                 .$Price.unit == "Millions"& .$Price.value <=10 
                                 ~ "Price.value * 10000000",
                                 .$Price.unit == "Millions"& .$Price.value >100 
                                 ~ "Price.value * 10000",
                                 .$Price.unit == "Millions"& .$Price.value >10 & .$Price.value <100 
                                 ~"Price.value * 1000000",
                                 TRUE ~ "Price.value"
                                  ),
              # Create even more variables
              Announcer.Name = ifelse(is.na(Pseudo) == FALSE,Pseudo ,as.character(Store.Name)),
              Announcer.Type = 
                      case_when(is.na(Store.Addresse) == FALSE & 
                                        str_detect(toupper(Store.Name), "AGENCE|AG") == TRUE ~ "AGENCE",
                                         is.na(Store.Addresse) == FALSE & 
                                        str_detect(toupper(Store.Name), "PROMOTION") == TRUE ~ "PROMOTEUR ",
                                         TRUE ~ "BUREAU D'AFFAIRE"),
              Announcer.Name = ifelse(is.na(Pseudo) == FALSE,Pseudo ,as.character(Store.Name)),
              Hour =  str_sub(str_trim(str_split_fixed(Date,"à", 2)[,2]),1,2),
              Date = dmy(str_trim(str_split_fixed(Date,"à", 2)[,1])),
              Month = month(Date,label = TRUE),
              Municipality=factor(str_trim(str_extract(Title,"(Alger.*)"))),
              Garage=str_detect(Specifics, "Garage"),
              Garden=str_detect(Specifics, "Jardin") ,
              Furnished=str_detect(Specifics, "Meublé"),
              Promise=str_detect(Specifics, "Promesse de vente"),
              New.Project=str_detect(Specifics, "Promotion"),
              Payment=factor(ifelse(str_detect(Specifics,"Paiement par tranches") == TRUE, "tranches", "comptant"))
              )
               
 housing

write.csv(housing, "Algiers_SalesAppartments26072018_c.csv", row.names = FALSE, fileEncoding = "UTF-8") 
```

<br><br>

## Having a look at our data after cleansing
```{r echo=TRUE, fig.width=12}
### let's start analysing our data getwd()
housing<- read.csv("Algiers_SalesAppartments26072018_c.csv") 
### Have a look at our extended Housing  dataset
housing %>% select(Date,Nb.Floor,Nb.Room,Nb.Views) %>% summary() 
housing %>% select(Area,Price.value.dzd,Municipality) %>% summary() 
housing %>% select(Garage:Payment) %>% summary() 
```

<br><br>  

## let's apply some more filters and prepare the data for geocoding  

```{r fig.width=12}
## remove abnormal data 
housing2 <- housing %>%
        filter(Price.value.dzd <100000000 & Price.value.dzd >= 4000000,Area >30 & Area <= 300,    #Applying some filters 
               str_trim(Nb.Room) %in% c("2","3","4","5")) %>%
        mutate(Nb.Room = factor(Nb.Room)) 
housing2$Nb.Room <- ordered(str_trim(housing2$Nb.Room) ,levels =1:7) #Ordering the levels of the Nb.Room variable  

# construct a function for multiple pattern replacement
mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
} 

#read the shape file for Algiers
sh<-readShapePoly("C:/Users/fateh/Documents/R Scripts/Shapefile/algeria/communes.shp")
sh<-sh[sh@data$wilaya=="ALGER",]


#Do some cleansing and preparation in order to match the names of the municipalities in ouedkniss to those 
#in our shape file
housing2$Municipality<-  toupper(str_split_fixed(string =housing2$Municipality, pattern = " ",n = 2 )[,2])
housing2$Municipality <- factor(ifelse(str_detect (string = housing2$Links,pattern = "alger-centre") == TRUE & is.na(housing2$District) == TRUE,
                                "ALGER CENTRE", as.character(housing2$Municipality))) 
#Correct the naming
housing2$Municipality<- mgsub(c("GUE DE CONSTANTINE", "BAB EZZOUAR","BACHDJERRAH","HAMMAMET", "BIRKHADEM",
                                    "BELOUIZDAD","BOLOGHINE","ALGER CENTRE", "CHEVALLEY","HRAOUA","TESSALA EL MERDJA","SAID HAMDINE","AIN NAADJA"),
      c("DJISR KSENTINA","BEB EZZOUAR","BACH DJERRAH", "BAINS ROMAINS", "BIR KHADEM", "HAMMA ANASSERS",
        "BOLOGHINE IBN ZIRI", "ALGER" ,"BOUZAREAH", "HARAOUA", "TASSALA EL MERDJA","BIR MOURAD RAIS","DJISR KSENTINA"),
     housing2$Municipality)

housing2$Municipality <- factor(ifelse(housing2$Municipality== "","ALGER", as.character(housing2$Municipality)))
```

```{r}
##Creating top municipalities variable
housing2<- housing2%>% mutate(Top_Municipalities = fct_lump(housing2$Municipality, 10)) 

housing2 %>% select(Date,Nb.Floor,Nb.Room,Nb.Views) %>% summary() 
housing2 %>% select(Area,Price.value.dzd,Municipality) %>% summary() 
housing2 %>% select(Garage:Payment) %>% summary() 

```


```{r}
data.frame(Top_Municipalities =table(housing2$Top_Municipalities))## check

unique(sort(factor(housing2$Municipality)))
unique(sort(factor(sh@data$commune0)))## check again nehi twesswiss
```

<br><br>

## Get The geolocation of the Housing Munucipalities from Google Map API

```{r}
#Get the geolocation of the 
commune.names <- paste("Algiers,", as.character(unique(housing2$Municipality)), sep=" ")
commune.names[commune.names == "Algiers, ALGER"] <- "ALGER CENTRE, Algiers"
commune.info <- geocode(commune.names, output = "more", override_limit = TRUE)

#Get the localities that was not geocoded in the dirst run
commune.names.missing <- cbind(commune.names,commune.info) %>% 
        filter(is.na(lon))  
#Second geocode pass
commune.info.missing <- geocode(as.character(commune.names.missing$commune.names), output = "more", override_limit = TRUE)


#Bind rows first and second geocoding pass
commune.info_final<-
        cbind(commune.names,commune.info) %>%
        filter(!is.na(lon)) %>%
        bind_rows(.,cbind(commune.names=commune.names.missing$commune.names,commune.info.missing)) 

#Do more cleansing
commune.info_final <-
        mutate(commune.info_final,commune.names = factor(ifelse(commune.names == "ALGER CENTRE, Algiers",
                                                                              "Algiers, ALGER", 
                                                                              as.character(commune.names))))
commune.info_final$Municipality <- trimws(str_split_fixed(commune.info_final$commune.names,",",2))[,2]

#Create an augmented data set with geolocation data
Housing_Data <- 
        housing2%>%
        left_join(., commune.info_final) %>%
        #creating Announcer name and announcer Type variable
        mutate(Announcer.Name = ifelse(is.na(Pseudo) == FALSE,as.character(Pseudo) ,as.character(Store.Name)),
               Announcer.Type = case_when(str_detect(toupper(Store.Name), "AGENCE|AG") == TRUE ~ "AGENCE",
                                          str_detect(toupper(Store.Name), "PROMOTION") == TRUE ~ "PROMOTEUR" ,
                                          str_detect(toupper(Store.Name), "BUREAU|AFFAIRE") == TRUE ~ "BUREAU D'AFFAIRE",
                                          is.na(Pseudo) == FALSE ~ "PARTICULIER",
                                          TRUE ~ "BUREAU D'AFFAIRE")
               ) 
```

```{r}
commune.info_final
Housing_Data
```



## Get the geolocation of the Store(Promoteur,Agence, Bureau d'etudes)  


```{r}
#Getting the Municipality of the Store by looking for similar address pattern between the House Municipality and the Store Address
Housing_Data$Store.Municipality <- 
        str_match(toupper(housing2$Store.Addresse),
                  pattern = paste(c(unique(as.character(housing2$Municipality))[-10],
                                  c("GUE DE CONSTANTINE", "BAB EZZOUAR","BACHDJERRAH","HAMMAMET", "BIRKHADEM", "BELOUIZDAD","BOLOGHINE",
                                    "ALGER CENTRE", "CHEVALLEY","HRAOUA","TESSALA EL MERDJA","SAID HAMDINE","AIN NAADJA")) ,
                                  collapse = "|")) [,1] 
#Matching the Store Municipality names with the Housing Municipality names
Housing_Data$Store.Municipality<- mgsub(c("GUE DE CONSTANTINE", "BAB EZZOUAR","BACHDJERRAH","HAMMAMET", "BIRKHADEM",
                                    "BELOUIZDAD","BOLOGHINE","ALGER CENTRE", "CHEVALLEY","HRAOUA","TESSALA EL MERDJA","SAID HAMDINE","AIN NAADJA"),
      c("DJISR KSENTINA","BEB EZZOUAR","BACH DJERRAH", "BAINS ROMAINS", "BIR KHADEM", "HAMMA ANASSERS",
        "BOLOGHINE IBN ZIRI", "ALGER" ,"BOUZAREAH", "HARAOUA", "TASSALA EL MERDJA","BIR MOURAD RAIS","DJISR KSENTINA"),
     Housing_Data$Store.Municipality)

#Getting the long and lat for Store Municipality
Housing_Data <-left_join(Housing_Data, select(commune.info_final,Municipality, Store.lon=lon, Store.lat=lat), by = c("Store.Municipality" = "Municipality"))
```

```{r}
glimpse(Housing_Data)
write.csv(Housing_Data, "Housing_Data26072018.csv", row.names = FALSE)
```

