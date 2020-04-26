## Loading packages
if(!require(readr))install.packages("readr")
if(!require(tidyverse))install.packages("tidyverse")

## Geting Data Set
#url <- "https://www.kaggle.com/gregorut/videogamesales/download/OExKsLPk1MffQwx39Ex4%2Fversions%2FHrmHcMK3ZvGhoCopnQhB%2Ffiles%2Fvgsales.csv?datasetVersionNumber=2"
#destfile <- "videogamesales.zip"
#download.file(url, destfile)   ## OCUPO ARREGLAR AQUI. No descarga la tabla

## Loading Data Set
vgsales <- read_csv("C:/Users/user/Desktop/LIBRARY/Practice Datasets/vgsales.csv")

## Data frame charactheristics
df_structure <- str(vgsales)
ncol(vgsales)           # Number of variables in the data set
head(vgsales, n = 10)   #First 10 rows of the data set
df_variables <- names(vgsales)          # Column names

addinfo <- "https://www.kaggle.com/gregorut/videogamesales"  ## Additional info about the dataset can be found here

## Some descriptive statistics for each variable 
summary(vgsales)

## Sales per Genre
sales_per_genre <- vgsales %>%
        group_by(Genre) %>%
        summarise(Total_Sales_NA = sum(NA_Sales),
                  Total_Sales_EU = sum(EU_Sales),
                  Total_Sales_JP = sum(JP_Sales),
                  Global_Sales = sum(Global_Sales)
                  ) 
## Top Selling Genres in North America
best.sell.genre.NA <- sales_per_genre %>%
        select(Genre,Total_Sales_NA) %>%
        arrange(desc(Total_Sales_NA))

## Visualization of Top Selling Genres in North America
(gg_topselling_genre_NA <- ggplot(best.sell.genre.NA) +
        geom_bar(mapping = aes(x = Total_Sales_NA, y = reorder(Genre, Total_Sales_NA)), stat = "identity") +
        labs(title = "Generos mas Vendidos en Norteamerica", subtitle = "1980 - 2020") +
        xlab("Ventas totales (Millones de Dolares") +
        ylab("Generp") )

ggsave("Best Selling Genres in North America .png",device = "png",width = 11.8, height = 7.20 )  ## Saving plot in png format
        
## Best selling videogames of the best selling Gender in North America
vg_action_na <- vgsales %>% filter(Genre == "Action") %>% arrange(desc(NA_Sales))
## Top Selling Genres in Europe
best.sell.genre.EU <- sales_per_genre %>%
        select(Genre,Total_Sales_EU) %>%
        arrange(desc(Total_Sales_EU))
## Visualization of Top Selling Genres in Europe
(gg_topselling_genre_EU <- ggplot(best.sell.genre.EU) +
        geom_bar(mapping = aes(x = Total_Sales_EU, y = reorder(Genre, Total_Sales_EU)), stat = "identity") +
        labs(title = "Generos mas Vendidos en Europa", subtitle = "1980 - 2020") +
        xlab("Ventas Totales en Europa (Millones de Dolares)") +
        ylab("Genero") 
)
ggsave("Best Selling Genres in Europe.png",device = "png",width = 11.8, height = 7.20 )  ## Saving plot in png format

## Top Selling Genres in JAPAN
best.sell.genre.JP <- sales_per_genre %>%
        select(Genre,Total_Sales_JP) %>%
        arrange(desc(Total_Sales_JP))

## Visualization of Top Selling Genres in Japan
(gg_topselling_genre_JP <- ggplot(best.sell.genre.JP) +
        geom_bar(mapping = aes(x = Total_Sales_JP, y = reorder(Genre, Total_Sales_JP)), stat = "identity") +
        labs(title = "Top Selling Genres in Japan", subtitle = "1980 - 2020, Millions of Dollars") +
        xlab("Total Sales Japan") +
        ylab("Genre") )

ggsave("Best Selling Genres in Japan.png",device = "png",width = 11.8, height = 7.20 )  ## Saving plot in png format

## Global Sales by Genre
(Top_selling_genre_globaly <- ggplot(sales_per_genre) +
        geom_bar(mapping = aes(x = Global_Sales, y = reorder(Genre, Global_Sales)), stat = "identity") +
        labs(title = "Top Selling Genres Globally", subtitle = "1980 - 2020, Million of Dollars") +
        xlab("Global Sales") +
        ylab("Genre"))

ggsave("Best Selling Genres Globally.png", device = "png", width = 11.8, height = 7.20) ## Saving plot in png format

## Global Sales per Region

Sales_per_region <- vgsales %>%
        select(Name:Genre, NA_Sales:Global_Sales) %>%
        mutate(
                prop_sales_NA = NA_Sales/Global_Sales,
                prop_sales_EU = EU_Sales/Global_Sales,
                prop_sales_JP = JP_Sales/Global_Sales
                )

Sales_by_region %>%
        group_by(Genre) %>%
        mutate(Total_Sales_NA = sum(NA_Sales),
                  Total_Sales_EU = sum(EU_Sales),
                  Total_Sales_JP = sum(JP_Sales),
                  Global_Sales = sum(Global_Sales)
        )



## Sales by Platform
Sales_platorm <- vgsales %>%                     ## Videogames made for ps2  had the best sales
        group_by(Platform) %>%
        summarise(Total_Sales_NA = sum(NA_Sales),
                  Total_Sales_EU = sum(EU_Sales),
                  Total_Sales_JP = sum(JP_Sales),
                  Total_Global_Sales = sum(Global_Sales)) %>%
        arrange(Global_Sales)

## Visualization of Sales by Platform
(ggplot(data = Sales_platorm) +
        geom_bar(mapping = aes(x = Total_Global_Sales, y = reorder(Platform, Total_Global_Sales)), stat = "identity") +
        labs(title = "Platforms by video game Sales", subtitle = "1980 - 2020") +
        xlab("Global Sales (Million of Dollars") +
        ylab("Platform"))

ggsave("VideoGames Sales by Platform.png", device = "png", width = 11.8, height = 7.20)   ## Saving Plot in PNG format

## Sales by Publisher
Publishers <- unique(vgsales$Publisher)  ## Publisher companies

sales_publisher <- vgsales %>% 
        group_by(Publisher) %>%
        summarise(Total_Sales_NA = sum(NA_Sales),
                  Total_Sales_EU = sum(EU_Sales),
                  Total_Sales_JP = sum(JP_Sales),
                  Total_Sales_row = sum(Other_Sales),
                  Total_Global_Sales = sum(Global_Sales)) %>%
        arrange(desc(Total_Global_Sales))

## Visualization of Sales by Pubslisher
gg_sales_publisher <- sales_publisher %>% 
        filter(Total_Global_Sales >= 200) %>%
        ggplot(mapping = aes(x = Total_Global_Sales, y = reorder(Publisher, Total_Global_Sales))) +
        geom_bar(stat = "identity") +
        labs(title = "Top 12 Publisher Companies by Global Sales", subtitle = "1980 - 2020") +
        xlab("Global Sales (Millions of Dollars)")+
        ylab("Publisher Company")

ggsave("Top 12 Publisher Companies by Global Sales.png", device = "png", width = 11.8, height = 7.20)

## Analazying if there has been any change in buying patterns for North America

#)o do so im gonna build a tibble. With Genre as columns and years as rows




