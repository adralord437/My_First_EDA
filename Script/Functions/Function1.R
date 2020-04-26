# Creating a Vector with Years
years_vector <- vgsales %>% 
        arrange(Year) %>% 
        unique()

## Aqui esta mi funcion que me dice cuantas ventas hubieron para Nortamerica por cada genero por cada año
tmseries <- function(x){
        for(i in unique(x)){
               c<- vgsales %>%
                        filter(Year == i) %>%
                        group_by(Genre) %>%
                        summarise(Total_Sales_NA = sum(NA_Sales))
               c <- list(c)
               print(c)
        
}}
i <- 1:2 
vgsales %>%
        arrange(Year) %>%
        filter(Year == years_vector[1,]) %>%
        select(Rank:NA_Sales)
for (j in 1:40) {
for(i in "1980":"2017"){
        a[[j]] <- vgsales %>%
                filter(Year == i) %>%
                group_by(Genre) %>%
                summarise(Total_Sales_NA = sum(NA_Sales))
        print(c)
}}
                
