#The following is Jorge Tenorio's exercise 1 for Springboard

#install packages
install.packages("dplyr")
install.packages("tidyr")

#load package
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

#rename dataset
toys <- tbl_df(refine_original)
toys

#----------------------------------------------------------------------------------------------------------
#TASK 1: Clean up brand names 
#observe all spelling variations in company name of dt
toys %>% select(company) %>% unique() %>% arrange(company)

#fix company names using gsub
toys[,1] <- (gsub(".*ps.*", "philips", as.matrix(toys[,1]), ignore.case = TRUE))
toys[,1] <- (gsub(".*ak.*", "akzo", as.matrix(toys[,1]), ignore.case = TRUE))
toys[,1] <- (gsub(".*ver.*", "unilever", as.matrix(toys[,1]), ignore.case = TRUE))
toys[,1] <- (gsub(".*houten.*", "van houten", as.matrix(toys[,1]), ignore.case = TRUE))

toys %>% select(company) %>% unique() %>% arrange(company)
#----------------------------------------------------------------------------------------------------------
#TASK 2: Separate product code and product number into separate columns
colnames(toys)[2] <- "Product.Info"
toys <- separate(data = toys, col = Product.Info, into = c("Product.Code", "Product.Number"), sep = "\\-")

#----------------------------------------------------------------------------------------------------------
#TASK 3: Add a column for product category (nested if/else)
toys <- toys %>% mutate(Product.Category = 
                ifelse(Product.Code == "p", "smartphone",
                ifelse(Product.Code == "v", "tv",
                ifelse(Product.Code == "x", "laptop",
                ifelse(Product.Code == "q", "tablet","NA")))))

#toys %>% View()
#----------------------------------------------------------------------------------------------------------
#TASK 4: Add full address for geocoding 
toys$full_address = paste(toys$address, toys$city, toys$country, sep = ", ")

#toys %>% View()
#----------------------------------------------------------------------------------------------------------
#TASK 5: Create dummy variables for company and product category 
toys <- toys %>% mutate(company_philips = ifelse(company == "philips",1,0))
toys <- toys %>% mutate(company_akzo = ifelse(company == "akzo",1,0))
toys <- toys %>% mutate(company_van_houten = ifelse(company == "van houten",1,0))
toys <- toys %>% mutate(company_unilever = ifelse(company == "unilever",1,0))

toys <- toys %>% mutate(product_smartphone = ifelse(Product.Code == "p",1,0))
toys <- toys %>% mutate(product_tv = ifelse(Product.Code == "v",1,0))
toys <- toys %>% mutate(product_laptop = ifelse(Product.Code == "x",1,0))
toys <- toys %>% mutate(product_tablet = ifelse(Product.Code == "q",1,0))

toys %>% View()

