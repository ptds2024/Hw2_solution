library(rvest)
library(stringr)

# https://www.anibis.ch/robots.txt does not mention /mobilier*

# web scrape 140x200 or 140x190 beds sold by individuals from anibis
# first research page
url <- "https://www.anibis.ch/fr/q/mobilier/Ak6dMaXQgMTQwqWZ1cm5pdHVyZZSRkqljb21wYW55QWSncHJpdmF0ZcDAwA?sorting=newest&page=1&query=Lit+140"
website <- read_html(url)

# get the number of pages
number_of_pages <- as.integer(website %>% html_nodes("ul li:nth-last-child(2)") %>% html_text())

# image_src <- website %>% html_nodes("img") %>% html_attr("src")
# image_src <- image_src[grepl("https://c.anibis.ch/", image_src)]
# sum(grepl("https://c.anibis.ch/account_logo", image_src))

# Save region (first two digits of postcode) and price for beds of dimension 140x200
price <- NA_integer_
region <- NA_integer_
dates <- NA_character_
int <- 0L

for(j in 1:number_of_pages){
  url <- paste0("https://www.anibis.ch/fr/q/mobilier/Ak6dMaXQgMTQwqWZ1cm5pdHVyZZSRkqljb21wYW55QWSncHJpdmF0ZcDAwA?sorting=newest&page=",j,"&query=Lit+140")
  
  # read webpage
  website <- read_html(url) 
  
  # get all infos
  infos <- website %>% html_nodes("span.MuiTypography-root") %>% html_text()
  n <- length(infos)
  infos <- infos[-c(1:21, (n-20):n)] # remove unwanted infos (first 21 entries and last 20 entries)
  n <- length(infos)
  if(n %% 3!=0) stop("Error: number of infos is not a multiple of 3")
  
  for(i in seq(2,n,by=3)){
    if(grepl("140(.|\n)+200", infos[i]) || grepl("140(.|\n)+190", infos[i])){
      # price
      if(grepl("Gratuit",infos[i+1])) next
      # update iterator
      int <- int + 1L
      price[int] <- as.integer(gsub("\\W*","",infos[i+1]))
      
      
      # region
      tmp_region <- as.integer(str_extract(infos[i-1], "\\d{4}"))
      region[int] <- gsub("\\d{2}$","",tmp_region)
      
      # date
      tmp_date <- str_extract(infos[i-1], "\\d{2}\\.\\d{2}\\.\\d{4}")
      if(!is.na(tmp_date)){
        dates[int] <- tmp_date} else {
          if(grepl("Aujourd'hui", infos[i-1])){dates[int] <- format(Sys.Date(), "%d.%m.%Y")} else {
            if(grepl("Hier", infos[i-1])){dates[int] <- format(Sys.Date()-1, "%d.%m.%Y")} else {
              dates[int] <- NA_character_
            }
          }
          
        }
    }
  }
  cat(j,"/",number_of_pages,"\n")
}

# Transform from region code to region name
url <- "https://en.wikipedia.org/wiki/Postal_codes_in_Switzerland_and_Liechtenstein"
website <- read_html(url)

tmp_region <- website %>% html_nodes("li > ul li") %>% html_text()
region_names <- gsub("\\d{2}xx\\s*-\\s*","",tmp_region)
region_code <- str_extract(tmp_region, "^\\d{2}")

region2 <- rep(NA_character_, length(region))
for(i in seq_along(region)){
  region2[i] <- region_names[region[i] == region_code]
}

# create a data frame with difference price - discount
anibis <- data.frame(region = region2, price = price, dates = as.Date(dates, format = "%d.%m.%Y"))
# plot price vs difference
library(ggplot2)
library(MASS)
fit <- rlm(price ~ region, data = anibis)
prediction <- predict(fit, data.frame(region = "Region Lausanne, Echallens (North-West)"), interval = "confidence")


ggplot(anibis, aes(y = price, x = dates, color = region)) + 
  geom_point(size = 5) +
  labs(title = paste0("Price of 140x200 or 140x190 beds sold by individuals on Anibis.ch (update ",Sys.Date(),")"),
       x = "Dates",
       y = "Price") +
  theme_minimal() + 
  geom_hline(yintercept = prediction[1], color = "blue")

# save data
save(anibis, file = "anibis.rds")
# print the data set 
anibis
