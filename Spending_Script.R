library(tidyr)
library(dplyr)
library(shiny)
library(data.table)
library(tidyverse)
library(lubridate)
library(forcats)
### If having trouble installing packages:
# execute this in console: trace(utils:::unpackPkgZip, edit=TRUE)
# find Sys.sleep(0.5) --> change to Sys.sleep(2.5)

# set working directory
setwd("C:/Users/592798/Documents/Monthly Spending Reports/R_Banking")

# read in the data
spending <- fread("Bank_Statement_10_01_to_2_28.csv", data.table=FALSE)

purchases = list(
  gym = c('GOLDS GYM'),
  store = c('CVS/PHARMACY', 'RITE AID STORE', "UPPY'S", 'WAWA','CHEVRON/QUICKWAY MART','HUDSON NEWS','TOM THUMB STORE','TOMMYS 27',
            'TROLLEY MARKET','QT 885 0800', 'SHELL SERVICE STATION','BP#9708157CARY ST','Z MARKET','UNIVERSAL WINE & L', 'HUDSON',
            'VA ABC STORE','UNION WINE AND LIQ','7-ELEVEN','COLUMBIA PIKE CI', 'FAMILY DOLLAR'),
  household_items = c("LOWE'S",'BEDBATH&BEYOND'),
  shipping_online_purchase = c('AMZN DIGITAL','USPS.COM','Amazon.com*MB7H24C'),
  bar = c('CRYSTAL CITY SPORT', 'EL REY', 'FOX BROS BAR','LUCKY BAR',"ORMSBY'S",'SINE IRISH PUB','SOUTHERN RAILWAY T','ROCK AND ROLL',
          'THE QUEEN VIC','THE FAINTING GOAT','ZEN BISTRO','BAJA BEAN','BARCODE','BASEMENT BAR','DON`T LOOK BACK','Flash','FLORA',
          'HUDSON ST1468','LUCKY STRIKE WASHI','Mission','PEARL RAW BAR','POSTBELLUM','SARAHS PLACE','Sticky Rice','WHISKEY BUSINESS',
          'YEOLDEBULLANDBUSH','LOCAL 16','Wet Dog Tavern','THE BRIGHTON','DC SANTA CRAWL','PIK NIK','Mockingbird Hill','CITY TAP HOUSE',
          'ABIGAIL','CAFE CITRON','JEFFERSON TJS REST','F.W. SULLIVANS','WHITLOWS','CITY TAP','MARRIOTT RICHMOND','THUNDER GRILL',
          'THE BROADBERRY','ALL SOULS','El Centro'),
  restaurant = c("BEAUVINE BURGER", "DESPERADOS BURGER", "BASIC BURGER", "BURGER KING", "BURGER BACH",'SAVI PHARR', 'STARBUCKS', 'AUNTIE ANNES', 'BREZZA CUCINA','BUCKHEAD IRISH PUB','CHIPOTLE','KOGIYA RESTAURANT',
                 "MCDONALD'S",'PANDA EXPRESS', "PAPA JOHN'S", 'POTBELLY','RICHMOND AIRPORT','DC DONER','DC FOOD TRUCK',
                 'NASSTA CORP','PARS KABOB',"TGI FRIDAY'S",'AN UNCOMMON CAFE','BESTOLLI PIZZA','BONEFISH GRILL','Dominos',
                 'CARLYLE','CHICK-FIL-A','JAMBA JUICE','JOHNNY PISTOLAS', 'DOSI ROCK','MARY ANGELAS PIZZE','MUKUNDRAI INC',
                 'NY DELI','PANERA BREAD','POPEYES',"ALEXA'S FRIED","ALEXA'S FRIED",'IRON GRILL','SWEETBAKE',
                 'TASTY KABOB','DCTAPASTRU','SUGAR & TWINE','LOCAL EATERY','WOODSHED SMOKE',"WENDY'S",'JIMMY JOHNS','KIMBERLY SAVIL',
                 'SHAKE SHACK','SIDEWALK CAFE','CHANELLOS PIZZA','LAMPLIGHTER','SUBWAY','JEFFERSON LEMAIRE','WENDYS','LEVELUP*CAVA',
                 'CAFE TWELVE','SMALLCAKES CUPCAKE','UPPER SHIRLEY VINE',"KELLY'S CAJUN",'SAISON','TACO BELL','TEMPLE','KFC',
                 'CRACKER BARREL','SMOOTHIE KING','TNR CAFE','TARRANTS','TOPPERS PIZZA','CITY DOGS','CAVA'),
  books = c('BARNESNOBLE','HBOOKSELLER'),
  clothes_shoes = c('GAP US','TARGET','NORDSTROM','MACYS','T.J. MAXX'),
  donations = c('HUMAN RIGHTS'),
  electronics_Movies = c('BEST BUY','WAL-MART','NINTENDO','GOOGLE'),
  insurance = c('STATE FARM INSURAN'),
  fun_activities = c('AGECROFT HALL','DEGGELLER ATTRACTI','STATE FAI','MOVIELAND',
                     'REGAL POTOMAC YARD','RUSSELL FOODS','STONE MOUNTAIN','MOONRISE FESTI',
                     'STOCKYARD SPORTS','WM SUPERCENTER','DC BRUNCH','REGAL CINEMAS','POE FOUNDATION',
                     'WINTERGREEN','DAYS INN'),
  gifts_holidays = c('PREPAID','MOOSE APPLE CHRIST','Amzn.com/bill',
                     'ACACIA MID-TOWN','GAMESTOP #4101 1100 S HAY 12-23-18','VZW WEBPAY VZ WIRELESS'),
  groceries = c('HARRIS TEETER', 'KROGER'),
  haircut = c('GREAT CLIPS'),
  laundry = c('COINMACH RIVER'),
  media = c('ITUNES','Hulu','Prime Video','Spotify', 'Amazon Prime'),
  parking = c('PARKMOBILE','RIVER HOUSE VALET'),
  phone_plan = c('VERIZON WIRELESS'),
  rent_utilities = c('PROPERTY PAYMENT'),
  salary_reimbursement = c('DIRECT DEP BOOZ ALLEN HAMIL','MOBILE DEPOSIT'),
  snack = c('CMSVEND','EAST POTOMAC GOLF','PENTAGON CENTER TR','BLCKT VE'),
  student_loans = c('STUDNTLOAN'),
  transfer_from_savings = c('FROM SAVINGS'),
  transfer_to_savings = c('TO SAVINGS'),
  transit = c('ERM','METRO'),
  transportation = c('AMTRAK','GREYHOUND','MEGABUS'),
  travel = c('SPIRIT AIRL','DELTA AIR','MARC GARAGE & BWI','PEN AND PROSE - BWI'),
  ridesharing = c('UBER','LYFT'),
  venmo_paid = c('CASHOUT VENMO'),
  venmo_payment = c('PAYMENT VENMO'),
  withdrawal = c('ATM')
)

### create new column Purchase
spending$Purchase = ""

for(key in purchases){
  for(value in key){
    spending$Purchase <- ifelse(grepl(value, spending$Description), value, spending$Purchase)
  }
}

## Create list of nulls for adding values as new purchases arise
# inspect null dataframe and include new keywords
spending_null <- spending %>% filter(spending$Purchase == "")


### create new column Category
spending$Category = ""

for(i in 1:NROW(purchases)){
  for(value in purchases[i]){
    for(n in value){
      spending$Category <- ifelse(spending$Purchase %in% n, names(purchases)[i], spending$Category)
    }
  }
}

### create new column Type
types = list(
  day_to_day = c('restaurant','store','ridesharing','bar','groceries',
                 'transit','extra','transportation','laundry','snack',
                 'withdrawal','haircut','books','shipping_online_purchase',
                 'parking'),
  monthly_bills = c('media','phone_plan','insurance',
                    'student_loans','rent_utilities','gym'),
  ad_hoc_purchases = c('clothes_shoes','travel','electronics_Movies',
                       'gifts_holidays','household_items','donations'),
  income = c('salary_reimbursement'),
  savings_transfers = c('transfer_from_savings','transfer_to_savings'),
  venmo = c('venmo_payment','venmo_paid')
)

spending$Type = ""

for(i in 1:NROW(types)){
  for(value in types[i]){
    for(n in value){
      spending$Type <- ifelse(spending$Category %in% n, names(types)[i], spending$Type)
    }
  }
}

### Create new column City
city = list(
  c("WASHINGTON DC", "Arlington VA", "RICHMOND VA", "ARLINGTON VA", "WINTERGREEN VA", "WAYNESBORO VA",  "HALETHORPE MD", 
    "ALEXANDRIA VA", "Washington DC", "RUTHER GLEN VA", "FOREST HEIGHT MD", "Baltimore MD", "Richmond VA", "FALLS CHURCH VA",
    "San Francisco CA", "EAST RUTHERFO NJ", "GLEN ALLEN VA", "BOWIE MD", "HERNDON VA", "Alexandria VA", "Fairfax VA",
    "ANNANDALE VA", "ATLANTA GA", "WARRENTON VA", "BALTIMORE MD", "IRVING TX", "FT WORTH TX", "BENBROOK TX", "FT. WORTH TX",
    "DFW AIRPORT TX", "LINTHICUM HEI MD", "NEWARK DE", "GREENVILLE DE", "BERRYVILLE VA", "CHANTILLY VA", "CHARLES CITY VA")
)

spending$City = ""

for(key in city){
  for(value in key){
    spending$City <- ifelse(grepl(value, spending$Description), value, spending$City)
  }
}

### Create new column State
state = list(
  c(" DC ", " VA ", " MD ", " CA ", " NJ ", " GA "," TX "," DE ")
)

spending$State = ""

for(key in state){
  for(value in key){
    spending$State <- ifelse(grepl(value, spending$Description), value, spending$State)
  }
}

#spending_state_null <- spending %>% filter(spending$State == "")

## format Amount as number
spending$Amount <- gsub("\\(", "-",spending$Amount)
spending$Amount <- gsub("\\)", "",spending$Amount)
spending$Amount <- gsub("\\$", "",spending$Amount)
spending$Amount <- gsub(",", "",spending$Amount)
spending$Amount <- as.numeric(spending$Amount)

## format Purchase, Category and Type
spending$Purchase <- toupper(gsub('_', ' ', spending$Purchase))
spending$Category <- toupper(gsub('_', ' ', spending$Category))
spending$Type <- toupper(gsub('_', ' ', spending$Type))

### DATE

# Create a function that gets the date from the Description (Date variable is wrong)
# Date formats: 00-00-00 and 00-00 (MM/DD(/YY))

# Create Short_Date
for(j in 1:NROW(spending)){
  spending$Short_Date[j] = ""
  for(i in 1:nchar(spending$Description[j])){
    chunk = substr(spending$Description[j], i, i+6)
    if(substr(chunk, 1, 1) == " " & substr(chunk, 4, 4) == "-" & substr(chunk, 7,7) == " "){
      spending$Short_Date[j] <- chunk
    }
  }
}

#Create Long_Date
for(j in 1:NROW(spending)){
  spending$Long_Date[j] = ""
  for(i in 1:nchar(spending$Description[j])){
    chunk = substr(spending$Description[j], i, i+9)
    if(substr(chunk, 1, 1) == " " & substr(chunk, 4, 4) == "-" & substr(chunk, 10,10) == " "){
      spending$Long_Date[j] <- chunk
    }
  }
}

# format original date to have leading 0s
spending$Date <- format(as.Date(spending$Date, "%m/%d/%Y"), "%m/%d/%Y")

# Create column New_Date - NVL logic: Long_Date > Short_Date > Date
for(i in 1:NROW(spending)){
  if(spending$Long_Date[i]!=""){
    spending$New_Date[i] <- spending$Long_Date[i]
    } else if(spending$Short_Date[i]!=""){
      spending$New_Date[i] <- spending$Short_Date[i]
      } else {
        spending$New_Date[i] <- spending$Date[i]
      }
}

# drop unnecessary columns
keeps <- c("Description", "Amount", "New_Date", "Date", "Purchase", "Category", "Type", "City", "State")
spending <- spending[keeps]

# formatting New_Date
spending$New_Date <- gsub("-", "/", spending$New_Date)
spending$New_Date <- gsub(" ", "", spending$New_Date)

# create a function that gets the last n characters of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# if New_Date doesn't have a year, take year from original Date
for(i in 1:NROW(spending)){
  if(nchar(spending$New_Date[i])==5){
    spending$New_Date[i] <- paste0(spending$New_Date[i], substrRight(spending$Date[i], 5))
  }
}

# if New_Date year is 2 characters, insert "20" into year
# this doesn't work because the dates from original Date didn't have leading 0s
for(i in 1:NROW(spending)){
  if(nchar(spending$New_Date[i])==8){
    spending$New_Date[i] <- paste0(substr(spending$New_Date[i], 1, 6), "20", substr(spending$New_Date[i], 7, 8))
  }
}

# Issue: when year is extracted from original date when no included in the description-extracted date, the year might be wrong at
# the beginning of the year 
# correct incorrect years

spending$New_Date <- as.Date(spending$New_Date, "%m/%d/%Y")
spending$Date <- as.Date(spending$Date, "%m/%d/%Y")

for(i in 1:NROW(spending)){
  if(month(spending$New_Date[i])==12 & month(spending$Date[i])==01 & year(spending$New_Date[i])==year(spending$Date[i])){
    spending$New_Date[i] <- spending$New_Date[i]-365
  }
}

# drop unnecessary columns
keeps <- c("Description", "Amount", "New_Date", "Purchase", "Category", "Type", "City", "State")
spending <- spending[keeps]


### Fix specific purchases to align them with the correct date/type/etc if incorrect (be specific in comments)

# 1: Fix date of first month at Riverhouse (month should be August, was actually paid 7/31/2018)
for(i in 1:NROW(spending)){
  if(spending$Category[i]=="RENT UTILITIES" & spending$New_Date[i]=="2018-07-31"){
    spending$New_Date[i] <- "2018-08-01"
  }
}

# 2: There are 2 phone plan payments in October and none in September. Change the date of the 2018-10-01 payment to 09-30
for(i in 1:NROW(spending)){
  if(spending$Category[i]=="PHONE PLAN" & spending$New_Date[i]=="2018-10-01"){
    spending$New_Date[i] <- "2018-09-30"
  }
}
# 
# # 3: I cover the family phone plan, and am venmoed for everyone elses share. Therefore I actually end up paying $90
# for(i in 1:NROW(spending)){
#   if(spending$Category[i]=="PHONE PLAN"){
#     spending$Amount[i] <- -90
#   }
# }

# To make these issues fix themselves in the future, write a function that checks to see if each month has multiple of
# same monthly bills charged to the same month, and if true, move the later bill to the next month

# Create column Weekday
spending$Weekday <- weekdays(spending$New_Date)

#Create column Month
spending$Month <- months(spending$New_Date)
spending$Month <- recode(spending$Month,
                         August="01_August",
                         September="02_September",
                         October="03_October",
                         November="04_November",
                         December="05_December",
                         January="06_January",
                         February="07_February",
                         March="08_March",
                         April="09_April",
                         May="10_May",
                         June="11_June",
                         July="12_July")

# Remove rows containing July spending
spending <- spending%>%
  filter(New_Date > "2018-07-31")

# Pivot: Monthly Bills
Monthly_Bills <- spending %>%
  filter(Type=="MONTHLY BILLS")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

# Pivot: Monthly Bills Total
Total_Monthly_Bills <- spending %>%
  filter(Type=="MONTHLY BILLS")%>%
  group_by(Month, Type)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

# Combine Monthly Bills and Monthly Bills Total
Monthly_Bills_Table <- rbindlist(list(Monthly_Bills, Total_Monthly_Bills))

# Add average column and sort on it
##############################################################################
Monthly_Bills_Table$Total <- 0
Monthly_Bills_Table$Average <- 0

for(i in 1:nrow(Monthly_Bills_Table)){
  Monthly_Bills_Table$Total[i] <- sum(Monthly_Bills_Table[i,2:(ncol(Monthly_Bills_Table)-2)])
}

for(i in 1:nrow(Monthly_Bills_Table)){
  Monthly_Bills_Table$Average[i] <- Monthly_Bills_Table$Total[i] / (ncol(Monthly_Bills_Table)-3)
}

Monthly_Bills_Table <- setorderv(Monthly_Bills_Table, "Average", order=1L)
##################################################################################

#### Create table for Day-To-Day purchases

# Pivot: Day-to-Day Spending
Daily_Spending <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

# Pivot: Day-to-Day Spending
Total_Daily_Spending <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Type)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

Day_To_Day_Spending_Table <- rbindlist(list(Daily_Spending, Total_Daily_Spending))

Day_To_Day_Spending_Table$Total <- 0
Day_To_Day_Spending_Table$Average <- 0

for(i in 1:nrow(Day_To_Day_Spending_Table)){
  Day_To_Day_Spending_Table$Total[i] <- sum(Day_To_Day_Spending_Table[i,2:(ncol(Day_To_Day_Spending_Table)-2)])
}

for(i in 1:nrow(Day_To_Day_Spending_Table)){
  Day_To_Day_Spending_Table$Average[i] <- Day_To_Day_Spending_Table$Total[i] / (ncol(Day_To_Day_Spending_Table)-3)
}

Day_To_Day_Spending_Table <- setorderv(Day_To_Day_Spending_Table, "Average", order=1L)


#### Create table for Provisional purchases

# Pivot: Provisional Spending
Provisional_Spending <- spending %>%
  filter(Type=="AD HOC PURCHASES")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

# Pivot: Provisional Spending
Total_Provisional_Spending <- spending %>%
  filter(Type=="AD HOC PURCHASES")%>%
  group_by(Month, Type)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

Provisional_Spending_Table <- rbindlist(list(Provisional_Spending, Total_Provisional_Spending))

Provisional_Spending_Table$Total <- 0
Provisional_Spending_Table$Average <- 0

for(i in 1:nrow(Provisional_Spending_Table)){
  Provisional_Spending_Table$Total[i] <- sum(Provisional_Spending_Table[i,2:(ncol(Provisional_Spending_Table)-2)])
}

for(i in 1:nrow(Provisional_Spending_Table)){
  Provisional_Spending_Table$Average[i] <- Provisional_Spending_Table$Total[i] / (ncol(Provisional_Spending_Table)-3)
}

Provisional_Spending_Table <- setorderv(Provisional_Spending_Table, "Average", order=1L)


#### Create venmo table

# Pivot: Venmo Transactions
Venmo_Transactions <- spending %>%
  filter(Type=="VENMO")%>%
  group_by(Month, Category)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

# Pivot: Provisional Spending
Total_Venmo_Transactions <- spending %>%
  filter(Type=="VENMO")%>%
  group_by(Month, Type)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)

Venmo_Table <- rbindlist(list(Venmo_Transactions, Total_Venmo_Transactions))

#### Create total spending table

Total_Spending <- spending %>%
  filter(Type!="SAVINGS TRANSFERS" & Type!="INCOME")%>%
  group_by(Month)%>%
  summarise(Total=sum(Amount))%>%
  spread(Month, Total, fill=0)

Total_Spending_Table <- rbindlist(list(Total_Provisional_Spending, Total_Daily_Spending, Total_Monthly_Bills, Total_Venmo_Transactions, Total_Spending), fill = TRUE)

####### Total Income

Total_Income <- spending %>%
  filter(Type=="INCOME")%>%
  group_by(Month, Type)%>%
  summarise(Total=sum(Amount))%>%
  spread(Month, Total, fill=0)
  
####### Total Net Spending

Total_Net_Spending <- spending %>%
  filter(Type!="SAVINGS TRANSFERS")%>%
  group_by(Month)%>%
  summarise(Total=sum(Amount))%>%
  spread(Month, Total, fill=0)

Total_Net_Spending_Table <- rbindlist(list(Total_Income, Total_Monthly_Bills,  Total_Daily_Spending, Total_Provisional_Spending, Total_Venmo_Transactions, Total_Net_Spending), fill = TRUE)

Total_Net_Spending_Table$Total <- 0
Total_Net_Spending_Table$Average <- 0

for(i in 1:nrow(Total_Net_Spending_Table)){
  Total_Net_Spending_Table$Total[i] <- sum(Total_Net_Spending_Table[i,2:(ncol(Total_Net_Spending_Table)-2)])
}

for(i in 1:nrow(Total_Net_Spending_Table)){
  Total_Net_Spending_Table$Average[i] <- Total_Net_Spending_Table$Total[i] / (ncol(Total_Net_Spending_Table)-3)
}

##############################################################################################################


### Create a pivot of Day-to-Day Purchase counts 

# Pivot: 20 Most Frequent Purchases this month (ordered by the most recent month)
Monthly_Purchase_Count <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Purchase)%>%
  summarise(Total = n())%>%
  spread(Month, Total, fill=0)%>%
  as.data.frame()

Monthly_Purchase_Count$Total <- 0
Monthly_Purchase_Count$Average <- 0

for(i in 1:nrow(Monthly_Purchase_Count)){
  Monthly_Purchase_Count$Total[i] <- sum(Monthly_Purchase_Count[i,2:(ncol(Monthly_Purchase_Count)-2)])
}

for(i in 1:nrow(Monthly_Purchase_Count)){
  Monthly_Purchase_Count$Average[i] <- Monthly_Purchase_Count$Total[i] / (ncol(Monthly_Purchase_Count)-3)
}

Monthly_Purchase_Count <- setorderv(Monthly_Purchase_Count, "Average", order=-1L)

Top_20_Purchases_Count <- Monthly_Purchase_Count[1:20,]


# Pivot: 20 Most Frequent Purchases this month (ordered by the most recent month)
Monthly_Purchase_Amount <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Purchase)%>%
  summarise(Total = sum(Amount))%>%
  spread(Month, Total, fill=0)%>%
  as.data.frame()

Monthly_Purchase_Amount$Total <- 0
Monthly_Purchase_Amount$Average <- 0

for(i in 1:nrow(Monthly_Purchase_Amount)){
  Monthly_Purchase_Amount$Total[i] <- sum(Monthly_Purchase_Amount[i,2:(ncol(Monthly_Purchase_Amount)-2)])
}

for(i in 1:nrow(Monthly_Purchase_Amount)){
  Monthly_Purchase_Amount$Average[i] <- Monthly_Purchase_Amount$Total[i] / (ncol(Monthly_Purchase_Amount)-3)
}

Monthly_Purchase_Amount <- setorderv(Monthly_Purchase_Amount, "Average", order=1L)

Top_20_Purchases_Amount <- Monthly_Purchase_Amount[1:20,]

#############################################################################################

#### Count
# Pivot: Day-to-Day Spending
Daily_Spending_Cat_Count <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Category)%>%
  summarise(Total = n())%>%
  spread(Month, Total, fill=0)

# Pivot: Day-to-Day Spending
Total_Daily_Spending_Cat_Count <- spending %>%
  filter(Type=="DAY TO DAY")%>%
  group_by(Month, Type)%>%
  summarise(Total = n())%>%
  spread(Month, Total, fill=0)

Day_To_Day_Spending_Count_Table <- rbindlist(list(Daily_Spending_Cat_Count, Total_Daily_Spending_Cat_Count))

Day_To_Day_Spending_Count_Table$Total <- 0
Day_To_Day_Spending_Count_Table$Average <- 0

for(i in 1:nrow(Day_To_Day_Spending_Count_Table)){
  Day_To_Day_Spending_Count_Table$Total[i] <- sum(Day_To_Day_Spending_Count_Table[i,2:(ncol(Day_To_Day_Spending_Count_Table)-2)])
}

for(i in 1:nrow(Day_To_Day_Spending_Count_Table)){
  Day_To_Day_Spending_Count_Table$Average[i] <- Day_To_Day_Spending_Count_Table$Total[i] / (ncol(Day_To_Day_Spending_Count_Table)-3)
}

Day_To_Day_Spending_Count_Table <- setorderv(Day_To_Day_Spending_Count_Table, "Average", order=-1L)


#################### TO DO:

# Remove totals from each table
# Graphics of each pivot in ggplot
## Map of where day to day money is spent
## historams of spending

# all the averages after the first i are including the average column in the calculation (which is why as.numeric is necessary)
# workaround: create a total column first, and then create a new column that divides the total column by the number of months

# FINAL TABLES
# Monthly_Bills_Table
# Day_To_Day_Spending_Table
# Provisional_Spending_Table
# Total_Spending_Table
# Total_Net_Spending_Table
# Top_20_Purchases_Amount
# Top_20_Purchases_Count
# Day_To_Day_Spending_Count_Table

