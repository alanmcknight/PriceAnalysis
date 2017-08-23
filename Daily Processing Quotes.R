setwd("//DISKSTATION/ApricotShare/Branch/ALL DAILY STATISTICS/DAILY STATS")
Data <- read.csv("stats.csv", stringsAsFactors =FALSE)

Data$UserID <- substr(Data$BTXPolref, 1, 6)
Data$BFDPbalance[is.na(Data$BFDPbalance)] <- 0
PolicySales <- subset(Data, (BTXTrantype == "New Business" | BTXTrantype == "Renewal") & (BTXPoltype == "PC" | BTXPoltype == "HQ" | BTXPoltype == "TW"))
x <- rbind(Data, PolicySales)
x <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(Data), ]
PolicySales$FinanceValue <- PolicySales$BFDPbalance * .096

###### Traffic Costs and Sources
PolicySales$TrafficCost <- 0
PolicySales$Source <- "None"
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "PC"] <- 42.50
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "TW"] <- 46.50
PolicySales$TrafficCost[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE) & PolicySales$BTXPoltype == "HQ"] <- 42.50
PolicySales$TrafficCost[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- 50
PolicySales$Source[grepl("APRQZ-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Quotezone"
PolicySales$Source[grepl("APRCC-", PolicySales$ECWebref, ignore.case=FALSE)] <- "Call Connection"
PolicySales[PolicySales$BTXTrantype == "Renewal", "TrafficCost"] <- 0

##### Assign Executive
code = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
PolicySales$BPYExec[PolicySales$BPYExec == ""] <- "0"
Executive = c("Not Assigned", "Mark", "Audrey", "Aine", "Louise", "Megan", "Elaine", "Susan", "Stephen")
execRef <- data.frame(code, Executive)
PolicySales <- merge(PolicySales, execRef, by.x="BPYExec", by.y="code", all.x=TRUE)

PolicySales$Cancellation <- "N"
PolicySales$CancellationCommission <- 0
PolicySales$AddOnValue <- 0
PolicySales$Discount <- 0

PolicySales$TotalValue <- PolicySales$BTXCommamt + PolicySales$FinanceValue - PolicySales$TrafficCost

# #####Age Calculation
# Today <- as.Date("2016/11/01")
# PolicySales$Age = round(as.numeric((Today - as.Date(PolicySales$BCMDob, "%d/%m/%Y")) / 365.25), 0)
# 
# PolicySales$Age.Range = cut(PolicySales$Age,
#                             breaks = c(-Inf, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, Inf),
#                             labels = c("17-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "70+"),
#                             right = FALSE)

#Day of Week Created Calculation
PolicySales$Day.of.Week = weekdays(as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"))
#Month Created Calculation
PolicySales$Month = months(as.Date(PolicySales$BTXDatecreated, "%d/%m/%Y"))

#Email Domain
PolicySales$Email.Domain = sub("^[^@]*", "", PolicySales$BCMEmail)

# #UK Postcode Area and Region
# setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool")
# PostcodeRegions <- read.csv("PostcodeRegions.csv")
# PolicySales$Post.Code.Prefix = gsub('[[:digit:]]+', '', substring(as.character(PolicySales$BCMPcode), 1, 2))
# PolicySales$Postcode.Area = PostcodeRegions$AREA[ match(PolicySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)]
# PolicySales$Postcode.Region = PostcodeRegions$REGION[ match(PolicySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)]

# #Quote Map Location
# Postcodes <- read.csv("ukpostcodes.csv")
# #setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Partner Analysis")
# PolicySales$BCMPcode <- toupper(PolicySales$BCMPcode)
# PolicySales$Longitude = Postcodes$longitude[ match(PolicySales$BCMPcode, Postcodes$postcode)]
# PolicySales$Latitude = Postcodes$latitude[ match(PolicySales$BCMPcode, Postcodes$postcode)]

#write.csv(PolicySales, "DailyStatsProcessed.csv")

##### COMBINE SALES REPORT

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Monthly Sales")
#a <- read.csv("AllSalesReport-2016-08.csv", stringsAsFactors =FALSE)
#b <- read.csv("AllSalesReport-2016-09.csv", stringsAsFactors =FALSE)
#c <- read.csv("AllSalesReport-2016-10.csv", stringsAsFactors =FALSE)
#d <- read.csv("AllSalesReport-2016-11.csv", stringsAsFactors =FALSE)
#e <- read.csv("AllSalesReport-2016-12.csv", stringsAsFactors =FALSE)
#f <- read.csv("AllSalesReport-2017-01.csv", stringsAsFactors =FALSE)
#g <- read.csv("AllSalesReport-2017-02.csv", stringsAsFactors =FALSE)
#h <- read.csv("AllSalesReport-2017-03.csv", stringsAsFactors =FALSE)
#i <- read.csv("AllSalesReport-2017-04.csv", stringsAsFactors =FALSE)
#j <- read.csv("AllSalesReport-2017-05.csv", stringsAsFactors =FALSE)
K <- read.csv("AllSalesReport-2017-06.csv", stringsAsFactors =FALSE)
l <- read.csv("AllSalesReport-2017-07.csv", stringsAsFactors =FALSE)
Sales <- rbind(K, l)
Sales$POSTCODE <- toupper(Sales$POSTCODE)

DailySales <- merge(PolicySales, Sales, by.x=c("BCMDob", "BCMPcode"), by.y=c("BIRTH.DATE", "POSTCODE"), all.x=TRUE)

##### COMBINE QUOTES REPORT

setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Quote Reports")
Quotes <- read.csv("Quotes08.csv", stringsAsFactors =FALSE)
#Quotes1 <- read.csv("Quotes10.csv", stringsAsFactors =FALSE)
#Quotes <- rbind(Quotes, Quotes1)
Quotes$Date.Of.Birth <- gsub("[.]", "/", Quotes$Date.Of.Birth)
DailySales <- merge(DailySales, Quotes, by.x=c("BCMDob", "BCMPcode"), by.y=c("Date.Of.Birth", "Post.Code"), all.y=TRUE)
#DailySales <- DailySales[!is.na(DailySales$BTXPolref),]
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Apricot/Daily Report Processing")
#write.csv(DailySales, "DailySales.csv", row.names=F)

bspot <- which(names(DailySales)=="Price.Position.1")

#UK Postcode Area and Region
setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool")
PostcodeRegions <- read.csv("PostcodeRegions.csv")
DailySales <- data.frame(DailySales[1:(bspot-1)], Post.Code.Prefix = gsub('[[:digit:]]+', '', substring(as.character(DailySales$BCMPcode), 1, 2)), DailySales[(bspot):ncol(DailySales)])
DailySales <- data.frame(DailySales[1:(bspot-1)], Postcode.Area = PostcodeRegions$AREA[ match(DailySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)], DailySales[(bspot):ncol(DailySales)])
DailySales <- data.frame(DailySales[1:(bspot-1)], Postcode.Region = PostcodeRegions$REGION[ match(DailySales$Post.Code.Prefix, PostcodeRegions$POSTCODE.PREFIX)], DailySales[(bspot):ncol(DailySales)])

#Quote Map Location
Postcodes <- read.csv("ukpostcodes.csv")
#setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/Partner Analysis")
DailySales$BCMPcode <- toupper(DailySales$BCMPcode)
DailySales <- data.frame(DailySales[1:(bspot-1)], Longitude = Postcodes$longitude[ match(DailySales$BCMPcode, Postcodes$postcode)], DailySales[(bspot):ncol(DailySales)])
DailySales <- data.frame(DailySales[1:(bspot-1)], Latitude = Postcodes$latitude[ match(DailySales$BCMPcode, Postcodes$postcode)], DailySales[(bspot):ncol(DailySales)])

#####Age Calculation
Today <- as.Date("2017/05/01")
DailySales <- data.frame(DailySales[1:(bspot-1)], Age = round(as.numeric((Today - as.Date(DailySales$BCMDob, "%d/%m/%Y")) / 365.25), 0), DailySales[(bspot):ncol(DailySales)])

DailySales <- data.frame(DailySales[1:(bspot-1)], Age.Range = cut(DailySales$Age,
                            breaks = c(-Inf, 21, 26, 31, 36, 41, 46, 51, 56, 61, 66, 71, Inf),
                            labels = c("17-20", "21-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "70+"),
                            right = FALSE), DailySales[(bspot):ncol(DailySales)])

#UK Residency Calculation
DailySales$Uk.Resident.Date <- gsub("[.]", "/", DailySales$Uk.Resident.Date)
DailySales <- data.frame(DailySales[1:(bspot-1)], UK.Residency.Years = round(as.numeric((Today - as.Date(DailySales$Uk.Resident.Date, "%d/%m/%Y")) / 365.25), 0), DailySales[(bspot):ncol(DailySales)])

#Licence Calculation
DailySales$How.long.have.you.held.this.licence. <- gsub("[.]", "/", DailySales$How.long.have.you.held.this.licence.)
DailySales <- data.frame(DailySales[1:(bspot-1)], Licence.Years = round(as.numeric((Today - as.Date(DailySales$How.long.have.you.held.this.licence., "%d/%m/%Y")) / 365.25), 0), DailySales[(bspot):ncol(DailySales)])

#Vehicle Value Range Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Vehicle.Value.Range = cut(DailySales$What.is.the.estimated.value.of.the.vehicle., 
                                                                            breaks = c(-Inf, 100, 500, 1000, 2000, 3000, 5000, 7000, 10000, 15000, 20000, 30000, Inf), 
                                                                            labels = c("Less than £100", "£100-£500", "£500-£1000", "£1000-£2000", "£2000-£3000", "£3000-£5000", "£5000-£7000", "£7000-£10,000", "£10,000-£15,000", "£15,000-£20,000", "£20,000-£30,000", "£30,000 +"), 
                                                                            right = FALSE), DailySales[(bspot):ncol(DailySales)])

#Day of Week Quote Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Day.of.Week.Quote = weekdays(as.Date(DailySales$Quote.Date, "%d/%m/%Y")), DailySales[(bspot):ncol(DailySales)])

#Quote Email Domain
DailySales <- data.frame(DailySales[1:(bspot-1)], Email.Domain = sub("^[^@]*", "", DailySales$Email.Address), DailySales[(bspot):ncol(DailySales)])

#DATE SOLD AND RAISED DIFFERENCE

#Who.is.the.registered.keeper.of.the.vehicle.
DailySales$Who.is.the.registered.keeper.of.the.vehicle.[grepl("^M", DailySales$Who.is.the.registered.keeper.of.the.vehicle., ignore.case=FALSE)] <- "BLANK"

#Who.is.the.registered.keeper.of.the.vehicle.
DailySales$Who.is.the.owner.of.the.vehicle.[grepl("^M", DailySales$Who.is.the.owner.of.the.vehicle., ignore.case=FALSE)] <- "BLANK"

##### Apricot Position
UploadColumnNumbers <- ncol(DailySales)
NumberofProviders <- (ncol(DailySales)-(which( colnames(DailySales)=="Price.Position.1" )-1))/3
for(j in 1:nrow(DailySales)){
  ColumnMatch <- match("Apricot Agg (OGI standalone)",DailySales[j,(which( colnames(DailySales)=="Price.Position.1"):UploadColumnNumbers)])
  if(!is.na(ColumnMatch)){
    DailySales[j,UploadColumnNumbers+1] <- DailySales[j, ColumnMatch+(which( colnames(DailySales)=="Price.Position.1" )-3)]
    DailySales[j,UploadColumnNumbers+2] <- DailySales[j, ColumnMatch+(which( colnames(DailySales)=="Price.Position.1" )-2)]
    DailySales[j,UploadColumnNumbers+3] <- (ColumnMatch)/3
  }
}
colnames(DailySales)[UploadColumnNumbers+1] <- "Selected.Provider.Price"
colnames(DailySales)[UploadColumnNumbers+2] <- "Insurer"
colnames(DailySales)[UploadColumnNumbers+3] <- "Apricot.Position"
DailySales <- DailySales[, c((-1:which( colnames(DailySales)=="Price.Position.4" )+2), (ncol(DailySales)-2):(ncol(DailySales)))]

#Premium Range Calculation
DailySales <- data.frame(DailySales[1:(bspot-1)], Price.Returned.Range = cut(as.numeric(DailySales$Selected.Provider.Price), 
                                                                             breaks = c(-Inf, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1800, 2000, 2500, 3000,Inf), 
                                                                             labels = c("Less than £100", "£100-£200", "£200-£300", "£300-£400", "£400-£500", "£500-£600", "£600-£700", "£700-£800", "£800-£900", "£900-£1,000", "£1,000-£1,100", "£1,100-£1,200", "£1,200-£1,300", "£1,300-£1,400", "£1,400-15,00", "£1,500-£1,600", "£1,600-£1,800", "£1,800-£2,000", "£2,000-£2,500", "£2,500-£3,000", "£3,000+"), 
                                                                             right = FALSE), DailySales[(bspot):ncol(DailySales)])


##### UPDATE MASTER REPORT
#setwd("//192.168.1.7/On-Site Home/alan.mcknight/Pricing Tool/SalesAnalysis")
#Master <- read.csv("ApricotSalesMaster201016.csv", stringsAsFactors =FALSE)
##### ADD DAILY SALES
Master <- DailySales

##### ADD CANCELLATIONS
CANCELLATIONS <- subset(Data, BTXTrantype == "Cancellation")
#Master$Cancellation[match(Master$BTXPolref,CANCELLATIONS$BTXPolref)] <- "Cancellation"
#Master$CancellationCommission[match(Master$BTXPolref,CANCELLATIONS$BTXPolref)] <- CANCELLATIONS$BTXCommamt

Master <- merge(Master,CANCELLATIONS, by="BTXPolref", all.x = TRUE)
Master <- within(Master, Cancellation[BTXTrantype.y == 'Cancellation'] <- 'Cancellation')
Master$BTXCommamt.y[is.na(Master$BTXCommamt.y)] <- 0
Master$BTXTrantype.y[is.na(Master$BTXTrantype.y)] <- 0
for(i in 1:nrow(Master)){
  if(Master$BTXTrantype.y[i] == 'Cancellation'){
    Master$CancellationCommission[i] <- Master$BTXCommamt.y[i]}}
Master <- Master[,1:which( colnames(Master)=="Apricot.Position")]

##### ADD ADDITIONAL PRODUCTS
ADDITIONALPRODUCTS <- subset(x, BTXTrantype != "Cancellation")
ADDON <- aggregate(as.numeric(ADDITIONALPRODUCTS$BTXCommamt), by=list(Category=ADDITIONALPRODUCTS$UserID), FUN=sum)
Master$AddOnValue <- Master$AddOnValue + ADDON$x[match(Master$UserID, ADDON$Category)]
Master$AddOnValue[is.na(Master$AddOnValue)] <- 0

##### ADD DISCOUNT
DISCOUNT <- subset(x, BTXTrantype == "Charge")
Master$Discount <- Master$Discount + DISCOUNT$BTXOrigdebt[match(Master$UserID, DISCOUNT$UserID)]
Master$Discount[is.na(Master$Discount)] <- 0

Master$TotalValue <- Master$BTXCommamt + Master$CancellationCommission + Master$FinanceValue + Master$AddOnValue - Master$TrafficCost + Master$Discount

colnames(Master) <- gsub("[.]x$", "", colnames(Master))
colnames(Master) <- gsub("[.]y$", "", colnames(Master))
Master <- Master[!is.na(Master$Quote.Reference),]

write.table(Master, "ApricotQuotesMasterAug.csv", row.names= FALSE, sep=",")
