getwd()


stats <- read.csv("P2-Demographic-Data (1).csv")

head(stats)
colnames(stats) <- c("Country","CountryCode","BirthRate","InternetUsers","IncomeGroup")
tail(stats)
nrow(stats)
ncol(stats)
summary(stats)

testDF <- data.frame(Code = Codes_2012_Dataset, Countries = Countries_2012_Dataset, Regions = Regions_2012_Dataset)
is.data.frame(testDF)
head(testDF)
tail(testDF)
nrow(testDF)
ncol(testDF)
summary(testDF)

updatedStats <- merge(stats,testDF, by.x = "CountryCode", by.y = "Code")
head(updatedStats)
updatedStats$Countries <- NULL
tail(updatedStats)
nrow(updatedStats)
ncol(updatedStats)
summary(updatedStats)


library(ggplot2)

qplot(data = updatedStats, x = BirthRate, y = InternetUsers, color = Regions, size = I(3), shape = I(18), alpha = 0.4, xlim = c(0,30), ylim = c(25,75))

plot1 <- ggplot(data = updatedStats, aes(x = BirthRate, y = InternetUsers, color = Regions))
plot1 + geom_point(alpha = 0.4, shape = I(18), size = I(3)) + facet_grid(Regions~., space = "free") + geom_smooth() + coord_cartesian(x = c(10,40), y = c(0,50))


plot2 <- ggplot(data = updatedStats, aes(x = BirthRate, y = InternetUsers, color = Regions))
plot2 + geom_jitter(shape = I(17)) + geom_boxplot(alpha = 0.4) + facet_grid(Regions~., space = "free")


plot3 <- ggplot(data = updatedStats, aes(x = BirthRate, fill = Regions))
themeTest <- plot3 + geom_histogram(binwidth = 5, color = "Black") + facet_grid(Regions~IncomeGroup,)

themeTest + xlab("Birth Rate") + ylab("Population") + ggtitle("Birth Rate Analysis") +
  theme(axis.title.x = element_text(color = "Red", size = 10),
        axis.title.y = element_text(color = "Blue", size = 10),
        axis.text = element_text(color = "Dark Blue", size = 10),
        plot.title = element_text(color = "Dark Green", size = 12))

  
Games[c(1,10), c(9,10)]
Games[c("KobeBryant","DwayneWade"),c("2013","2014")]

myplot <- function(data, row = 1:5){
  
  matplot(t(data[row,,drop = F]), type = "o", pch = 15:18, col = c(1:4,6),lwd = 1, lty = 1:4)
  legend("bottomright", legend = Players[row], inset = 0.01, pch = 15:18, col = c(1:4,6), lwd = 1, lty = 1:4)
}

myplot(MinutesPlayed/Games, c(1,9))


#Seasons
Seasons <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

#Players
Players <- c("KobeBryant","JoeJohnson","LeBronJames","CarmeloAnthony","DwightHoward","ChrisBosh","ChrisPaul","KevinDurant","DerrickRose","DwayneWade")

#Free Throws
KobeBryant_FT <- c(696,667,623,483,439,483,381,525,18,196)
JoeJohnson_FT <- c(261,235,316,299,220,195,158,132,159,141)
LeBronJames_FT <- c(601,489,549,594,593,503,387,403,439,375)
CarmeloAnthony_FT <- c(573,459,464,371,508,507,295,425,459,189)
DwightHoward_FT <- c(356,390,529,504,483,546,281,355,349,143)
ChrisBosh_FT <- c(474,463,472,504,470,384,229,241,223,179)
ChrisPaul_FT <- c(394,292,332,455,161,337,260,286,295,289)
KevinDurant_FT <- c(209,209,391,452,756,594,431,679,703,146)
DerrickRose_FT <- c(146,146,146,197,259,476,194,0,27,152)
DwayneWade_FT <- c(629,432,354,590,534,494,235,308,189,284)
#Matrix



#Free Throw Attempts
KobeBryant_FTA <- c(819,768,742,564,541,583,451,626,21,241)
JoeJohnson_FTA <- c(330,314,379,362,269,243,186,161,195,176)
LeBronJames_FTA <- c(814,701,771,762,773,663,502,535,585,528)
CarmeloAnthony_FTA <- c(709,568,590,468,612,605,367,512,541,237)
DwightHoward_FTA <- c(598,666,897,849,816,916,572,721,638,271)
ChrisBosh_FTA <- c(581,590,559,617,590,471,279,302,272,232)
ChrisPaul_FTA <- c(465,357,390,524,190,384,302,323,345,321)
KevinDurant_FTA <- c(256,256,448,524,840,675,501,750,805,171)
DerrickRose_FTA <- c(205,205,205,250,338,555,239,0,32,187)
DwayneWade_FTA <- c(803,535,467,771,702,652,297,425,258,370)


FreeThrows <- rbind(KobeBryant_FT,JoeJohnson_FT,LeBronJames_FT,CarmeloAnthony_FT,DwightHoward_FT,ChrisBosh_FT,ChrisPaul_FT,KevinDurant_FT,DerrickRose_FT,DwayneWade_FT)
colnames(FreeThrows) <- Seasons
colnames(FreeThrowsAttempts) <- Seasons


myplot2 <- function(data, row = 1:5){
  
  matplot(t(data[row,,drop = F]), type = "o", pch = 15:18, col = c(1:4,6), lwd = 1, lty = 1:4)  
  legend("bottomright",legend = Players[row], inset = 0.01, pch = 15:18, col = c(1:4,6), lwd = 1, lty = 1:4)
}



myplot2(FieldGoals/FieldGoalAttempts, c(1,5))



