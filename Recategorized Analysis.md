``` R
rm(list=ls()) #clear environment

#load libraries
library(purrr)
library(writexl)
library(openxlsx)
library(readxl)
library(tidyverse)
library(graphics)
library(ggthemes)
library(outliers)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggforce)
library(ggrepel)
library(hexbin)
library(latex2exp)
library(rmarkdown)
```
# SECTOR RE-CAT 
## 1902
``` R
nameG <- c("Comestibles, boissons, tabacs", "Animaux (Piece)", "Matières animales, engrais et dechets", "Cuirs et peaux, cuir, ouvrages en acuir, chaussures","Semences; plantes; produits végétaux servant à l'alimentation du bétail et déchets végétaux","Bois","Papier et produits des arts graphiques","Matières textiles et à tresser; confection","Matières minérales","Argile et grès ; poteries","Verre","Métaux","Machines, engins mécaniques et véhicules","Horloges et montres; instruments et appareils","Drogueries, substances et produits chimiques, couleurs et produits similaires","Graisses, huiles et cires pour usages industriels; huiles minérales, huiles de goudron et huiles résineuses; savons","Articles divers.")



recatData <- function(file_path) {

  data <- read_excel(file_path)
  data$Category <- as.factor(data$Category)

  data_summary <- data %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>%
    as.data.frame()

  data_count <- data %>% dplyr::count(Category)
  data_merged <- left_join(data_summary, data_count, by= 'Category')
  levels(data_merged$Category) <- nameG
  return(data_merged[-c(17), ])

}

oTwoDF <- recatData("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1902_Recat_V3.xlsx")



head(oTwoDF)
``` 


## 1891 

nineOne <- read_excel("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1891_Recat_V2.xlsx")
nineOne$Category<-as.factor(nineOne$Category)

nineOneT <- nineOne %>%
  group_by(Category) %>%
  summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

nineOneN <- nineOne %>% dplyr::count(Category)

nineOneDF <- left_join(nineOneT, nineOneN, by= join_by(Category))

levels(nineOneDF$Category ) <- nameG





## 1887

nameG <- c("Comestibles, boissons, tabacs", "Animaux (Piece)", "Matières animales, engrais et dechets", "Cuirs et peaux, cuir, ouvrages en cuir, chaussures",
           "Semences; plantes; produits végétaux servant à l'alimentation du bétail et déchets végétaux","Bois","Papier et produits des arts graphiques","Matières textiles et à tresser; confection","Matières minérales","Argile et grès ; poteries","Verre",
           "Métaux","Machines, engins mécaniques et véhicules","Horloges et montres; instruments et appareils","Drogueries, substances et produits chimiques, couleurs et produits similaires",
           "Graisses, huiles et cires pour usages industriels; huiles minérales, huiles de goudron et huiles résineuses; savons", "Articles divers.")

eightSev <- read_excel("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1887_Recat_V2.xlsx")
eightSev$Category<-as.factor(eightSev$Category)

eightSevT <- eightSev %>%
  group_by(Category) %>%
  summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

eightSevN <- eightSev %>% dplyr::count(Category)

eightSevDF <- left_join(eightSevT, eightSevN, by= join_by(Category))

levels(eightSevDF$Category ) <- nameG

eightSevDF <- eightSevDF[-c(17), ]


## 1884
nameG <- c("Comestibles, boissons, tabacs", "Animaux (Piece)", "Matières animales, engrais et dechets", "Cuirs et peaux, cuir, ouvrages en cuir, chaussures",
           "Semences; plantes; produits végétaux servant à l'alimentation du bétail et déchets végétaux","Bois","Papier et produits des arts graphiques","Matières textiles et à tresser; confection","Matières minérales","Argile et grès ; poteries","Verre",
           "Métaux","Machines, engins mécaniques et véhicules","Horloges et montres; instruments et appareils","Drogueries, substances et produits chimiques, couleurs et produits similaires",
           "Graisses, huiles et cires pour usages industriels; huiles minérales, huiles de goudron et huiles résineuses; savons", "Articles divers.")

eightFour <- read_excel("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1884_Recat_V2.xlsx")
eightFour$Category<-as.factor(eightFour$Category)

eightFourT <- eightFour %>%
  group_by(Category) %>%
  summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

eightFourN <- eightFour %>% dplyr::count(Category)

eightFourDF <- left_join(eightFourT, eightFourN, by= join_by(Category))

levels(eightFourDF$Category ) <- nameG

eightFourDF <- eightFourDF[-c(17), ]



## 1851_accepted 

fiveOne <- read_excel("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1851_accepted_Recat_V3.xlsx")
fiveOne$Category<-as.factor(fiveOne$Category)

nameG <- c("Comestibles, boissons, tabacs", "Animaux (Piece)", "Matières animales, engrais et dechets", "Cuirs et peaux, cuir, ouvrages en cuir, chaussures",
           "Semences; plantes; produits végétaux servant à l'alimentation du bétail et déchets végétaux","Bois","Papier et produits des arts graphiques","Matières textiles et à tresser; confection","Matières minérales","Argile et grès ; poteries","Verre",
           "Métaux","Machines, engins mécaniques et véhicules","Horloges et montres; instruments et appareils","Drogueries, substances et produits chimiques, couleurs et produits similaires",
           "Graisses, huiles et cires pour usages industriels; huiles minérales, huiles de goudron et huiles résineuses; savons", "Articles divers.")

fiveOneT <- fiveOne %>%
  group_by(Category) %>%
  summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

fiveOneN <- fiveOne %>% dplyr::count(Category)
fiveOneDF <- left_join(fiveOneT, fiveOneN, by= join_by(Category))


levels(fiveOneDF$Category) <- nameG

fiveOneDF <- fiveOneDF[-c(17), ]

## 1849 

fourNine <- read_excel("D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1849_Recat_V4.xlsx")
fourNine$Category<-as.factor(fourNine$Category)

nameG <- c("Comestibles, boissons, tabacs", "Animaux (Piece)", "Matières animales, engrais et dechets", "Cuirs et peaux, cuir, ouvrages en cuir, chaussures",
           "Semences; plantes; produits végétaux servant à l'alimentation du bétail et déchets végétaux","Bois","Papier et produits des arts graphiques","Matières textiles et à tresser; confection","Matières minérales","Argile et grès ; poteries","Verre",
           "Métaux","Machines, engins mécaniques et véhicules","Horloges et montres; instruments et appareils","Drogueries, substances et produits chimiques, couleurs et produits similaires",
           "Graisses, huiles et cires pour usages industriels; huiles minérales, huiles de goudron et huiles résineuses; savons", "Articles divers.")

#converting Batz to Franc
fourNine <- fourNine %>% 
  mutate(`General Import Tax` = Batz / 7)

fourNineT <- fourNine %>%
  group_by(Category) %>%
  summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

fourNineN <- fourNine %>% dplyr::count(Category)
fourNineDF <- left_join(fourNineT, fourNineN, by= join_by(Category))


levels(fourNineDF$Category) <- nameG

fourNineDF <- fourNineDF[-c(17), ]


#################### REAL TARIFFS ####################################
# Run this chunk BEFORE creating the master DF below if you want to account for the Swiss CPI (real tariffs), don't otherwise
{nineOne <- nineOne %>% 
    mutate(`General Import Tax` = `General Import Tax` / 1.048);
  eightSev <- eightSev %>% 
    mutate(`General Import Tax` = `General Import Tax` / .922);
  eightFour <- eightFour %>% 
    mutate(`General Import Tax` = `General Import Tax` / 1.051);
  fiveOne <- fiveOne %>% 
    mutate(`General Import Tax` = `General Import Tax` / 0.864);
  fourNine <- fourNine %>% 
    mutate(`General Import Tax` = `General Import Tax` / 0.745);
  oTwoT <- oTwo %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  oTwoN <- oTwo %>% dplyr::count(Category);
  oTwoDF <- left_join(oTwoT, oTwoN, by= join_by(Category));
  levels(oTwoDF$Category) <- nameG;
  nineOneT <- nineOne %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  nineOneN <- nineOne %>% dplyr::count(Category);
  nineOneDF <- left_join(nineOneT, nineOneN, by= join_by(Category));
  levels(nineOneDF$Category ) <- nameG;
  eightSevT <- eightSev %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  eightSevN <- eightSev %>% dplyr::count(Category);
  eightSevDF <- left_join(eightSevT, eightSevN, by= join_by(Category));
  levels(eightSevDF$Category ) <- nameG;
  eightSevDF <- eightSevDF[-c(17), ];
  eightFourT <- eightFour %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  eightFourN <- eightFour %>% dplyr::count(Category);
  eightFourDF <- left_join(eightFourT, eightFourN, by= join_by(Category));
  levels(eightFourDF$Category ) <- nameG;
  eightFourDF <- eightFourDF[-c(17), ];
  fiveOneT <- fiveOne %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  fiveOneN <- fiveOne %>% dplyr::count(Category);
  fiveOneDF <- left_join(fiveOneT, fiveOneN, by= join_by(Category));
  levels(fiveOneDF$Category) <- nameG;
  fiveOneDF <- fiveOneDF[-c(17), ];
  fourNineT <- fourNine %>%
    group_by(Category) %>%
    summarise_at(vars(`General Import Tax`), list(mean=mean, sd=sd)) %>% 
    as.data.frame();
  fourNineN <- fourNine %>% dplyr::count(Category);
  fourNineDF <- left_join(fourNineT, fourNineN, by= join_by(Category));
  levels(fourNineDF$Category) <- nameG;
  fourNineDF <- fourNineDF[-c(17), ];
}

############################################## Creating Master Data Frame (DF): 1902, 1891, 1887, 1884, 1851, and 1849 ###################################



## Master DF... needs to be pivoted
{ppp <- left_join(oTwoDF, nineOneDF, by = join_by(Category));
Tppp <- left_join(ppp, eightSevDF, by = join_by(Category));
Cppp <- left_join(Tppp, eightFourDF, by = join_by(Category));
Gppp <- left_join(Cppp, fiveOneDF, by = join_by(Category));
Qppp<- left_join(Gppp, fourNineDF, by = join_by(Category));
}

## Pivoting Master DF long...
# Pivoting Mean
{mmm = subset(Qppp, select = c(Category, mean.x, mean.y, mean.x.x, mean.y.y, mean.x.x.x, mean.y.y.y));
mmm <- mmm %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'mean.', values_to = "mean");
  
mmm$Year[mmm$Year == "x"] = "1902"; mmm$Year[mmm$Year == "y"] = "1891";
  mmm$Year[mmm$Year == "x.x"] = "1887"; mmm$Year[mmm$Year == "y.y"] = "1884"; mmm$Year[mmm$Year == "x.x.x"]="1851";
  mmm$Year[mmm$Year == "y.y.y"]="1849"
}


# Pivoting SD
{sdDF = subset(Qppp, select = c(Category, sd.x, sd.y, sd.x.x, sd.y.y, sd.x.x.x, sd.y.y.y));
sdDF <- sdDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'sd.', values_to = "sd");
  
sdDF$Year[sdDF$Year == "x"] = "1902"; sdDF$Year[sdDF$Year == "y"] = "1891"; sdDF$Year[sdDF$Year == "sd"] = "1887" ;
  sdDF$Year[sdDF$Year == "x.x"] = "1887"; sdDF$Year[sdDF$Year == "y.y"] = "1884"; sdDF$Year[sdDF$Year == "x.x.x"] = "1851";
  sdDF$Year[sdDF$Year == "y.y.y"]="1849"
}

# Pivoting n
{nDF = subset(Qppp, select = c(Category, n.x, n.y, n.x.x, n.y.y, n.x.x.x, n.y.y.y));
nDF <- nDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'n.', values_to = "n")
  
nDF$Year[nDF$Year == "x"] = "1902"; nDF$Year[nDF$Year == "y"] = "1891"; nDF$Year[nDF$Year == "n"] = "1887";
  nDF$Year[nDF$Year == "x.x"] = "1887"; nDF$Year[nDF$Year == "y.y"] = "1884"; nDF$Year[nDF$Year == "x.x.x"] = "1851";
  nDF$Year[nDF$Year == "y.y.y"] = "1849"
}


# joining long-pivoted DFs into final DF
fDFt <- left_join(mmm, sdDF, by= join_by(Category, Year))
fDF <- left_join(fDFt, nDF, by= join_by(Category, Year))
fDF[is.na(fDF)] <- 0 # Final Dataframe: "fDF"

#Write to excel file (warning: will overwrite file with same name. Make sure to change name and path) 
#write_xlsx(fDF, "D:\\Everything\\School\\RA position\\Tariff Schedules\\Analysis\\Sector Recategorization\\1902-1849 Master - Real_V2.xlsx")

###################### GRAPHS ######################


# Histogram
reCatLabs <- c("1":"16")

ggplot(fDF , aes(x=Category, y=mean, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") + #comment this out to remove SD bars.
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="Mean (Fr.)")+
  ggtitle("Mean Tariffs Recategorized (Real)")+ # edit graph title
  geom_text(data = fDF, aes(label = n), 
            position = position_dodge(width = 0.9), vjust = -0.25)


# Scatterplot
fDF$ID <- rep(1:16, each = 6)

fDF$zoom <- ifelse(fDF$x < 2, TRUE, FALSE)

ggplot(fDF, x=sd, y=mean)+
  geom_point(stat="identity", aes(color=Year, x=sd, y=mean), size=2)+
  geom_smooth(aes(color= Year, x=sd, y=mean), method = "lm", alpha = .15)+
  #theme_economist()+
  theme(legend.position= "left")+
  xlim(0,50)+
  ylim(0, 50)+
  labs(x="Standard Deviation", y="Mean (Fr.)")+
  ggtitle("Recategorized: Mean ~ Standard Deviation")+
  geom_text_repel(aes(color= Year, x=sd, y=mean),
                   label=paste0(fDF$ID,"^(",fDF$n,")"),
                   parse=TRUE,
                   label.padding = unit(0.1, "lines"), # Rectangle size around label
                   label.size = 0.02,
                   force=1,
                   force_pull = 0.1
  )+
  facet_zoom(xlim = c(0, 10),
             zoom.size = 0.75,
             ylim=c(0,10))

lm(mean ~ Category, data=fDF)

############################## Measuring Change Over Consecutive Years ##############################

fDF <- fDF %>%
  arrange(Category, Year) %>%
  ungroup()

### The % change of the number of items in each category over consecutive years: [(n_(it)-n_(it-1))/n_(it-1)]*100
n_percentage_change_df <- fDF %>%
  group_by(Category) %>%
  mutate(n_Percentage_Change = (n - lag(n)) / lag(n) * 100) %>%
  mutate(n_Percentage_Change = ifelse(Year == 1849, NA, n_Percentage_Change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup()

n_percentage_change_df <- na.omit(n_percentage_change_df) #removing all 1849 rows because they have no previous year to be compared to

# Histogram
ggplot(n_percentage_change_df , aes(x=Category, y=n_Percentage_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in 'n'")+
  ggtitle(TeX("Difference in Category(i) n-values over consecutive Years (t): $\\frac{(n_{it}-n_{it-1})}{n_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = n_percentage_change_df, aes(label = round(n_Percentage_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

### The % change of the proportions of categories (of all items in that year) over consecutive years: [(%n_(it)-%n_(it-1))/%n_(it-1)]*100
# finding totals first
prop_df <- fDF
prop_df$Year <- as.factor(fDF$Year)

prop_df <- within(prop_df, {
  year_sum <- ave(n, Year, FUN=sum)
})

# proportional change over previous year
n_proportion_change_df <- prop_df %>%
  arrange(Category, Year) %>%
  group_by(Year) %>%
  mutate(n_Proportion = n/year_sum) %>% 
  mutate(n_prop_change = (n_Proportion - lag(n_Proportion)) / lag(n_Proportion) * 100) %>%
  mutate(n_prop_change = ifelse(Year == 1849, NA, n_prop_change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup()

n_proportion_change_df <- na.omit(n_proportion_change_df) #removing all 1849 rows because they have no previous year to be compared to

# Histogram
ggplot(n_proportion_change_df, aes(x=Category, y=n_prop_change, group=Year)) +
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change")+
  ggtitle(TeX("Proportional difference in Category (i) n-values over consecutive Years (t): $\\frac{(p_{it}-p_{it-1})}{p_{it-1}}\\cdot 100,\\frac{n_{it}}{\\sum_in_{i}}= p_{it}$"))+ # edit graph title
  geom_text(data = n_proportion_change_df, aes(label = round(n_prop_change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

## The % change in mean tariffs over consecutive years: [(T_(it)-T_(it-1))/T_(it-1)]*100
# (make sure: are you using REAL or NOMINAL tariffs? Change ggplot text accordingly)
fDF <- fDF %>%
  arrange(Category, Year) %>%
  ungroup()

realMean_change_df <- fDF %>%
  group_by(Category) %>%
  mutate(realMean_Percentage_Change = (mean - lag(mean)) / lag(mean) * 100) %>%
  mutate(realMean_Percentage_Change = ifelse(Year == 1849, NA, realMean_Percentage_Change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup() %>%
  na.omit(realMean_change_df)




# Histogram
ggplot(realMean_change_df , aes(x=Category, y=realMean_Percentage_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in Nominal Mean Tariffs")+
  ggtitle(TeX("Difference in Nominal Mean Tariffs (NT) arranged by Category (i) over consecutive Years (t): $\\frac{(NT_{it}-NT_{it-1})}{NT_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = realMean_change_df, aes(label = round(realMean_Percentage_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

## The % change in Tariff SDs over consecutive years: [(SD_(it)-SD_(it-1))/SD_(it-1)]*100
# (make sure: are you using REAL or NOMINAL tariffs?)
fDF <- fDF %>%
  arrange(Category, Year) %>%
  ungroup()

realSD_change_df <- fDF %>%
  group_by(Category) %>%
  mutate(realSD_Percentage_Change = (sd - lag(sd)) / lag(sd) * 100) %>%
  mutate(realSD_Percentage_Change = ifelse(Year == 1849, NA, realSD_Percentage_Change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup()

realSD_change_df <- na.omit(realSD_change_df)

# Histogram
ggplot(realSD_change_df , aes(x=Category, y=realSD_Percentage_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in Nominal Tariff SD")+
  ggtitle(TeX("Difference in Nominal Tariff SDs (SD) arranged by Category (i) over consecutive Years (t): $\\frac{(SD_{it}-SD_{it-1})}{SD_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = realSD_change_df, aes(label = round(realSD_Percentage_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

############################## Measuring change over: 1849-1884 & 1884-1902 ##############################
# Limiting to these years because they have higher observations and mean tariffs; indicates economic protection
### The % change of the number of items in each category [(n_(it)-n_(it-1))/n_(it-1)]*100

fournine_otwo_n_percentage_change_df <- fDF %>%
  arrange(Category, Year) %>%
  group_by(Category) %>%
  mutate(row = ifelse(Year == 1851| Year == 1887| Year == 1891, NA, 1)) %>% # filtering out unwanted years
  na.omit(fournine_otwo_n_percentage_change_df) %>% #...
  select(!(row)) %>% #...
  mutate(n_Change = (n - lag(n))/ lag(n) * 100) %>% 
  mutate(n_Change = ifelse(Year == 1849, NA, n_Change))%>% #setting all 1849 rows to NA to be removed later
  na.omit(fournine_otwo_n_percentage_change_df) %>% #removing all 1849 rows because they have no previous year to be compared to
  ungroup()
  
# Histogram
ggplot(fournine_otwo_n_percentage_change_df , aes(x=Category, y=n_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in 'n'")+
  ggtitle(TeX("Difference in Category(i) n-values: 1849-1884 & 1884-1902 (t): $\\frac{(n_{it}-n_{it-1})}{n_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = fournine_otwo_n_percentage_change_df, aes(label = round(n_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

### The % change of the proportions of observations per category: [(%n_(it)-%n_(it-1))/%n_(it-1)]*100
# finding totals first
prop_df <- fDF
prop_df$Year <- as.factor(fDF$Year)

prop_df <- within(prop_df, {
  year_sum <- ave(n, Year, FUN=sum)
})


# proportional change over previous year
fournine_otwo_n_proportion_change_df <- prop_df %>%
  arrange(Category, Year) %>%
  group_by(Category) %>%
  mutate(row = ifelse(Year == 1851| Year == 1887| Year == 1891, NA, 1)) %>%
  na.omit(fournin_otwo_n_proportion_change_df) %>%
  select(!(row)) %>%
  mutate(n_Proportion = n/year_sum) %>% 
  mutate(n_prop_change = (n_Proportion - lag(n_Proportion)) / lag(n_Proportion) * 100) %>%
  mutate(n_prop_change = ifelse(Year == 1849, NA, n_prop_change)) %>% #setting all 1849 rows to NA to be removed later
  na.omit(fournin_otwo_n_proportion_change_df) %>% #removing all 1849 rows because they have no previous year to be compared to
  ungroup()

# Histogram
ggplot(fournine_otwo_n_proportion_change_df, aes(x=Category, y=n_prop_change, group=Year)) +
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change")+
  ggtitle(TeX("Proportional difference in Category (i) n-values: 1849-1884 & 1884-1902 (t): $\\frac{(p_{it}-p_{it-1})}{p_{it-1}}\\cdot 100,\\frac{n_{it}}{\\sum_in_{i}}= p_{it}$"))+ # edit graph title
  geom_text(data = fournine_otwo_n_proportion_change_df, aes(label = round(n_prop_change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)


## The % change in mean tariffs: [(T_(it)-T_(it-1))/T_(it-1)]*100
# (make sure: are you using REAL or NOMINAL tariffs? Change ggplot text accordingly)
fournine_otwo_Mean_change_df <- fDF %>%
  arrange(Category, Year) %>%
  group_by(Category) %>%
  mutate(row = ifelse(Year==1851 | Year==1887 | Year==1891, NA, 1)) %>%
  na.omit(fournine_otwo_Mean_change_df) %>%
  select(!(row)) %>%
  mutate(Mean_Percentage_Change = (mean - lag(mean)) / lag(mean) * 100) %>%
  mutate(Mean_Percentage_Change = ifelse(Year == 1849, NA, Mean_Percentage_Change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup() %>% 
  na.omit(fournine_otwo_Mean_change_df) #removing all 1849 rows because they have no previous year to be compared to


reCatLabs <- c("1":"16")
# Histogram
ggplot(fournine_otwo_Mean_change_df , aes(x=Category, y=Mean_Percentage_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in Real Mean Tariffs")+
  ggtitle(TeX("Difference in Real Mean Tariffs (RT) arranged by Category (i): 1849-1884 & 1884-1902 (t): $\\frac{(RT_{it}-RT_{it-1})}{RT_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = fournine_otwo_Mean_change_df, aes(label = round(Mean_Percentage_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)

## The % change in Tariff SDs: [(T_(it)-T_(it-1))/T_(it-1)]*100
# (make sure: are you using REAL or NOMINAL tariffs? Change ggplot text accordingly)
fournine_otwo_SD_change_df <- fDF %>%
  arrange(Category, Year) %>%
  group_by(Category) %>%
  mutate(row = ifelse(Year==1851 | Year==1887 | Year==1891, NA, 1)) %>%
  na.omit(fournine_otwo_SD_change_df) %>%
  select(!(row)) %>%
  mutate(SD_Percentage_Change = (sd - lag(sd)) / lag(sd) * 100) %>%
  mutate(SD_Percentage_Change = ifelse(Year == 1849, NA, SD_Percentage_Change)) %>% #setting all 1849 rows to NA to be removed later
  ungroup() %>% 
  na.omit(fournine_otwo_SD_change_df) #removing all 1849 rows because they have no previous year to be compared to

# Histogram
ggplot(fournine_otwo_SD_change_df , aes(x=Category, y=SD_Percentage_Change, group=Year)) + 
  geom_bar(stat="identity", aes(fill=Year), size=2, position="dodge")+
  #geom_errorbar(aes(fill= Year, ymin= ifelse(mean-sd < 0, 0, mean-sd), ymax=mean+sd), position= "dodge") +
  geom_point(size=2, position= position_dodge(width=0.9))+
  #theme_economist()+
  scale_x_discrete(labels=reCatLabs)+
  labs(x="", y="% Change in Real Tariff SDs ")+
  ggtitle(TeX("Difference in Real Tariff SDs (SD) arranged by Category (i): 1849-1884 & 1884-1902 (t): $\\frac{(SD_{it}-SD_{it-1})}{SD_{it-1}}\\cdot 100$"))+ # edit graph title
  geom_text(data = fournine_otwo_SD_change_df, aes(label = round(SD_Percentage_Change, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.25)


###########################################################################################
###################################### Outdated Code ####################################
############################################## (Run this chunk if you want to graph 1902, 1891, 1887 and 1884) ############################
# Joint Bar graph

reCatLabs <- c("1":"16")

# joint df
ppp <- left_join(oTwoDF, nineOneDF, by = join_by(Category))
Tppp <- left_join(ppp, eightSevDF, by = join_by(Category))
Qppp <- left_join(Tppp, eightFourDF, by = join_by(Category))

# Mean df
mmm = subset(Qppp, select = c(Category, mean.x, mean.y, mean.x.x, mean.y.y))
mmm <- mmm %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'mean.', values_to = "mean")

{mmm$Year[mmm$Year == "x"] = "1902"; mmm$Year[mmm$Year == "y"] = "1891"; mmm$Year[mmm$Year == "mean"] = "1887" ;
  mmm$Year[mmm$Year == "x.x"] = "1887"; mmm$Year[mmm$Year == "y.y"] = "1884"}

# Sd df
sdDF = subset(Qppp, select = c(Category, sd.x, sd.y, sd.x.x, sd.y.y))
sdDF <- sdDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'sd.', values_to = "sd")
{sdDF$Year[sdDF$Year == "x"] = "1902"; sdDF$Year[sdDF$Year == "y"] = "1891"; sdDF$Year[sdDF$Year == "sd"] = "1887" ;
  sdDF$Year[sdDF$Year == "x.x"] = "1887"; sdDF$Year[sdDF$Year == "y.y"] = "1884"}


# n df
nDF = subset(Qppp, select = c(Category, n.x, n.y, n.x.x, n.y.y))
nDF <- nDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'n.', values_to = "n")
{nDF$Year[nDF$Year == "x"] = "1902"; nDF$Year[nDF$Year == "y"] = "1891"; nDF$Year[nDF$Year == "n"] = "1887";
  nDF$Year[nDF$Year == "x.x"] = "1887"; nDF$Year[nDF$Year == "y.y"] = "1884"}


# combining pivoted_longers
fDFt <- left_join(mmm, sdDF, by= join_by(Category, Year))
fDF <- left_join(fDFt, nDF, by= join_by(Category, Year))

############################################## (Run this chunk if you want to graph 1902 vs 1891) ############################################## 

# Joint Bargraph

reCatLabs <- c("1":"16")

# joint df
ppp <- left_join(oTwoDF, nineOneDF, by= join_by(Category))

# Mean df
mmm = subset(ppp, select = c(Category, mean.x, mean.y))
mmm <- mmm %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'mean.', values_to = "mean")
mmm$Year[mmm$Year == "x"] = "1902"; mmm$Year[mmm$Year == "y"] = "1891"

# Sd df
sdDF = subset(ppp, select = c(Category, sd.x, sd.y))
sdDF <- sdDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'sd.', values_to = "sd")
sdDF$Year[sdDF$Year == "x"] = "1902"; sdDF$Year[sdDF$Year == "y"] = "1891"


# n df
nDF = subset(ppp, select = c(Category, n.x, n.y))
nDF <- nDF %>%
  pivot_longer(cols=-Category, names_to = 'Year', names_prefix = 'n.', values_to = "n")
nDF$Year[nDF$Year == "x"] = "1902"; nDF$Year[nDF$Year == "y"] = "1891"


# combining pivoted_longers
fDFt <- left_join(mmm, sdDF, by= join_by(Category, Year))
fDF <- left_join(fDFt, nDF, by= join_by(Category, Year))



