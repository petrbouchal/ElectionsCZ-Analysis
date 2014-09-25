Krajské volby 2012
========================================================


```r
kraje <- read.csv("~/github/local/ElectionsCZ/CSVKraje/Kraje_CandRegDetails2012_2.csv")
```

```
## Warning: cannot open file
## '/Users/petrbouchal/github/local/ElectionsCZ/CSVKraje/Kraje_CandRegDetails2012_2.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
library(ggplot2)
library(stringr)
library(plyr)
library(reshape)
```

```
## Attaching package: 'reshape'
```

```
## The following object(s) are masked from 'package:plyr':
## 
## rename, round_any
```


Resolve gender from names list


```r
# load data for resolving female names
firstnames_female <- read.csv("~/github/local/ElectionsCZ/OtherData/NamesFemale.csv")
```

```
## Warning: cannot open file
## '/Users/petrbouchal/github/local/ElectionsCZ/OtherData/NamesFemale.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
firstnames_female$female <- 1
```

```
## Error: object 'firstnames_female' not found
```

```r
firstnames_male <- read.csv("~/github/local/ElectionsCZ/OtherData/NamesMale.csv")
```

```
## Warning: cannot open file
## '/Users/petrbouchal/github/local/ElectionsCZ/OtherData/NamesMale.csv': No
## such file or directory
```

```
## Error: cannot open the connection
```

```r
firstnames_male$female <- 0
```

```
## Error: object 'firstnames_male' not found
```

```r

nameslist <- rbind(firstnames_male, firstnames_female)
```

```
## Error: object 'firstnames_male' not found
```

```r
nameslist <- rename(nameslist, c("Name" = "KandKrestniJmeno"))
```

```
## Error: object 'nameslist' not found
```

```r
nameslist$Rank <- NULL
```

```
## Error: object 'nameslist' not found
```

```r
nameslist$Count <- NULL
```

```
## Error: object 'nameslist' not found
```

```r

nameslist <-unique(nameslist)
```

```
## Error: object 'nameslist' not found
```

```r

kraje <- join(kraje, nameslist,type="left",match="first")
```

```
## Error: object 'nameslist' not found
```

```r

rm(firstnames_female, firstnames_male, nameslist)
```

```
## Warning: object 'firstnames_female' not found
```

```
## Warning: object 'firstnames_male' not found
```

```
## Warning: object 'nameslist' not found
```


Add real names for regions and parties, add candidate counts and relative
position


```r
ciselnik_kandidatky <- read.csv("~/github/local/ElectionsCZ/OtherData/Ciselnik_KZ_2012_kandidatky.csv")
```

```
## Warning: cannot open file
## '/Users/petrbouchal/github/local/ElectionsCZ/OtherData/Ciselnik_KZ_2012_kandidatky.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
ciselnik_kandidatky <- rename(ciselnik_kandidatky, c("KandNum" = "Kandidatka"))
```

```
## Error: object 'ciselnik_kandidatky' not found
```

```r
kraje <- join(kraje, ciselnik_kandidatky, by = "Kandidatka", match="first")
```

```
## Error: object 'kraje' not found
```

```r

ciselnik_kraje <- read.csv("~/github/local/ElectionsCZ/OtherData/Ciselnik_KZ_2012_kraje.csv")
```

```
## Warning: cannot open file
## '/Users/petrbouchal/github/local/ElectionsCZ/OtherData/Ciselnik_KZ_2012_kraje.csv':
## No such file or directory
```

```
## Error: cannot open the connection
```

```r
kraje <- join(kraje, ciselnik_kraje, by = "Kraj",match="first")
```

```
## Error: object 'kraje' not found
```

```r

kraje_pocetkand <- ddply(kraje, .(Kraj, Kandidatka), nrow)
```

```
## Error: object 'kraje' not found
```

```r
kraje_pocetkand <- rename(kraje_pocetkand, c("V1"="PocetKand"))
```

```
## Error: object 'kraje_pocetkand' not found
```

```r
kraje <- join(kraje, kraje_pocetkand, by=c("Kandidatka","Kraj"),match="first")
```

```
## Error: object 'kraje' not found
```

```r

kraje$relPoradiKand <- kraje$PoradiKand/kraje$PocetKand
```

```
## Error: object 'kraje' not found
```

```r
kraje$relPoradiKand <- 1-kraje$relPoradiKand
```

```
## Error: object 'kraje' not found
```


Create simple tool for making subsets easily


```r
select_regions$Kraj <- c(10)
```

```
## Error: object 'select_regions' not found
```

```r
select_regions$selected <- 1
```

```
## Error: object 'select_regions' not found
```

```r

select_list$Kandidatka <- c()
```

```
## Error: object 'select_list' not found
```

```r
select_list$selected <- 1
```

```
## Error: object 'select_list' not found
```

```r

kraje_seldkraj = merge(kraje,select_regions)
```

```
## Error: object 'kraje' not found
```


Calculate how women are represented on lists and what positions they get


```r
avgfemale <- ddply(kraje, .(Kraj, Kandidatka), summarise,
                   FemPerc = mean(female))
```

```
## Error: object 'kraje' not found
```

```r

kraje_fem <- subset(kraje, female == 1)
```

```
## Error: object 'kraje' not found
```

```r
avgpostfemale <- ddply(kraje_fem, .(Kraj, Kandidatka), summarise,
                      FemPos = 1-mean(PoradiKand/PocetKand))
```

```
## Error: object 'kraje_fem' not found
```

```r
women <- merge(avgfemale, avgpostfemale, c("Kandidatka", "Kraj"))
```

```
## Error: object 'avgfemale' not found
```

```r
women <- merge(women, ciselnik_kandidatky)
```

```
## Error: object 'ciselnik_kandidatky' not found
```

```r
women <- merge(women, ciselnik_kraje)
```

```
## Error: object 'ciselnik_kraje' not found
```

```r

# build plot
plot_women <- ggplot(subset(women, str_detect(women$KandName,"SZ")),
               aes(FemPerc, FemPos,
                  colour = as.factor(KandName),
                  label=KrajName)) +
          geom_point() +
          #facet_wrap(~KandName) + 
          scale_color_discrete("clarity") +
          ylim(0,1) + 
          xlim(0,1)
plot_women
```

```
## Error: object 'FemPerc' not found
```


This is a plot of the representation and position of women on lists of party X:


```r
plot(plot_women)
```

```
## Error: object 'FemPerc' not found
```


Look at how education affects position on lists


```r
# create dummies for relevant education markers
kraje$nodegree <- ifelse(kraje$KandTitul == "",1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$bc <- ifelse(kraje$KandTitul == "Bc.",1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$bc <- ifelse(kraje$KandTitul == "\\bBA",1,kraje$bc)
```

```
## Error: object 'kraje' not found
```

```r
kraje$ing <- ifelse(str_detect(kraje$KandTitul, "Ing."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$mgr <- ifelse(str_detect(kraje$KandTitul, "Mg"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$mudr <- ifelse(str_detect(kraje$KandTitul, "MUDr."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$judr <- ifelse(str_detect(kraje$KandTitul, "JUDr."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$mvdr <- ifelse(str_detect(kraje$KandTitul, "MVDr."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$rndr <- ifelse(str_detect(kraje$KandTitul, "RNDr."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$phdr <- ifelse(str_detect(kraje$KandTitul, "PhDr."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$phdr <- ifelse(str_detect(kraje$KandTitul, "Paed."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$mba <- ifelse(str_detect(kraje$KandTitul, "MBA"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$prof <- ifelse(str_detect(kraje$KandTitul, "Prof"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "Ph."),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "Th."),1,kraje$phd)
```

```
## Error: object 'kraje' not found
```

```r
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "CSc"),1,kraje$phd)
```

```
## Error: object 'kraje' not found
```

```r
kraje$doc <- ifelse(str_detect(kraje$KandTitul, "doc."),1,0)
```

```
## Error: object 'kraje' not found
```

```r

# build education level variable
kraje$KandEdu <- "bez VŠ"
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu[kraje$bc==1] <- "bakalářské"
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu[kraje$mgr==1 | kraje$judr==1 | kraje$mudr==1 | kraje$mvdr==1 | kraje$ing == 1] <- "magisterské"
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu[kraje$phd==1] <- "doktorské"
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu[kraje$doc==1] <- "docent"
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu[kraje$prof==1] <- "profesor"
```

```
## Error: object 'kraje' not found
```

```r

kraje$KandEdu <- as.factor(kraje$KandEdu)
```

```
## Error: object 'kraje' not found
```

```r
kraje$KandEdu <- relevel(kraje$KandEdu, ref = 2)
```

```
## Error: object 'kraje' not found
```

```r

# run first exploratory model
lm_party_educ <- lm(relPoradiKand ~ KandVek + KandEdu + female + mba + judr + mudr,
                    kraje,
                    #subset(str_detect(women$KandName,"TOP"))
                    )
```

```
## Error: object 'kraje' not found
```

```r
summary.lm(lm_party_educ)
```

```
## Error: object 'lm_party_educ' not found
```


Now let's look at professions/regional elected positions:


```r
kraje$starosta <- ifelse(str_detect(kraje$Povolani, "starost"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$starosta[kraje$starosta==1] <- ifelse(str_detect(kraje$Povolani[kraje$starosta==1],
                                                       "místostarost"),
                                     0,kraje$starosta[kraje$starosta==1])
```

```
## Error: object 'kraje' not found
```

```r
kraje$mistostarosta <- ifelse(str_detect(kraje$Povolani, "místostarost"),1,0)
```

```
## Error: object 'kraje' not found
```

```r

kraje$hejtman <- ifelse(str_detect(kraje$Povolani, "ejtm"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$vicehejtman <- 0
```

```
## Error: object 'kraje' not found
```

```r
kraje$vicehejtman[kraje$hejtman==1] <- ifelse(str_detect(kraje$Povolani[kraje$hejtman==1],
                                                         "nám"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$hejtman[kraje$hejtman==1] <- ifelse(str_detect(kraje$Povolani[kraje$hejtman==1],
                                                    "nám"),
                                    0,kraje$hejtman[kraje$hejtman==1])
```

```
## Error: object 'kraje' not found
```

```r

kraje$poslanec <- ifelse(str_detect(kraje$Povolani, "poslan"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$zastupitel <- ifelse(str_detect(kraje$Povolani, "zastupitel"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$radni <- ifelse(str_detect(kraje$Povolani, "\\bradní\\b"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
table(kraje$radni)
```

```
## Error: object 'kraje' not found
```

```r

kraje$lekar <- ifelse(str_detect(kraje$Povolani, "lékař"),1,0)
```

```
## Error: object 'kraje' not found
```

```r
kraje$ucitel <- ifelse(str_detect(kraje$Povolani, "učitel"),1,0)
```

```
## Error: object 'kraje' not found
```

```r

kraje$bezprislusnosti <- ifelse(kraje$Prislusnost=="BEZPP",1,0)
```

```
## Error: object 'kraje' not found
```



```r
krajelist <- unique(kraje$KrajName)
```

```
## Error: object 'kraje' not found
```

```r
lm_party_SPOZ <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"SPOZ")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_CSSD <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"ČSSD")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_ODS <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"ODS")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_TOP <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"TOP")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_KDU <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"KDU")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_VV <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"VV")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_KSCM <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"KSČM")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_SZ <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"SZ")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_Pirati <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"Piráti")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_Svobodni <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"Svobodní")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

lm_party_NarSoc <- lm(relPoradiKand ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     lekar*mudr +
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"NÁR.SOC.")
                   ))
```

```
## Error: object 'kraje' not found
```

```r

#summary.lm(lm_party_all)
```


           

```r
library(coefplot)
alllmplot <- multiplot(lm_party_ODS, lm_party_CSSD, lm_party_TOP, lm_party_VV,
            lm_party_KSCM, lm_party_SPOZ, lm_party_SZ, lm_party_Pirati,
            lm_party_Svobodni, lm_party_NarSoc,
            single=FALSE,
            names=c("ODS", "ČSSD", "TOP09", "VV", "KSČM", "SPOZ", "Zelení",
                    "Piráti","Svobodní","Národní Socialisté"),
            sort="natural",
            ncol=5,
            plot=TRUE,
            color="red",
            fillColor="blue",
            intercept=FALSE,
            zeroColor="grey",
            zeroType=1
            )
```

```
## Warning: restarting interrupted promise evaluation
```

```
## Error: object 'lm_party_ODS' not found
```

```r
alllmplot + theme(panel.background = element_rect(fill = "white")) +
  xlim(-0.8,0.8)
```

```
## Error: object 'alllmplot' not found
```

```r
#alllmplot <- alllmplot[with(alllmplot, order(Name)), ]

```



```r
library(nlme)
multireg <- lmList(relPoradiKand ~ KandVek + female +
                     onlybc + mgr + mudr + judr + mba +   akad + ing + nodegree +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel | Kandidatka,
                     kraje,
                   na.action=na.omit)
```

```
## Error: object 'kraje' not found
```



```r
kraje_select <- subset(kraje, (KandName=="ČSSD" | KandName=="ODS" | KandName=="Piráti"|
                               KandName=="TOP+STAN"|KandName=="VV"|KandName=="KSČM"|
                               str_detect(kraje$KandName,"SZ\\b")|KandName=="SPOZ"|
                               KandName=="NÁR.SOC."|
                               KandName=="Svobodní" | Kandidatka==51 | Kandidatka==69 |
                               Kandidatka==74))
```

```
## Error: object 'kraje' not found
```

```r
educrossplot <- ggplot(kraje_select, aes(x=KandName, y=KandEdu))
```

```
## Error: object 'kraje_select' not found
```

```r
educrossplot + stat_sum(aes(colour=..n..), geom="tile") +
  scale_colour_gradient(low="yellow", high="red") +
  facet_wrap(~KrajName, scales="free")
```

```
## Error: object 'educrossplot' not found
```

