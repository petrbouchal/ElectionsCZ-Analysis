##Krajské volby 2012
##========================================================

library(ggplot2)
library(stringr)
library(plyr)
library(reshape)
library(ggthemes)

setwd('~/github/local/ElectionsCZ-Analysis/')
kraje12 <- read.csv("../ElectionsCZ/data-votes/KZ2012/KZ12_PrefHlasy_KandidatKraj.csv")
strkraje08 <- read.csv("../ElectionsCZ/data-votes/KZ2008/KZ08_PrefHlasy_KandidatKraj.csv")

kraje12$year <- 2012
kraje08$year <- 2008

kraje <- rbind(kraje12, kraje08) 

## Resolve gender from names list

# load data for resolving female names
firstnames_female <- read.csv("../ElectionsCZ/OtherData/NamesFemale.csv")
firstnames_female$female <- 1
firstnames_male <- read.csv("../ElectionsCZ/OtherData/NamesMale.csv")
firstnames_male$female <- 0

nameslist <- rbind(firstnames_male, firstnames_female)
nameslist <- rename(nameslist, c("Name" = "KandKrestniJmeno"))
nameslist$Rank <- NULL
nameslist$Count <- NULL

nameslist <-unique(nameslist)

kraje <- join(kraje, nameslist,type="left",match="first")

krajstrana <- read.csv('../ElectionsCZ/CSVKraje/Kraje_PartyReg_2012_Kraj.csv')
krajstrana$Kandidatka <- krajstrana$KandidatkaCislo
krajstrana$URL <- NULL
krajstrana$HlasyStranaKraj <- krajstrana$Hlasy
krajstrana$Hlasy <- NULL
krajstrana$ProcentaStranaKraj <- krajstrana$Procenta
krajstrana$Procenta <- NULL

sumprefvotes <- ddply(kraje, .(Kandidatka,Kraj), summarize,
                       SumPrefVotes = sum(Hlasy))

kraje <- join(kraje, sumprefvotes)
kraje <- join(kraje, krajstrana)

krajstrana <- join(krajstrana, sumprefvotes)
krajstrana <- join(krajstrana, ciselnik_kandidatky)
krajstrana <- join(krajstrana, ciselnik_kraje)
krajstrana$UsedPrefVotes <- krajstrana$SumPrefVotes/(4*krajstrana$HlasyStranaKraj)

kraje$PercPrefVotes <- kraje$Hlasy/kraje$SumPrefVotes

check <- ddply(kraje, .(Kraj, Kandidatka), summarise,
              Check=sum(PercPrefVotes))
mean(check$Check)
rm(check)

rm(firstnames_female, firstnames_male, nameslist)

##Add real names for regions and parties, add candidate counts and relative
## position

ciselnik_kandidatky <- read.csv("../ElectionsCZ/OtherData/Ciselnik_KZ_2012_kandidatky.csv", sep="\t")
ciselnik_kandidatky <- rename(ciselnik_kandidatky, c("KandNum" = "Kandidatka"))
kraje <- join(kraje, ciselnik_kandidatky, by = "Kandidatka", match="first")

ciselnik_kraje <- read.csv("~/github/local/ElectionsCZ/OtherData/Ciselnik_KZ_2012_kraje.csv")
kraje <- join(kraje, ciselnik_kraje, by = "Kraj",match="first")

# create list of all candidate lists across regions, with names of both

kraje_pocetkand <- ddply(kraje, .(Kraj, Kandidatka), nrow)
kraje_pocetkand <- rename(kraje_pocetkand, c("V1"="PocetKand"))
kraje <- join(kraje, kraje_pocetkand, by=c("Kandidatka","Kraj"),match="first")

kraje$relPoradiKand <- kraje$PoradiKand/kraje$PocetKand
kraje$relPoradiKand <- 1-kraje$relPoradiKand

## Calculate how women are represented on lists and what positions they get

avgfemale <- ddply(kraje, .(Kraj, Kandidatka), summarise,
                   FemPerc = mean(female))

kraje_fem <- subset(kraje, female == 1)
avgpostfemale <- ddply(kraje_fem, .(Kraj, Kandidatka), summarise,
                      FemPos = 1-mean(PoradiKand/PocetKand))

women <- merge(avgfemale, avgpostfemale, c("Kandidatka", "Kraj"))
women <- merge(women, ciselnik_kandidatky)
women <- merge(women, ciselnik_kraje)

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

##This is a plot of the representation and position of women on lists of party X:

plot(plot_women)

## Look at how education affects position on lists

# create dummies for relevant education markers
kraje$nodegree <- ifelse(kraje$KandTitul == "",1,0)
kraje$bc <- ifelse(kraje$KandTitul == "Bc.",1,0)
kraje$bc <- ifelse(kraje$KandTitul == "\\bBA",1,kraje$bc)
kraje$ing <- ifelse(str_detect(kraje$KandTitul, "Ing."),1,0)
kraje$mgr <- ifelse(str_detect(kraje$KandTitul, "Mg"),1,0)
kraje$mudr <- ifelse(str_detect(kraje$KandTitul, "MUDr."),1,0)
kraje$judr <- ifelse(str_detect(kraje$KandTitul, "JUDr."),1,0)
kraje$mvdr <- ifelse(str_detect(kraje$KandTitul, "MVDr."),1,0)
kraje$rndr <- ifelse(str_detect(kraje$KandTitul, "RNDr."),1,0)
kraje$phdr <- ifelse(str_detect(kraje$KandTitul, "PhDr."),1,0)
kraje$paeddr <- ifelse(str_detect(kraje$KandTitul, "Paed."),1,0)
kraje$mba <- ifelse(str_detect(kraje$KandTitul, "MBA"),1,0)
kraje$prof <- ifelse(str_detect(kraje$KandTitul, "Prof"),1,0)
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "Ph."),1,0)
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "Th."),1,kraje$phd)
kraje$phd <- ifelse(str_detect(kraje$KandTitul, "CSc"),1,kraje$phd)
kraje$doc <- ifelse(str_detect(kraje$KandTitul, "doc."),1,0)

# build education level variable
kraje$KandEdu <- "bez VŠ"
kraje$KandEdu[kraje$bc==1] <- "bakalářské"
kraje$KandEdu[kraje$mgr==1 | kraje$judr==1 | kraje$mudr==1] <- "magisterské"
kraje$KandEdu[kraje$mvdr==1 | kraje$ing == 1] <- "magisterské"
kraje$KandEdu[kraje$phd==1] <- "doktorské"
kraje$KandEdu[kraje$doc==1] <- "docent"
kraje$KandEdu[kraje$prof==1] <- "profesor"

kraje$KandEdu <- as.factor(kraje$KandEdu)
kraje$KandEdu <- relevel(kraje$KandEdu, ref = 2)

# run first exploratory model
lm_party_educ <- lm(Hlasy ~ KandVek + KandEdu + female + mba + judr + mudr,
                    kraje,
                    #subset(str_detect(women$KandName,"TOP"))
                    )
summary.lm(lm_party_educ)

## Now let's look at professions/regional elected positions:

kraje$starosta <- ifelse(str_detect(kraje$Povolani, "starost"),1,0)
kraje$starosta[kraje$starosta==1] <- ifelse(str_detect(kraje$Povolani[kraje$starosta==1],
                                                       "místostarost"),
                                     0,kraje$starosta[kraje$starosta==1])
kraje$mistostarosta <- ifelse(str_detect(kraje$Povolani, "místostarost"),1,0)

kraje$hejtman <- ifelse(str_detect(kraje$Povolani, "ejtm"),1,0)
kraje$vicehejtman <- 0
kraje$vicehejtman[kraje$hejtman==1] <- ifelse(str_detect(kraje$Povolani[kraje$hejtman==1],
                                                         "nám"),1,0)
kraje$hejtman[kraje$hejtman==1] <- ifelse(str_detect(kraje$Povolani[kraje$hejtman==1],
                                                    "nám"),
                                    0,kraje$hejtman[kraje$hejtman==1])

kraje$poslanec <- ifelse(str_detect(kraje$Povolani, "poslan"),1,0)
kraje$zastupitel <- ifelse(str_detect(kraje$Povolani, "zastupitel"),1,0)
kraje$radni <- ifelse(str_detect(kraje$Povolani, "\\bradní\\b"),1,0)
table(kraje$radni)

kraje$lekar <- ifelse(str_detect(kraje$Povolani, "lékař"),1,0)
kraje$ucitel <- ifelse(str_detect(kraje$Povolani, "učitel"),1,0)

kraje$bezprislusnosti <- ifelse(kraje$Prislusnost=="BEZPP",1,0)
kraje$prvnimisto <- ifelse(kraje$PoradiKand==1,1,0)

krajelist <- unique(kraje$KrajName)
lm_party <- lm(PercPrefVotes ~ KandVek + female + PoradiKand +
                     KandEdu + prvnimisto +
                     judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + KrajName +
                     bezprislusnosti + lekar*mudr,
                     kraje,
                     subset=(str_detect(kraje$KandAbbrev,"TOP")
                   ))
summary.lm(lm_party)

lm_party_CSSD <- lm(Hlasy ~ KandVek + female +
                     KandEdu + judr + mudr + mba +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel + 
                     bezprislusnosti,
                     kraje,
                     subset=(str_detect(kraje$KandName,"ČSSD")
                   ))

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

#summary.lm(lm_party_all)
    
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
alllmplot + theme(panel.background = element_rect(fill = "white")) +
  xlim(-0.8,0.8)
#alllmplot <- alllmplot[with(alllmplot, order(Name)), ]


library(nlme)
multireg <- lmList(relPoradiKand ~ KandVek + female +
                     onlybc + mgr + mudr + judr + mba +   akad + ing + nodegree +
                     zastupitel + poslanec + radni + 
                     hejtman + vicehejtman +
                     starosta + mistostarosta +
                     lekar + ucitel | Kandidatka,
                     kraje,
                   na.action=na.omit)

kraje_select <- subset(kraje, (KandName=="ČSSD" | KandName=="ODS" | KandName=="Piráti"|
                               KandName=="TOP+STAN"|KandName=="VV"|KandName=="KSČM"|
                               str_detect(kraje$KandName,"SZ\\b")|KandName=="SPOZ"|
                               KandName=="NÁR.SOC."|
                               KandName=="Svobodní" | Kandidatka==51 | Kandidatka==69 |
                               Kandidatka==74))
educrossplot <- ggplot(kraje_select, aes(x=KandName, y=KandEdu))
educrossplot + stat_sum(aes(colour=..n..), geom="tile") +
  scale_colour_gradient(low="yellow", high="red") +
  facet_wrap(~KrajName, scales="free")

obcesocec2010 = read.csv("../ElectionsCZ/data-socec/obce2010.csv")
kz12_obce0 = read.csv("../ElectionsCZ/CSVKraje/Kraje_PartyTown_Kraj_2012.csv")
obcesocec2010$CisloObce <- obcesocec2010$KODOBCE 
kz12_obce = join(kz12_obce0, obcesocec2010)
rm(kz12_obce0)

kz12_obce <- join(kz12_obce, ciselnik_kandidatky)
kz12_obce <- rename(kz12_obce, c("Kraj" = "Kraj1"))
kz12_obce <- rename(kz12_obce, c("Okres" = "Kraj"))
kz12_obce <- rename(kz12_obce, c("Kraj1" = "Okres"))
kz12_obce <- join(kz12_obce, ciselnik_kraje)


kz12_KSCM <- subset(kz12_obce, KandAbbrev == "KSČM")
lm_KSCM <- lm(Procenta ~ 
                Ucast + PlatneProc + PocetObyv + MigraceSaldo +
                Prirustek + Over65Perc + Under15Perc + NezamPerc + factor(Kraj),
              kz12_obce,
              na.action=na.omit,
              subset=(str_detect(kz12_obce$KandAbbrev,"KSČM")))
summary.lm(lm_KSCM)

## Kolik hlasů šlo stranám < 5%?
hlasypod5 <- subset(krajstrana, ProcentaStranaKraj < 4.99)
hlasypod5 <- ddply(hlasypod5, .(Kraj), summarize,
                   HlasyPod5 = sum(HlasyStranaKraj))
hlasytotal  <- ddply(krajstrana, .(Kraj), summarize,
                     HlasyTotalKraj = sum(HlasyStranaKraj))
hlasyperkraj <- join(hlasypod5, hlasytotal)
hlasyperkraj$PercPod5 <- hlasyperkraj$HlasyPod5/hlasyperkraj$HlasyTotalKraj