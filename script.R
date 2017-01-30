
#############
# ANALYSE de la COMPETITIVITE des PAYS, rapport IMD2016
#############

# lien vers le site internet de l'IMD
browseURL(url="http://www.imd.org/wcc/news-wcy-ranking/")

# preparation des donnees
# setwd("...  [ mon chemin repertoire de travail ] ...")
library(dplyr)
library(ggplot2)
library(leaps)

# Fonctions utilisees dans ce script
import = function(file, desktop=FALSE, ...) { # entre guillemets
  path <- file
  if (desktop) {
    path <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/", file)
  }
  return(read.csv2(path, sep=";", dec=",", header=TRUE, ...)) 
}
HistoDens = function(x, intervalles=NULL, titre="histogramme et densite lissee") {
  g <- qplot(x, geom='blank') + 
    geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
    geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth=intervalles) +                        
    scale_colour_manual(name = 'densit?', values="red", label="lissage") +
    xlab("intervalles de la variable") + ylab("frequence & densite") + ggtitle(titre)
  return(g)
}

# Creation du tableau de donnees
df <- import("donnees.csv")
row.names(df) <- df$pays
# Correction des variables
df$log.population <- log10(df$population)
df$log.superficie <- log10(df$superficie)

#################
# DESCRIPTION DES DONNEES

glimpse(df)
# variable reponse (score): description
summary(df$score); message("ecart-type du score:"); df$score %>% sd %>% round(2)
shapiro.test(df$score)
# ecart d'ordre de grandeur pour population et surface
range(df$population) %>% log10 %>% diff
range(df$superficie) %>% log10 %>% diff
# normalite?
shapiro.test(df$log.population)
HistoDens(df$log.population, titre="distribution normale des valeurs de population, en LOG base 10") + scale_x_continuous(labels=c(0,"1m","10m", "100m", "1b")) + xlab("intervalles de population") + ylab("frequence et densite")

#################
# ANALYSE

# Test d’un lien entre superficie d’un pays et score
fit <- lm(data=df, score ~ log.population)
# des individus trop "influents" sur la regression normale?
which(cooks.distance(fit) > 3*length(fit$coef)/nrow(df))
# regression robuste
fit <- MASS::rlm(data=df, score ~ log.superficie)
car::Anova(fit); coef(fit)
# validation (rapide ici)
shapiro.test(fit$residuals)

# Test d’un lien entre population d’un pays et score
fit <- MASS::rlm(data=df, score ~ log.population)
car::Anova(fit); coef(fit)

# Test d’un lien entre appartenance a l’UE/zone Euro et le score
wilcox.test(x = df %>% filter(continent==1, UE==0, euro==0) %>% .$score,
            y = df %>% filter(continent==1, UE==1, euro==1) %>% .$score,
            alternative="two.sided", paired=F, exact=T, correct=F, conf.int=T)
# Test d’un lien entre appartenance a l’UE dans le monde le score
wilcox.test(x = df %>% filter(UE==0) %>% .$score,
            y = df %>% filter(UE==1) %>% .$score,
            alternative="two.sided", paired=F, exact=T, correct=F, conf.int=T)
# Test d’un lien entre appartenance a l’euro dans le monde le score
wilcox.test(x = df %>% filter(euro==0) %>% .$score,
            y = df %>% filter(euro==1) %>% .$score,
            alternative="two.sided", paired=F, exact=T, correct=F, conf.int=T)

# Test d'un modele complet et selection de variable
fit <- regsubsets(data=df, score ~ (log.population+log.superficie+UE+euro + continent)^2)
summary(fit)

################
# FIN
