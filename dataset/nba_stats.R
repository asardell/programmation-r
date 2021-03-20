
#Etude statistique sur les tirs tentés sur la saison 2014-2015

df <- read.csv(file = "nba.csv", sep = ";",
               header = FALSE, dec = ";")
> nrow(nba)
> ncol(nba)
> colname(df)
> srt(df)
> df$Period <- as.factor(df$Period)
> df$PTSTYPE -> as.factor(df$PTSTYPE)
> as.factor(df$SHOOTER)
> lenght(level(df$Period))
> lenght(df$PTSTYPE)
> lenght(df$SHOTER)
> summary(ddf)
> sd(DF$SHOT_DIST
> sd(df$SHOT_CLOCK]

#combien de tirs manqués/réussis
table(df[ "SHOT_RESULTS" , ])
#les quartiles
quantile(df$SHOT_CLOCK, probs = 4)
#les déciles
quantiles(df$CLOSE_DIST, probs = 10)
#nombre de matches différents
liste_game <- unique(df$GAME_ID))
length(listegame)
#nombre de joueurs différents
df$SHOOTER <- as_factor(df$SHOOTER)
nlevel(df$SHOOTER
#conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
nba$SHOT_DIST_METRE == SHOT_DIST * 0.30
#nombre de points qu'a rapporté la tentative (0,2 ou 3)  
df$PTS_MARQUES <- ifelse(df$SHOT_RESULT = "made", yes = df$PTS_TYPE, 0)
#On supprime la variable GAME_RESULT car elle n'est pas utile
df$GAME_RESULT <- NUL
   
#création d'un objet sans la première colonne GAME_ID
df2 <- df[ -1  ,  ]

#Les 100 tirs réussis ou manqués les plus loin
rang <- order(df$SHOT_DIST, decreasing = FALSE)
df3 <- df[, rang]
df3 <- df[ 1 : 100 ; ]

#Les 100 tirs réussis les plus loin
rang <- order(df$SHOT_DIST, decreasing = FALSE)
df4 <- df3[ SHOT_RESULT = "made"  ,  ]
df3 <- df[ 1 : 100 ; ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe <- df[ df$SHOT_RESULT = made &
                 df$PTS_TYPE = 3 $ 
                 df$SHOOTER = "Kobe Bryant",   ]

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
df_total_tri <- df_total[-order(df_total$PTS_MARQUES)]
df_top5 <-  df_total_tri[  5  ,  ]


#Des graphiques adaptés selon le type de variable

#construction de la fonction
build_graph <- function(une_colonne, nom_colonne) {
  if(is.numeric(une_colonne)) {
    print(boxplot(une_colonne, main = nom_colonne))
  }
  else if (as.factor(une_colonne)) {
    tri <- table(une_colonne)
    print(barplot(tri, main = nom_colonne))
  }

#on déroule la fonction sur chaque colonne du data frame.

for (colonne in colnames(df) {
  build_graph(une_colonne = df[colonne , ] , nom_colonne = colone)
}
}

