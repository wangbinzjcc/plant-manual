#############################################################################
#  Rlanguage Headquarters :                                                 #
#       run  AbuMaxdbhIV_SpeLoopSave function     wangbin2012-10-19 0:48:21 #
#############################################################################
library(akima)    
library(grid)
library(gridExtra)
set_fil <- 'F:/SpeciesManual/'
source(paste(set_fil, 'r.data/AbuMaxdbhIV_SpeLoopSave2012-10-28104125.R', sep=''))
# ~~~~~ 
elev <- read.csv(paste(set_fil, "r.data/elev83.csv", sep=''))  # 高层数据
ele1 <- elev[,c(1,2,4)];  names(ele1) <- c("x", "y", "z") 
dat.smooth <- with(ele1, interp(x, y, z, 
                      xo=seq(min(x) + 0.2, max(x) - 0.2, length=(max(x) - min(x)) ),
                      yo=seq(min(y) + 0.2, max(y) - 0.2, length=(max(y) - min(y)) ),
                                linear=T))
 zz <- dat.smooth$z; xx <- (1:nrow(zz));   yy <- (1:ncol(zz))

# codes ,names ,numbers of each species ~~~~~ 
Hys_cod0 <- read.csv(file=paste(set_fil,'graphfile/spe_cod_Hys-2012-10-26 110946.csv',sep=''))
 
# Calculate the value of  IV，Abundance, Max dbh, Names code, etc.
LGdat00 <- read.csv(paste(set_fil, 'r.data/LGdat-eng-wb-2012-10-27 113959.csv', sep=''))
LGdat0 <- subset(LGdat00,is.na(bra))
LGdat.IV <- AbuMaxdbhIV_Calculate(datas = LGdat0, side = 20)

# text description of every species~~~
tx.al <- readLines(paste(set_fil, 'graphfile/species-text-description-2012-10-29160409.txt', sep=''))
tx.no <- grep('^[1-9][0-9]?[0-9]?. ?\\w', tx.al)
tx.cut <-cut(1:length(tx.al), c(tx.no,'inf'), right = F)
### save text ~~~
t.len <- length(unique(tx.cut))
lapply(1:t.len, Species_text_Save)
### ### save plots，tables ~~~
 lap.n<- dim(LGdat.IV)[1]
lapply(1:lap.n, Species_plot_table_Save)

############################################################

### test for cod of every section ~~~
txt.f0 <- dir(paste(set_fil, 'txtfile/', sep=''))
gra.f0 <- dir(paste(set_fil, 'graphfile/', sep=''))
plot.f0 <- dir(paste(set_fil, 'plotfile/', sep=''))
tab.f0 <- dir(paste(set_fil, 'tabfile/', sep=''))
LG_d0 <- LGdat.IV[,c('sp.nam','cod')]
Hys_d0 <- Hys_cod0[,c('spe', 'cod')]
#
Hys_d0[is.na(match(Hys_d0$cod, LG_d0$cod)),]

txt.f0

Hys_d0[is.na(match(Hys_d0$cod, substr(txt.f0,1,6))),]

gra.f0 <- dir(paste(set_fil, 'graphfile/', sep=''))
g.n <- grep('[0-9][0-9]?[0-9]?-',gra.f0)
g.nam <- strsplit(gra.f0[g.n], '-')

gra_cod <- sapply(g.nam,function(x)toupper(paste(substr(x[[2]],1,3), substr(x[[3]],1,3), sep='')   ))

Hys_d0[is.na(match(Hys_d0$cod, gra_cod)),]
#################################

# 
# hhh <- Hys_cod0[is.na(match(Hys_d0$cod,  gra_cod)),]
# sp.nam <- lapply(strsplit(as.character(hhh$种拉丁名)," "),function(x)paste(x[1],x[2],sep='-'))
# lapply(paste("F:/SpeciesManual/new/", hhh$spe,hhh$科号,'-',sp.nam,sep=''), dir.create)
# ##########################



#
########################################################################
#   Caption:                 #
############################### 
# ~~~ Input: ~~~
#
# 'F:/Species Manual'
# 'AbuMaxdbhIV_SpeLoopSave2012-10-26152259.R'  in 'F:/Species Manual/r.data/
# 'LGdat-eng-wb-2012-10-26 173303.csv'         in 'F:/Species Manual/r.data/
# 'elev83.csv'                                 in 'F:/Species Manual/r.data/
# 'spedescr.txt'                               in 'F:/Species Manual/r.data/
#
# ~~~ Output: ~~~
#
#  Abudences,Max dbhs,Important valures,Codes of every species 
#                           in 'F:/Species Manual/r.data/Abu_Maxdbh_IV_Cod.csv'
#  the tables of every species            in 'F:/Species Manual/tabfile/'
#  the scatter diagrams of every species  in 'F:/Species Manual/pointfile/'
#  the text descriptions of every species in 'F:/Species Manual/txtfile/'
#
#######################################