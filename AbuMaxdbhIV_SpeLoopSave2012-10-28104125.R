#########################################################################
#AbuMaxdbhIV_Calculate、Species_LoopSave    王斌   2012年10月19日0:37:23#
#########################################################################

AbuMaxdbhIV_Calculate <- function(datas, side = 20){
  
if(any(datas$x==0)){datas[datas$x == 0, ]$x <- 0.05}
if(any(datas$y==0)){datas[datas$y == 0, ]$y <- 0.05}
# 相对多度
sp.abu <- table(datas$cod)  # 多度
rel.den<- sp.abu*100 / sum(sp.abu)
# 中文名
sp.nam <- unique(datas$sp)[match(names(sp.abu),unique(datas$cod))]
# 相对优势度
sp.dbh <- tapply(datas$dbh, datas$cod, function(x){sum(pi * x^2 /4)})
rel.dom <- sp.dbh*100 / sum(sp.dbh)
# 相对频度
sp.fre <- tapply(paste(ceiling(datas$x / side), ceiling(datas$y / side)),  
                 datas$cod, function(x){sum(table(x) > 0)})
rel.fre <- sp.fre * 100 / sum(sp.fre)
# 重要值
result1 <- data.frame(cod= names(sp.abu), sp.abu = as.numeric(sp.abu), max.dbh =tapply(datas$dbh,datas$cod,max),sp.nam = sp.nam)
IV <- as.numeric(rel.den + rel.dom + rel.fre) / 3
result2 <- result1[order(-IV), ]
result2$ivNO. <- 1:length(sp.abu)
rownames(result2) <- NULL
# 输出:
         result2                                  }
#
# #################################
# 原始数据  表头包括  "sp", "x", "y", "dbh", "cod"
#   datas<- read.csv('F:/TUCE10/data/zw10datCod.csv')
#   IV.result <- Abu_Maxdbh_IV_Calculate(datas, side = 20)
# ###################################
# ~~~
CharPast <- function(data){  
       s <- data[1]
     for(i in 2:length(data)){s <- paste(s,data[i],sep="\n")}
       s                  }
# ### ~~~~~
Species_text_Save <- function(jj){
  # text description ~~~

  # find  the code name  from one species' text description ~~~
  tx.spe1 <- tx.al[which(tx.cut==unique(tx.cut)[jj])]
  SpC.num <- grep('SpCode',tx.spe1)
  txt.cod <- sub(' ?\\w*（\\w*）＝ ?','',tx.spe1[SpC.num])
  
  # find the values of one species from dat0.IV  ~~~
  numb <- subset(LGdat.IV, cod==txt.cod )
  if(length(unlist(numb))==0){numb <- data.frame(sp.abu=-999,max.dbh=-999,ivNO.=-999)}   
  # add species' values to the text description ~~~  
  inst1 <- grep('Individual number', tx.spe1)
  inst2 <- grep('Max DBH', tx.spe1)
  inst3 <- grep('Importance value rank', tx.spe1)
  
  tx.spe1[inst1] <- sprintf('%s %s', tx.spe1[inst1], round(numb$sp.abu))
  tx.spe1[inst2] <- sprintf('%s %s cm', tx.spe1[inst2], round(numb$max.dbh,1))
  tx.spe1[inst3] <- sprintf('%s %s', tx.spe1[inst3], round(numb$ivNO.))
    
  tx.spe3 <- tx.spe1[grep('\\w',tx.spe1)[-1]]
 
  lat.nam <- do.call(paste, as.list(unlist(strsplit(tx.spe3[1]," "))[1:2]))
  other.nam <- do.call(paste, as.list(unlist(strsplit(tx.spe3[1]," "))[-c(1:2)]))
  if(length(other.nam)==0){other.nam=''}
  tx.spe3[1] <- sprintf('\\textit{%s} %s',lat.nam,other.nam)
    
  Spe1_CP <- CharPast(tx.spe3)  
  cat(Spe1_CP,file=paste(set_fil,'txtfile/', txt.cod, '.txt',sep=''), sep='/n')
}

######################################
Species_plot_table_Save <- function (jj) {
# LGdata ~~~
  LG.cod <- LGdat.IV$cod[jj]
  dat.spe1 <- subset(LGdat0,cod==LG.cod )
# Scatter plot ~~~
  pdf.sav=T
  point.fil <- paste(set_fil, 'plotfile/', LG.cod, '-plot.png', sep="")
  if(pdf.sav==T){          
   png(file=point.fil, width=1000, height=650)
                 }
  par(mar=c(4.8, 2.5, 1.2, 1.2),mex=1)
  image(xx, yy, zz, col = terrain.colors(100), axes = F,xlab="",ylab="")
  contour(xx, yy, zz, levels = seq(0, max(zz), by = 10), 
          add = TRUE, col = "peru",labcex = 1)
  points(dat.spe1[, c('x','y')],col = "blue",lwd=1.3, pch =1,
         cex =1.3+dat.spe1$dbh*0.8/10)
  axis(1, at = seq(0.5, 500, length.out = 11),  
       labels = seq(0, 500, by = 50),cex.axis=1.8)
  axis(2, at = seq(0.5, 300, length.out = 7),  
       labels = seq(0, 300, by = 50),cex.axis=1.8)
  box()
  mtext("个体分布图 Distribution of individuals", side=1, line=3.5, cex= 2.8) 
  if(pdf.sav==T){dev.off()}
  
# table ~~~ 
  
  pdf.sav=T
  dat.cut <- cut(dat.spe1$dbh, c(0,2,5,10,20,30,60,'inf'), right=FALSE)
  dat.tab <- table(dat.cut)  # 多度
  dat.rou <- round(dat.tab*100/sum(dat.tab),2)  # 百分比
  dat.nam <- c('1-2', '2-5', '5-10', '10-20', '20-30', '30-60','≥60')
  tab0 <- cbind(dat.nam, dat.tab, dat.rou)
  colnames(tab0) <- c('胸径区间\n(Diameter class)\n(cm)', 
                      '个体数\n(No. of individuals\nin the plot)', 
                      '比例\n(Proportion)') 
  rownames(tab0) <- NULL
  tab.fil <- paste(set_fil, 'tabfile/', LG.cod, '-tab.png', sep='')
  if(pdf.sav==T){          
    png(file=tab.fil, width=750,height=500,pointsize = 32)  
  } 
  grid.arrange(tableGrob(tab0, theme=get("theme.white")()),
               main = textGrob("径级分布表  DBH Class",y = -0.5,
                               gp = gpar(fontsize=40,col=1)))   
  if(pdf.sav==T){dev.off()}
#
}

##########################################################################

