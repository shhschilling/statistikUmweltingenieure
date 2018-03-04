##############################################################################
### Feedback highly welcome: sclg@zhaw.ch
##############################################################################


#Parameters to be changed for plotting
cexsize=1  #variable defining size of cexsize


par(lwd=1)
#par(cexsize=5)
#Platform independent opening and closing of images
#openGraph() replaces windows() on windows machines and quartz() on mac and linux
#openGraph = function( width=8, height=8/sqrt(2) , mag=1.0 , ... ) {

openGraph = function( width=7, height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    # X11( width=width*mag , height=height*mag , type="cairo" , ... ) #Unix style
    quartz( width=width*mag , height=height*mag  , ... )
  } else { # Windows OS
    windows( width=width*mag , height=height*mag , ... )
  }
}

#short function to save graphics displayed either in quartz(for mac) or windows
saveGraph = function( file="saveGraphOutput" , type="pdf" , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    if ( any( type == c("png","jpeg","jpg","tiff","bmp")) ) {
      sptype = type
      if ( type == "jpg" ) { sptype = "jpeg" }
      savePlot( file=paste(file,".",type,sep="") , type=sptype , ... )
    }
    if ( type == "pdf" ) {
      dev.copy2pdf(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "eps" ) {
      dev.copy2eps(file=paste(file,".",type,sep="") , ... )
    }
    if ( type == "svg" ) {
    dev.copy(svg, file=paste(file,".",type,sep=""))
    dev.off()
    }

  } else { # Windows OS
    file=paste(file,".",type,sep="") # force explicit extension
    savePlot( file=file , type=type , ... )
  }
}




###### Testing for Normality and Visualization ###############################

test_norm_vis=function(x,y_axis_hist = c(0,0.4)){

  par(mfrow=c(1,2),oma = c(0, 0,3, 0))

  n=length(x)

## breaks can be changed manually here:
  hist(x,freq=FALSE, col="grey",breaks="Sturges",
       xlim=c(mean(x)-5*sd(x),mean(x)+5*sd(x)),ylim=y_axis_hist)

  norm_dens = function(z){dnorm(z,mean(x),sd(x))}

  curve(norm_dens,col="red",add=TRUE,lwd=2)

  par(new=TRUE)
  plot(density(x),col="blue",xlab="",ylab="",
       axes=FALSE,main="",lwd=2,
       xlim=c(mean(x)-5*sd(x),mean(x)+5*sd(x)),ylim=y_axis_hist)

  legend("topright", inset=0.05,c("fitted","estimated"),
         lty=1,lwd=2,col=c("red","blue"))
  box()

  qqnorm(x)
  qqline(x,col="red",lwd=2)

  KS = ks.test(x,pnorm,mean(x),sd(x))
  p_KS = signif(KS$p.value,2)
  SH = shapiro.test(x)
  p_SH=signif(SH$p.value,2)


  mtext(paste("Shapiro-Wilk: P = ",p_SH,"\n Kolmogorov-Smirnoff: P = ",p_KS,
        "\n Nullhypothesis: Data is normally distributed" ),
        outer = TRUE)

}

##############################################################################
##############################################################################

###### One-Sample t-Test ###############################

one_sample_tTest_twoSided = function(x,alpha,eff_mean){

openGraph()
par(oma=c(0,0,3,0))
stripchart(x,vertical=TRUE,xlim=c(0,2.5),col="grey50",xlab="")
axis(side=1,at=1,label="Sample 1")

points(mean(x),col=2,pch=1,lwd=3)

correction = qt(1-alpha,length(x)-1)*sd(x)/sqrt(length(x))

arrows(1,mean(x)+correction, 1, mean(x)-correction, angle=90, code=3,
       col=2,lty=1,lwd=2,length=0.1)

lines(x=c(0.7,1.3),y=c(eff_mean,eff_mean),col="blue",lwd=3)

legend("topright",inset=0.05,c("known mean","sample mean"),
       col=c("blue","red"),lwd=c(3,2))

t = t.test(x,mu=eff_mean,alternative="two.sided",)
p_value=t$p.value
p_value = signif(p_value,3)

mtext(paste("One Sample t-Test (two-sided): P = ",p_value,
            "\n Confidence Level = ",1-alpha), outer = TRUE)

# assumptions...
openGraph( )
test_norm_vis(x)


}



###### One-Sample t-Test ###############################

one_sample_tTest = function(x,alpha,eff_mean,side="two.sided"){

openGraph()
par(oma=c(0,0,3,0))
stripchart(x,vertical=TRUE,xlim=c(0,2.5),col="grey50",xlab="")
axis(side=1,at=1,label="Sample 1")

points(mean(x),col=2,pch=1,lwd=3)

correction = qt(1-alpha,length(x)-1)*sd(x)/sqrt(length(x))

arrows(1,mean(x)+correction, 1, mean(x)-correction, angle=90, code=3,
       col=2,lty=1,lwd=2,length=0.1)

lines(x=c(0.7,1.3),y=c(eff_mean,eff_mean),col="blue",lwd=3)

legend("topright",inset=0.05,c("reference mean","sample mean"),
       col=c("blue","red"),lwd=c(3,2))

t = t.test(x,mu=eff_mean,alternative=side)
p_value=t$p.value
p_value = signif(p_value,3)

mtext(paste("One Sample t-Test (sample", side,"): P = ",p_value,
            "\n Confidence Level = ",1-alpha), outer = TRUE)

# assumptions...
openGraph( )
test_norm_vis(x)

}


########################################################
## Alle 2-sample Tests: Intervalle gem. t-Test, konkrete
## Rechnungen mit Welch-Test, kleine Diskrepanzen moeglich

###### Two-Sample t-Test ###############################

two_sample_tTest = function(samples,fact,alpha,side="two.sided"){
  
  levels = unique(sort(fact))
  
  x1 = samples[fact==levels[1]]
  x2 = samples[fact==levels[2]]
  
  x = c(x1,x2)
  sp = max(x)-min(x); mi = min(x)-0.05*sp; ma = max(x)+0.2*sp;
  x = cbind(x,factor( c( rep(1,length(x1)),rep(2,length(x2)) ) )  )
  
  openGraph()
  par(oma=c(0,0,3,0))
  stripchart(samples~fact,vertical=TRUE,xlim=c(0,3),ylim=c(mi,ma),
             col=c("grey70","grey80"),ylab="samples",xlab="",axes=FALSE)
  
  axis(side=2)
  axis(side=1,at=c(1,2),label=levels)
  box()
  
  points(1,mean(x1),col=2,pch=1,lwd=3)
  points(2,mean(x2),col=2,pch=1,lwd=3)
  
  alpha_c = 1 - sqrt(1-alpha);
  
  correction1 = qt(1-0.5*alpha_c,length(x1)-1)*sd(x1)/sqrt(length(x1))
  correction2 = qt(1-0.5*alpha_c,length(x2)-1)*sd(x2)/sqrt(length(x2))
  
  arrows(1,mean(x1)+0.5*correction1, 1, mean(x1)-0.5*correction1, angle=90, code=3,
         col=2,lty=1,lwd=2,length=0.1)
  arrows(2,mean(x2)+0.5*correction2, 2, mean(x2)-0.5*correction2, angle=90, code=3,
         col=2,lty=1,lwd=2,length=0.1)
  
  abline(h=mean(x1)+0.5*correction1,col="grey30",lty=2,lwd=1)
  abline(h=mean(x1)-0.5*correction1,col="grey30",lty=2,lwd=1)
  
  legend("topright",inset=0.05,c("means and alpha corrected confidence Intervalls"),
         col=c("red"),lwd=2)
  
  t = t.test(x1,x2,alternative=side)
  p_value=t$p.value
  p_value = signif(p_value,3)
  
  mtext(paste("Two Sample Welch-Test(,",side,"): P = ",p_value,
              "\n Confidence Level = ",1-alpha), outer = TRUE)
  
  # assumptions...
  openGraph( )
  #test_norm_vis(rstandard(aov(samples~fact)))
  test_norm_vis(samples[fact==levels[1]])
  openGraph()
  test_norm_vis(samples[fact==levels[2]])
  
}














##############################################################################
##############################################################################

###### One-Sample Wilcoxon-Test / Vorzeichentest ###############################

one_sample_WilcoxonTest_twoSided = function(x,alpha,eff_med){

par(oma=c(0,0,3,0))
stripchart(x,vertical=TRUE,xlim=c(0,2.5),col="grey50",xlab="")
axis(side=1,at=1,label="Sample 1")

boxplot(x,notch=F,border="red",add=TRUE)

lines(x=c(0.7,1.3),y=c(eff_med,eff_med),col="blue",lwd=3)

legend("topright",inset=0.05,c("known median","sample median"),
       col=c("blue","red"),lwd=c(3,2))

t = wilcox.test(x,mu=eff_med,alternative="two.sided")
p_value=t$p.value
p_value = signif(p_value,3)

mtext(paste("Sign-Test (two-sided): P = ",p_value,
            "\n Confidence Level of notches appr. 0.95"), outer = TRUE)
}

###### One-Sample Wilcoxon-Test / Vorzeichentest ###############################



one_sample_WilcoxonTest = function(x,alpha,eff_med,side="two.sided",no=F){
  
  par(oma=c(0,0,3,0),mfrow=c(1,1))
  
  stripchart(x,vertical=TRUE,xlim=c(0,2.5),col="grey50",xlab="")
  axis(side=1,at=1,label="Sample 1")
  
  boxplot(x,notch=no,border="red",add=TRUE)
  
  lines(x=c(0.7,1.3),y=c(eff_med,eff_med),col="blue",lwd=3)
  
  legend("topright",inset=0.05,c("reference median","sample median"),
         col=c("blue","red"),lwd=c(3,2))
  
  t = wilcox.test(x,mu=eff_med,alternative=side)
  p_value=t$p.value
  p_value = signif(p_value,3)
  
  mtext(paste("Wilcoxon signed rank est (sample:",side,"): P = ",p_value),
        #      "\n Confidence Level of notches appr. 0.95"), 
        outer = TRUE)
}








###### Two-Sample U-Test  ###############################


#One function with flags for greater, less, two sided and notch
two_sample_WilcoxonTest = function(samples,fact,alpha,side="two.sided",notchf=F){
  #side: Seitigkeit
  #notchf: TRUE: plot notches in box plots
  levels = unique(sort(fact))
  
  x1 = samples[fact==levels[1]]
  x2 = samples[fact==levels[2]]
  
  x = c(x1,x2)
  sp = max(x)-min(x); mi = min(x)-0.05*sp; ma = max(x)+0.2*sp;
  x = cbind(x,factor( c( rep(1,length(x1)),rep(2,length(x2)) ) )  )
  openGraph()
  par(mfrow=c(1,1),oma=c(0,0,3,0))
  stripchart(samples~fact,vertical=TRUE,xlim=c(0,3),ylim=c(mi,ma),
             col=c("grey70","grey80"),axes=FALSE)
  
  axis(side=2)
  axis(side=1,at=c(1,2),label=levels)
  box()
  notchvalue=notchf
  boxplot(samples~fact,notch=notchvalue,border="red",add=TRUE,
          axes=FALSE,boxwex=0.3)
  
  par(oma=c(0,0,3,0))
  stripchart(samples~fact,vertical=TRUE,xlim=c(0,3),ylim=c(mi,ma),xlab=xlabel,
             col=c("grey70","grey80"),add=TRUE)
  
  
  
  
  legend("top",inset=0.05,
         c("medians, 0.25- and 0.75 quantiles, range and outliers"),
         col=c("red"),lwd=2)
  
  t = wilcox.test(samples~fact,alternative=side)

  p_value=t$p.value
  #p_value = signif(p_value,5)
 p_value=formatC(signif(p_value,digits=2))

  if(side=="less") {
    compare=c("<")
  } else if (side=="greater"){
    compare =c(">")
  } else
    compare=c("equals")
  mtext(paste(
   " Wilcoxon-Test: AH: median of factor",unique(fact)[1],compare,
              "median of factor", unique(fact)[2],
    "\n Wilcoxon-Test(",compare,"),",
    "p = ",p_value
            #  ,"\n Confidence Level of notches appr. 0.95"
   )
  ,cex=cexsize,outer=TRUE)
  return(t)

 
}


##############################################################################
##############################################################################

###### Two-Sample F-Test ###############################
#subtract means; two lines according to variances.

two_sample_FTest_twoSided = function(samples,fact,alpha){

levels = unique(sort(fact))

x1 = samples[fact==levels[1]]
x2 = samples[fact==levels[2]]

x1 = x1-mean(x1);
x2 = x2-mean(x2);

x = c(x1,x2)
sp = max(x)-min(x); sp = max(sp,var(x1),var(x2));
mi = min(x)-0.3*sp; ma = max(x)+0.3*sp;
x = cbind(x,factor( c( rep(1,length(x1)),rep(2,length(x2)) ) )  )

par(oma=c(0,0,3,0))
stripchart(x[,1]~x[,2],vertical=TRUE,xlim=c(0.5,3),ylim=c(mi,ma),
col=c("grey70","grey80"),ylab="centered samples",xlab="",axes=FALSE)

axis(side=2)
axis(side=1,at=c(1,2),label=levels)
box()

lines(x=c(1.1,1.1),y=c(-0.5*var(x1),0.5*var(x1)),col="blue",lwd=5)
lines(x=c(1.9,1.9),y=c(-0.5*var(x2),0.5*var(x2)),col="blue",lwd=5)

legend("topright",inset=0.05,c("variances"),
       col=c("blue"),lwd=2)

t = var.test(x1,x2,alternative="two.sided")
p_value=t$p.value
p_value = signif(p_value,3)

mtext(paste("Two Sample F-Test (two sided): P = ",p_value,
            "\n Confidence Level = ",1-alpha), outer = TRUE)
}













###### Two-Sample F-Test ###############################
#subtract means; two lines according to variances.

two_sample_FTest_less = function(samples,fact,alpha){

levels = unique(sort(fact))

x1 = samples[fact==levels[1]]
x2 = samples[fact==levels[2]]

x1 = x1-mean(x1);
x2 = x2-mean(x2);

x = c(x1,x2)
sp = max(x)-min(x); sp = max(sp,var(x1),var(x2));
mi = min(x)-0.3*sp; ma = max(x)+0.3*sp;
x = cbind(x,factor( c( rep(1,length(x1)),rep(2,length(x2)) ) )  )

par(oma=c(0,0,3,0))
stripchart(x[,1]~x[,2],vertical=TRUE,xlim=c(0.5,3),ylim=c(mi,ma),
col=c("grey70","grey80"),ylab="centered samples",xlab="",axes=FALSE)

axis(side=2)
axis(side=1,at=c(1,2),label=levels)
box()

lines(x=c(1.1,1.1),y=c(-0.5*var(x1),0.5*var(x1)),col="blue",lwd=5)
lines(x=c(1.9,1.9),y=c(-0.5*var(x2),0.5*var(x2)),col="blue",lwd=5)

legend("topright",inset=0.05,c("variances"),
       col=c("blue"),lwd=2)

t = var.test(x1,x2,alternative="less",)
p_value=t$p.value
p_value = signif(p_value,3)

mtext(paste("Two Sample F-Test (1<2): P = ",p_value,
            "\n Confidence Level = ",1-alpha), outer = TRUE)
}

###### Two-Sample F-Test ###############################
#subtract means; two lines according to variances.
two_sample_FTest = function(samples,fact,alpha,side="two.sided"){
  
  levels = unique(sort(fact))
  
  x1 = samples[fact==levels[1]]
  x2 = samples[fact==levels[2]]
  
  x1 = x1-mean(x1);
  x2 = x2-mean(x2);
  
  x = c(x1,x2)
  sp = max(x)-min(x); sp = max(sp,var(x1),var(x2));
  mi = min(x)-0.3*sp; ma = max(x)+0.3*sp;
  x = cbind(x,factor( c( rep(1,length(x1)),rep(2,length(x2)) ) )  )
  
  par(oma=c(0,0,3,0))
  stripchart(x[,1]~x[,2],vertical=TRUE,xlim=c(0.5,3),ylim=c(mi,ma),
             col=c("grey70","grey80"),ylab="centered samples",xlab="",axes=FALSE)
  
  axis(side=2)
  axis(side=1,at=c(1,2),label=levels)
  box()
  
  lines(x=c(1.1,1.1),y=c(-0.5*var(x1),0.5*var(x1)),col="blue",lwd=5)
  lines(x=c(1.9,1.9),y=c(-0.5*var(x2),0.5*var(x2)),col="blue",lwd=5)
  
  legend("topright",inset=0.05,c("variances"),
         col=c("blue"),lwd=2)
  
  t = var.test(x1,x2,alternative=side)
  p_value=t$p.value
  p_value = signif(p_value,3)
  
  mtext(paste("Two Sample F-Test (",side,"): P = ",p_value,
              "\n Confidence Level = ",1-alpha), outer = TRUE)
}



##############################################################################
##############################################################################

###### chi squared Test ###############################

chi_squared_test_vis = function( counts,
                                 count_labels = colnames(counts),
                                 category_names = rownames(counts) ){

  norm_counts = counts;

  for (i in 1:nrow(counts)){
    norm = sum(counts[i,]);
    norm_counts[i,] = counts[i,]/norm;
  }
  max_val_y = max(norm_counts)

  col_vec_full = rainbow(nrow(counts))
  col_vec_fade=rainbow(nrow(counts),s=0.5)
  x_val=seq(-0.5,ncol(norm_counts)+0.5,1)
  y_val=c(0,norm_counts[1,],0)

par(oma=c(0,0,3,0))

  plot(x_val,y_val,type="b",xlim=c(-0.5,ncol(counts)+1),
       ylim=c(0,1.4*max_val_y),axes=FALSE,xlab="classes",ylab="probability",
       col=col_vec_full[1],lwd=3)

  for (i in 2:nrow(counts)){
    y_val=c(0,norm_counts[i,],0)

    par(new=TRUE)
    plot(x_val,y_val,type="b",xlim=c(-0.5,ncol(counts)+1),
         ylim=c(0,1.4*max_val_y),axes=FALSE,xlab="classes",ylab="probability",
         col=col_vec_full[i],lwd=3)
  }

  barplot(norm_counts,names.arg=count_labels,
          xlim=c(-0.5,ncol(counts)+1),
          ylim=c(0,1.4*max_val_y),
          width=1/(nrow(counts)+1),space=c(0,1),add=TRUE,col=col_vec_fade,
          density=rep(20,nrow(counts)), border=col_vec_fade,
          beside=TRUE)

  box()

  category_names = as.character(category_names)

  legend("topright",inset=0.05,category_names,
       col=col_vec_full,lwd=2)

  chi = chisq.test(counts)
  p_value=chi$p.value
  p_value = signif(p_value,3)

  mtext(paste("Chi Squared Test: P = ",p_value), outer = TRUE)

}


##############################################################################
##############################################################################

###### Visualize ANOVA ###############################
## performs ANOVA, oneway test and post-hoc t.test
vis_ANOVA_clusters=function(samples,factor,alpha=0.05,xlab="groups",ylab="samples",cex=cexsize){

  n_classes = length(unique(factor))

  s = tapply(samples,factor,sd)
  m = tapply(samples,factor,mean)

  samples_per_class = c()
  for (i in 1:n_classes){
    samples_per_class[i] = sum(factor==unique(factor)[i])
  }

  an = aov(samples~factor)
  a = summary(an)

  oneway=oneway.test(samples~factor)

  maximum = max(samples,na.rm=T);
  minimum = min(samples,na.rm=T);
  sp = maximum - minimum;
  mi = minimum-0.3*sp; ma = maximum+0.3*sp;

openGraph()
  par(oma=c(0,0,3,0))
  stripchart(samples~factor,vertical=TRUE,xlim=c(0,n_classes+1),
            ylim=c(mi,ma),
            col=rep("grey30",n_classes),
            ylab=ylab,xlab=xlab,las=2)

# sd:
for (i in 1:n_classes){
  lines(x=c(i-0.2,i-0.2),y=c(m[[i]]-0.5*s[[i]],m[[i]]+0.5*s[[i]]),
        col=colors()[131],lwd=5)
}

for (i in 1:n_classes){
  lines(x=c(i-0.1,i+0.1),y=c(m[[i]],m[[i]]),
        col=colors()[552],lwd=3)
  arrows(i, m[[i]]+qt(1-0.025,samples_per_class[i]-1)*s[[i]]/sqrt(samples_per_class[i]),
         i, m[[i]]-qt(1-0.025,samples_per_class[i]-1)*s[[i]]/sqrt(samples_per_class[i]), angle=90, code=3,
         col=colors()[552],lty=1,lwd=2,length=0.1)

}

  tuk = TukeyHSD(an)
  library(multcompView)
  s = multcompLetters(tuk[[1]][,4],threshold=alpha)

  ord = c();
  v = attributes(s$Letters)$names
  f_levels = sort(unique(factor))
  for (i in 1:n_classes){
    ord[i] = which(v == f_levels[i])
  }

  text(seq(1:n_classes+1),mi,s$Letters[ord],col=colors()[81],cex=cexsize,lwd=2)

  mtext(paste("ANOVA: P = ",a[[1]][["Pr(>F)"]][[1]],"\n",
              "OneWay: P = ",oneway$p.value), outer = TRUE)
  legend("topright",inset=0.05,horiz=TRUE,
         c("sd","mean with 95% conf. intervall"),
         col=c(colors()[131],colors()[552]),lwd=4)

openGraph( )
par(mfrow=c(1,2), oma=c(0,0,3,0))
  plot(an$fitted,rstandard(an),main="std. Residuals vs. Fitted")
  abline(h=0,col=1,lwd=2)

  qqnorm(rstandard(an))
  qqline(rstandard(an),col="red",lwd=2)

  KS = ks.test(rstandard(an),pnorm,mean(rstandard(an)),sd(rstandard(an)))
  p_KS = signif(KS$p.value,3)
  SH = shapiro.test(rstandard(an))
  p_SH=signif(SH$p.value,3)
  bartlett=bartlett.test(samples~factor)
  bart = signif(bartlett$p.value,3)
  mtext(paste("Residual Analysis\n Shapiro-Wilk: P = ",p_SH,
              "\n Kolmogorov-Smirnoff: P = ",p_KS,
              "\n Homoscedacity:Bartlett Test, P = ",bart
              ),
        outer = TRUE)


}

###### Visualize ANOVA ###############################
#vis_ANOVA_interaction=function(samples,factor){...}

##############################################################################
### helper for Kruskal Wallis and post-hoc Wilcox:
sig_diffs_nongauss <- function(samples, factor)
{
	# function to produce a table similar to that produced for TukeyHSD
	# but for non-normally distributed data

	# calculate p values for each data classification

	ufactor = levels(factor)
	pwt = pairwise.wilcox.test(samples, factor)
	factormeans = matrix(0,length(ufactor),1)
	for(ii in 1:length(ufactor)){
		pos = which(factor == ufactor[ii]);
		factormeans[ii] = mean(samples[pos]);
	}

	# make a matrix with a row for every possible combination of
	# 2 data classifications and populate it with the calculated
	# p values

	xcomb = combn(length(ufactor), 2)
	tukeylike = matrix(0,ncol(xcomb),4)
	colnames(tukeylike) <- c("diff","lwr","upr","p adj")
	tukeynames = vector("list",ncol(xcomb))
	for(ii in 1:ncol(xcomb)){
		tukeynames[ii] =
			paste(ufactor[xcomb[2,ii]],"-",ufactor[xcomb[1,ii]],sep="");

            p_value = pwt$p.value[xcomb[2,ii]-1, xcomb[1,ii]];
            if (is.na(p_value)){p_value=1}
            tukeylike[ii,4] = p_value
		tukeylike[ii,1] = 0;
		tukeylike[ii,2] = 0;
		tukeylike[ii,3] = 0;
	}
	rownames(tukeylike) = tukeynames

	# re-format the table slightly so it is the same as that produced
	# by TukeyHSD and output

	tukeylike2 = list(tukeylike)
	print(tukeylike2)
	return(tukeylike2)
}


###### Visualize Kruskal_Wallis ###############################
## performs Kruskal Wallis and post-hoc Wilcoxon:

vis_Kruskal_Wallis_clusters=function(samples,factor,alpha=0.05,xlab="groups",ylab="samples",cex=cexsize,notch=F){

  n_classes = length(unique(factor))
  #define color scheme dependent on number of classes

    mc=rainbow(n_classes)


  s = tapply(samples,factor,sd)
  m = tapply(samples,factor,mean)

  samples_per_class = c()
  for (i in 1:n_classes){
    samples_per_class[i] = sum(factor==unique(factor)[i])
  }

  kk = kruskal.test(samples~factor)

  maximum = max(samples,na.rm=T);
  minimum = min(samples,na.rm=T);
  sp = maximum - minimum;
  mi = minimum-0.1*sp; ma = maximum+0.1*sp;


  openGraph()
  par(mfrow=c(1,1),oma = c(1, 0,1, 0)) #oma: outer margin sout, west, north, east
  
  if (notch==TRUE){
  b=boxplot(samples~factor,notch=TRUE,col=mc,las=1,xlim=c(0,n_classes+1),ylim=c(mi,ma),boxwex=0.5)
  }
  else
  {   b=boxplot(samples~factor,notch=FALSE,col=mc,las=1,xlim=c(0,n_classes+1),ylim=c(mi,ma),boxwex=0.5)
               }


  stripchart(samples~factor,vertical=TRUE,
             #method="jitter",
             col=rep("grey50",n_classes),
             ylab=ylab,xlab=xlab,
             las=1,#horizontal legend,
             add=TRUE
             )

mtext(c("n= ",b$n),at=c(0.7,seq(1,n_classes)),las=1) #nmber of cases in each group
  tuk = sig_diffs_nongauss(samples,factor)
  library(multcompView)
  s = multcompLetters(tuk[[1]][,4],threshold=alpha)

  ord = c();
  v = attributes(s$Letters)$names
  f_levels = sort(unique(factor))
  for (i in 1:n_classes){
    ord[i] = which(v == f_levels[i])
  }
(ma)
text(seq(1:n_classes+1),mi,s$Letters[ord],col="darkgreen",cex=cexsize,lwd=2)

  title(paste("Kruskal Wallis: P = ",signif(kk$p.value,digits=4)), outer = TRUE)
}




#############################################################################
#############################################################################

###### Visualize ANOVA ###############################
## performs 2way ANOVA
vis_2wayANOVA_clusters=function(samples,factor_1,factor_2){

  n_classes = length(unique(factor))

  s = tapply(samples,list(factor_1,factor_2),sd)
  m = tapply(samples,list(factor_1,factor_2),mean)

  samples_per_class = c()
  for (i in 1:n_classes){
    samples_per_class[i] = sum(factor==unique(factor)[i])
  }

  an = aov(samples~factor_1*factor_2)
  a = summary(an)

  maximum = max(samples);
  minimum = min(samples);
  sp = maximum - minimum;
  mi = minimum-0.3*sp; ma = maximum+0.3*sp;


}


#############################################################################
#############################################################################

### helper functions for regression: ###

conf_band = function(x,reg,P,up){
  a = reg$coefficients[2]
  b = reg$coefficients[1]
  md = x-mean(x);
  result=x;
  for (i in 1:length(x)){
  result[i] = a*x[i] + b + up*qt(P,length(x)-2)* sqrt(sum(reg$resid*reg$resid)/(length(x)-2))* sqrt(1/(length(x)-2) + md[i]^2/sum(md*md))
  }
  return(result)
}

progn_band = function(x,reg,P,up){
  a = reg$coefficients[2]
  b = reg$coefficients[1]
  md = x-mean(x);
  result=x;
  for (i in 1:length(x)){
  result[i] = a*x[i] + b + up*qt(P,length(x)-2)* sqrt(sum(reg$resid*reg$resid)/(length(x)-2))* sqrt(1 + 1/(length(x)-2) + md[i]^2/sum(md*md))
  }
  return(result)
}


##### Visualize Regression und trumpet curves ###############################
vis_regr_trumpets = function(x,y,P){

  reg = lm(y~x)
  summary(reg)

  ## error bands:
  y_conf_low = conf_band(x,reg,P,-1);
  y_conf_up = conf_band(x,reg,P,1);


  ma = max(y,reg$fitted);
  mi = min(y,reg$fitted);
  sp = ma - mi;

  openGraph()
  par(oma=c(0,0,5,0))
  plot(x,y,ylim=c(mi-0.1*sp,ma+0.4*sp))
  points(x,reg$fitted,type="l",col=2,lwd=2)

  points(x,y_conf_low,type="l",lwd=2,lty=2,col=colors()[84])
  points(x,y_conf_up,type="l",lwd=2,lty=2,col=colors()[84])


  legend("topleft",c("regr. line",paste("trumpet curves for gamma=",P)),
         lwd=2,col=c(2,colors()[84],colors()[85]),lty=c(1,2,3),bty = "n")

  s = summary(reg);


  mtext(paste("Regression: ax + b. trumpet curves for gamma = ", P,"\n \n"),outer=TRUE,cex=1.5)
  mtext(paste("\n \n a = ",signif(reg$coefficients[2],2), ", p = ", signif(s$coefficients[2,4],2),
              "\n b = ",signif(reg$coefficients[1],2), ", p = ", signif(s$coefficients[1,4],2),
              "\n R^2 = ",signif(summary(reg)$r.squared,4)
  ),

  outer = TRUE)


  openGraph()
  par(mfrow=c(1,2), oma=c(0,0,3,0))
  plot(reg$fitted,residuals(reg),main="Residuals vs. Fitted",xlab="Fitted Values",ylab="Residuals")
  abline(h=0,col=1,lwd=2)
  
  qqnorm(residuals(reg),ylab="Sample Quantiles of  Residuals")
  qqline(residuals(reg),col="red",lwd=2)
  
  KS = ks.test(residuals(reg),pnorm,mean(residuals(reg)),sd(residuals(reg)))
  p_KS = signif(KS$p.value,2)
  SH = shapiro.test(residuals(reg))
  p_SH = signif(SH$p.value,2)
  
  mtext(paste("Residual Analysis\n Shapiro-Wilk: P = ",p_SH,
              "\n Kolmogorov-Smirnoff: P = ",p_KS ),
        outer = TRUE)
  
  
  
  
  
  
  

}






###### Visualize Residuals ###############################
vis_resid = function(resid,fitted){

par(mfrow=c(1,2), oma=c(0,0,3,0))
  plot(fitted,resid,main="Residuals vs. Fitted")
  abline(h=0,col=1,lwd=2)

  qqnorm(resid)
  qqline(resid,col="red",lwd=2)

  KS = ks.test(resid,pnorm,mean(resid),sd(resid))
  p_KS = signif(KS$p.value,2)
  SH = shapiro.test(resid)
  p_SH=signif(SH$p.value,2)

  mtext(paste("Residual Analysis\n Shapiro-Wilk: P = ",p_SH,
              "\n Kolmogorov-Smirnoff: P = ",p_KS ),
        outer = TRUE)
  
  
  
  

}


###### Visualize Regression ###############################
vis_regr = function(x,y,ylim,alpha=0.05){
  P=alpha
  ord = order(x);
  x = sort(x)
  y = y[ord]

  reg = lm(y~x)
  summary(reg)

  ## error bands:
  y_conf_low = conf_band(x,reg,P,-1);
  y_conf_up = conf_band(x,reg,P,1);
  y_progn_low = progn_band(x,reg,P,-1);
  y_progn_up = progn_band(x,reg,P,1);

  ma = max(y,reg$fitted,y_progn_up);
  mi = min(y,reg$fitted,y_progn_low);
  sp = ma - mi;

  openGraph()
  par(oma=c(0,0,6,0))
  #  plot(x,y,ylim=c(mi-0.1*sp,ma+0.4*sp))
  plot(x,y,ylim=ylim)
  points(x,reg$fitted,type="l",col=2,lwd=2)

  points(x,y_conf_low,type="l",lwd=2,lty=2,col=colors()[84])
  points(x,y_conf_up,type="l",lwd=2,lty=2,col=colors()[84])
  points(x,y_progn_low,type="l",lwd=2,lty=3,col=colors()[85])
  points(x,y_progn_up,type="l",lwd=2,lty=3,col=colors()[85])

  legend("topleft",inset=0.05,c("regr. line","confidence band","prognosis band"),
         lwd=2,col=c(2,colors()[84],colors()[85]),lty=c(1,2,3))

  s = summary(reg);
  b = confint(reg);

  #mtext("test", outer = TRUE)

  mtext(paste("Regression: y = ax + b.\n Confidence 1-alpha = ", 1-P,"\n \n"),outer=TRUE,cex=1.5)
  mtext(paste("\n \n a = ",signif(reg$coefficients[2],2), ", Interval [",
              signif(b[2,1],2),",",signif(b[2,2],2),"]",
              ", p = ", signif(s$coefficients[2,4],2),
              "\n b = ",signif(reg$coefficients[1],2), ", Interval [",
              signif(b[1,1],2),",",signif(b[1,2],2),"]",
              ", p = ", signif(s$coefficients[1,4],2),
              "\n adjusted R^2 = ", signif(s$adj.r.squared,2)),
        outer = TRUE)

  # mtext(paste("\n a = ",reg$coefficients[2]), ", p = ", s$coefficients[2,4],
  #             "\n b = ",reg$coefficients[1]), ", p = ", s$coefficients[1,4]),
  #             outer = TRUE)


  openGraph()
  par(mfrow=c(1,2), oma=c(0,0,3,0))
  plot(reg$fitted,rstandard(reg),main="std. Residuals vs. Fitted",xlab="Fitted Values",ylab="Standardized Residuals")
  abline(h=0,col=1,lwd=2)

  qqnorm(rstandard(reg),ylab="Sample Quantiles of Std. Residuals")
  qqline(rstandard(reg),col="red",lwd=2)

  KS = ks.test(rstandard(reg),pnorm,mean(rstandard(reg)),sd(rstandard(reg)))
  p_KS = signif(KS$p.value,2)
  SH = shapiro.test(rstandard(reg))
  p_SH = signif(SH$p.value,2)

  mtext(paste("Residual Analysis\n Shapiro-Wilk: P = ",p_SH,
              "\n Kolmogorov-Smirnoff: P = ",p_KS ),
        outer = TRUE)

}




###### Two-Sample t-Test ###############################
