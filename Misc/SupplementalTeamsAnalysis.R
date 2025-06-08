library(ggplot2)

process(data=teams, cov=c("d1","d2","d3"), x="dysfunc", m="negtone", y="perform", 
        w="negexp", model=14, plot=1, wmodval = -.2045767, seed = 42517)


result<-process(data=teams,y="perform",x="dysfunc",m="negtone",
                w="negexp", cov = c("d1", "d2", "d3"), model=14,save=3,seed=42517)
boots<-result[[1]]
poutput<-result[[2]]
modvalt<-teams$negexp
minmod<-min(modvalt)
maxmod<-max(modvalt)
modval<-matrix(seq(minmod,maxmod,((maxmod-minmod)/10000)))
llci<-modval
ulci<-llci
jnfind<-matrix(999999)
effect<-poutput[3,1]*(poutput[10,1]+poutput[12,1]*modval) #change to match appropriate columns
for (i in (1:length(modval)))
{
bootind<-boots$col2*(boots$col8+boots$col10*modval[i]) #change to match appropriate columns
llci[i,]<-matrix(quantile(bootind,.025))
ulci[i,]<-matrix(quantile(bootind,.975))
}
chk<-sign(llci*ulci)
for (i in (1:(length(modval)-1)))
{
  chk2<-sign(chk[i]*chk[(i+1)])
  if (chk2 < 0)
{
vala<-ulci[i];valb<-ulci[i+1]
chk3<-sign(llci[i]*llci[i+1])
if (chk3 < 1)
{
vala<-llci[i];valb<-llci[i+1]
}
wgt1<-(1-(abs(vala)/abs(vala-valb)))
wgt2<-(1-(abs(valb)/abs(vala-valb)))
jnfind<-cbind(jnfind,(wgt1*modval[i]+wgt2*modval[i+1]))
}
}
plot(x=modval,y=effect,type="l",pch=19,lwd=3,
        xlim=c(min(modval),max(modval)),ylim=c(min(llci),max(ulci)),
        ylab="Conditional indirect effect of dysfunctional behavior",
        xlab="Team expressivity (W)")
points(modval,llci,lwd=2,lty=2,type="l",col="black")
points(modval,ulci,lwd=2,lty=2,type="l",col="black")
abline(h=0,untf = FALSE,lty=3,lwd=1,col="red")
if (length(jnfind) > 1)
{
jnfind<-jnfind[2:length(jnfind)]
for (i in (1:length(jnfind)))
{
abline(v=jnfind[i],untf=FALSE,lty=3,lwd=1)
text(jnfind[i],min(llci),format(jnfind[i]),cex=0.8)
}
}
print(jnfind)

process(data=teams, cov=c("d1","d2","d3"), x="dysfunc", m="negtone", y="perform", 
        w="negexp", boot=1000, model=15, plot=1, moments =1, intprobe=1, jn=1)


#dysfunc    negexp   perform
#-0.3370   -0.5520   -0.1613
#0.0347   -0.5520   -0.0016
#0.4063   -0.5520    0.1582
#-0.3370   -0.0083   -0.1659
#0.0347   -0.0083   -0.0217
#0.4063   -0.0083    0.1224
#-0.3370    0.5354   -0.1705
#0.0347    0.5354   -0.0419
#0.4063    0.5354    0.0867

x<-c(-0.3370,0.0347,0.4063,-0.3370,0.0347,0.4063,-0.3370,0.0347,0.4063)
w<-c(-.552,-.552,-.552,-.008,-.008,-.008,.535,.535,.535)
y<-c(-0.1613,-0.0016,0.1582,-0.1659,-0.0217, 0.1224,-0.1705,-0.0419,0.0867)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,xlab="Dysfunctional Team Behavior",
     ylab="Team performance")
legend.txt<-c("low expressive","moderate expressive","high expressive")
legend("bottomright", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),pch=c(15,16,17))
lines(x[w==-.552],y[w==-.552],lwd=2)
lines(x[w==-.008],y[w==-.008],lwd=3,lty=3)
lines(x[w==.535],y[w==.535],lwd=2,lty=6)


#Direct and Indirect Error Graph

directindirect <- data.frame(type = c(rep("direct", times = 3), rep("indirect", times = 3)),
                             negexp = rep(c(-0.5520,-0.0083,0.5354),times = 2),
                             effect = c(0.4298,0.3880,0.3461,-0.1617,-0.3014,-0.4411),
                             LLCI = c(-0.1147,0.0064,-0.0660,-0.4773,-0.5415,-0.7172),
                             ULCI = c(0.9743,0.7695,0.7583,0.2007,-0.0591,-0.1278))

# Default line plot
ggplot(directindirect, aes(x=negexp, y=effect, group=type, color=type)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=LLCI, ymax=ULCI), width=.2)+
  theme_classic()+
  labs(x="Negative Nonverbal Expressivity (W)", y = "Effect of Dysfunctional Behavior on Performance", group = "", color = "")+
  scale_color_manual(values=c('#111111','#E31C25'))+
  theme(legend.position="top")+
  geom_hline(yintercept = 0, linetype = "dashed")
