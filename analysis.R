# 區間估計與假設檢定：課本範例與十二星座
# 在 RStudio 開啟本檔，將工作目錄設為專案資料夾後 Source。
# 所有區間均為雙側 (1-alpha) CI；尾向只影響假設檢定。
# 以 Base R 即可重現網站平均數、比例及卡方近似檢定。
suppressWarnings(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
alpha <- .05
tail <- 'two' # 'two'、'left'、'right'
if(!file.exists('data/numeric-summary.csv')) stop('請先把工作目錄設為本專案資料夾')
num <- read.csv('data/numeric-summary.csv',fileEncoding='UTF-8-BOM')
freq <- read.csv('data/category-counts.csv',fileEncoding='UTF-8-BOM')
names_z <- c('水瓶座','雙魚座','牡羊座','金牛座','雙子座','巨蟹座','獅子座','處女座','天秤座','天蠍座','射手座','摩羯座')
p_tail <- function(stat,cdf,tail) {
 if(tail=='left') cdf(stat) else if(tail=='right') cdf(-stat) else 2*cdf(-abs(stat))
}
mean_test <- function(n,xbar,s,mu0,sigma,method='t',alpha=.05,tail='two') {
 se <- if(method=='z') sigma/sqrt(n) else s/sqrt(n)
 ci_cv <- if(method=='z') qnorm(1-alpha/2) else qt(1-alpha/2,n-1)
 cv <- if(method=='z') qnorm(1-alpha/if(tail=='two')2 else 1) else qt(1-alpha/if(tail=='two')2 else 1,n-1)
 stat <- (xbar-mu0)/se
 cdf <- if(method=='z') pnorm else function(q)pt(q,n-1)
 p <- p_tail(stat,cdf,tail)
 data.frame(method,n,xbar,s,mu0,sigma,se,ci_cv,lower=xbar-ci_cv*se,upper=xbar+ci_cv*se,
  statistic=stat,df=if(method=='t')n-1 else NA,critical=cv,p,decision=if(p<alpha)'拒絕 H0' else '保留 H0')
}
numeric_results <- do.call(rbind,lapply(seq_len(nrow(num)),function(i) {
 a<-num[i,]
 do.call(rbind,lapply(c('z','t'),function(m) cbind(variable=a$variable,星座=names_z[a$z],
  mean_test(a$n,a$mean,a$sd,a$mu,a$sigma,m,alpha,tail))))
}))
print(numeric_results,row.names=FALSE)

# 課本數值範例：睡眠時間的摘要由原始 36 筆資料重新計算。
# 已知 sigma=.7，參考 mu0=8；檢查同樣資料下 z 與 t 的不同。
sleep_example <- rbind(mean_test(36,7.5,1,8,.7,'z',alpha,tail),
                       mean_test(36,7.5,1,8,.7,'t',alpha,tail))
cat('\n課本範例：睡眠時間\n');print(sleep_example,row.names=FALSE)

# 類別資料：指定成功事件；Wilson 雙側區間與未修正比例 z 檢定。
proportion_test <- function(x,n,p0,alpha=.05,tail='two') {
 phat<-x/n;se<-sqrt(p0*(1-p0)/n);stat<-(phat-p0)/se;z<-qnorm(1-alpha/2)
 den<-1+z^2/n;center<-(phat+z^2/(2*n))/den
 half<-z*sqrt(phat*(1-phat)/n+z^2/(4*n^2))/den
 p<-p_tail(stat,pnorm,tail)
 data.frame(x,n,phat,p0,se,lower=center-half,upper=center+half,statistic=stat,p,
  decision=if(p<alpha)'拒絕 H0' else '保留 H0',approxOK=n*p0>=5&&n*(1-p0)>=5)
}
category_variable <- 'PhoneBrand'
a<-subset(freq,variable==category_variable)
m<-xtabs(count~z+category,data=a)
p0<-sum(m[,1])/sum(m)
proportion_results<-do.call(rbind,lapply(1:12,function(i)cbind(星座=names_z[i],
 proportion_test(m[i,1],sum(m[i,]),p0,alpha,tail))))
cat('\n比例 z：成功事件 =',colnames(m)[1],'\n');print(proportion_results,row.names=FALSE)

# 卡方獨立性檢定、所選星座對全體比例的適合度示範。
cat('\n卡方獨立性檢定\n');chi_independence<-chisq.test(m,correct=FALSE);print(chi_independence)
cat('\n卡方適合度檢定（水瓶座 vs 全體比例）\n')
chi_gof<-chisq.test(m[1,],p=colSums(m)/sum(m));print(chi_gof)
cat('\n期望次數 E<5 的格子：',sum(chi_independence$expected<5),'/',length(m),'\n')
# 稀疏表：R 的模擬法也可以參考；結果與網站使用不同亂數演算法，不需逐位相同。
# set.seed(20261028);chisq.test(m,simulate.p.value=TRUE,B=5000)
# set.seed(20261028);chisq.test(m[1,],p=colSums(m)/sum(m),simulate.p.value=TRUE,B=5000)

# R 圖：黑色 mu 實線；每星座成對 z/t；端點在外側；sigma 位置刻度。
plot_ci <- function(variable='B5') {
 a<-num[num$variable==variable,];yy<-12:1
 z<-do.call(rbind,lapply(1:12,function(i)mean_test(a$n[i],a$mean[i],a$sd[i],a$mu[i],a$sigma[i],'z',alpha,tail)))
 t<-do.call(rbind,lapply(1:12,function(i)mean_test(a$n[i],a$mean[i],a$sd[i],a$mu[i],a$sigma[i],'t',alpha,tail)))
 if(.Platform$OS.type=='windows') windowsFonts(JhengHei=windowsFont('Microsoft JhengHei'))
 old<-par(no.readonly=TRUE);on.exit({layout(1);par(old)})
 layout(matrix(1:2,ncol=1),heights=c(1.2,10));par(mar=rep(0,4),family=if(.Platform$OS.type=='windows')'JhengHei' else 'sans')
 plot.new();plot.window(c(0,1),c(0,1));text(.5,.75,paste(variable,a$label[1],'：區間估計'),font=2)
 legend(.5,.3,xjust=.5,horiz=TRUE,bty='n',legend=c('z','t'),col=c('#2563eb','#0891b2'),lty=c(1,2),pch=c(19,18))
 par(mar=c(5.5,7,.5,2));rg<-range(z$lower,z$upper,t$lower,t$upper);pad<-diff(rg)*.2
 plot(a$mean,yy,type='n',xlim=rg+c(-pad,pad),ylim=c(.2,12.65),xaxt='n',yaxt='n',xlab='μ ± kσ（下排為原始數值）',ylab='')
 axis(2,at=yy,labels=paste0(1:12,'. ',names_z,'\nn = ',a$n),las=1,cex.axis=.65,gap.axis=-1,tick=FALSE)
 ks<-pretty((par('usr')[1:2]-a$mu[1])/a$sigma[1],5);xx<-a$mu[1]+ks*a$sigma[1]
 labs<-vapply(ks,function(k)if(abs(k)<1e-10)'μ' else paste0('μ',if(k<0)'−' else '+',abs(k),'σ'),character(1))
 axis(1,at=xx,labels=paste(labs,round(xx,3),sep='\n'),cex.axis=.65,gap.axis=-1)
 abline(v=a$mu[1],col='black',lwd=2)
 for(j in 1:2){b<-if(j==1)z else t;y<-yy+if(j==1).14 else -.14
  color<-ifelse(b$lower>a$mu|b$upper<a$mu,'#c32d3b',if(j==1)'#2563eb' else '#0891b2')
  segments(b$lower,y,b$upper,y,col=color,lty=j,lwd=2);points(b$xbar,y,col=color,pch=if(j==1)19 else 18)
  text(b$lower-diff(rg)*.01,y,format(round(b$lower,3),nsmall=3),adj=c(1,.5),cex=.6,col=color)
  text(b$upper+diff(rg)*.01,y,format(round(b$upper,3),nsmall=3),adj=c(0,.5),cex=.6,col=color)}
 text(a$mean,yy-.45,format(round(a$mean,3),nsmall=3),cex=.6)
}
if(!interactive())png('confidence-intervals.png',width=1400,height=1400,res=120)
plot_ci('B5')
if(!interactive())dev.off()
write.csv(numeric_results,'numeric-results.csv',row.names=FALSE,fileEncoding='UTF-8')
cat('\n資料以星座分組，並非隨機分配；全體包含各組，μ、σ 與 p0 在本習作視為已知參考。\n')
cat('保留 H0 不代表證明相等。未作有限母體與多重比較修正。\n')
if(interactive())View(numeric_results,title='區間估計與假設檢定')
