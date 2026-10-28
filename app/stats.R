# All inferential calculations are carried out in R.
mean_result <- function(n,mean,sd,mu,sigma,method,alpha,tail) {
 se<-if(method=='z')sigma/sqrt(n) else sd/sqrt(n)
 cv<-if(method=='z')qnorm(1-alpha/(if(tail=='two')2 else 1)) else qt(1-alpha/(if(tail=='two')2 else 1),n-1)
 ci_cv<-if(method=='z')qnorm(1-alpha/2) else qt(1-alpha/2,n-1)
 stat<-(mean-mu)/se;cdf<-if(method=='z')pnorm else function(q)pt(q,n-1)
 p<-if(tail=='two')2*cdf(-abs(stat)) else if(tail=='left')cdf(stat) else cdf(-stat)
 data.frame(method,n,mean,sd,mu,sigma,se,critical=cv,ci_critical=ci_cv,stat,df=if(method=='t')n-1 else NA,
  lower=mean-ci_cv*se,upper=mean+ci_cv*se,p,reject=p<alpha,excluded=mu<mean-ci_cv*se||mu>mean+ci_cv*se)
}
prop_result <- function(x,n,p0,alpha,tail) {
 phat<-x/n;se<-sqrt(p0*(1-p0)/n);stat<-(phat-p0)/se
 p<-if(tail=='two')2*pnorm(-abs(stat)) else if(tail=='left')pnorm(stat) else pnorm(-stat)
 z<-qnorm(1-alpha/2);den<-1+z*z/n;center<-(phat+z*z/(2*n))/den;half<-z*sqrt(phat*(1-phat)/n+z*z/(4*n*n))/den
 data.frame(method='z',n,mean=phat,sd=NA,mu=p0,sigma=NA,se,critical=qnorm(1-alpha/(if(tail=='two')2 else 1)),ci_critical=z,stat,df=NA,
  lower=center-half,upper=center+half,p,reject=p<alpha,excluded=p0<center-half||p0>center+half,approxOK=n*p0>=5&&n*(1-p0)>=5)
}
chi_result <- function(counts,mode='independence',selected=1) {
 if(mode=='independence') {
  ch<-suppressWarnings(chisq.test(counts,correct=FALSE));obs<-counts
 } else {
  obs<-matrix(counts[selected,],nrow=1)
  ch<-suppressWarnings(chisq.test(as.numeric(obs),p=colSums(counts)/sum(counts)))
 }
 expected<-matrix(ch$expected,nrow=nrow(obs));contrib<-(obs-expected)^2/expected
 list(obs=obs,expected=expected,contrib=contrib,stat=unname(ch$statistic),df=unname(ch$parameter),p=ch$p.value,
  small=sum(expected<5),tiny=sum(expected<1),cells=length(obs),mode=mode)
}
esc <- function(x) {x<-gsub('&','&amp;',as.character(x),fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);gsub('>','&gt;',x,fixed=TRUE)}
fmt <- function(x,d=3) ifelse(is.finite(x),formatC(x,digits=d,format='f'),'—')
p_fmt <- function(x) ifelse(x<.0001,'< .0001',formatC(x,digits=4,format='f'))
svg_text <- function(x,y,label,col='#334155',size=13,anchor='middle') sprintf('<text x="%.2f" y="%.2f" fill="%s" font-size="%s" text-anchor="%s">%s</text>',x,y,col,size,anchor,esc(label))
ci_svg <- function(a,names_z,prop=FALSE,selected=1) {
 ref<-a$mu[1];sigma<-a$sigma[1];rg<-range(a$lower,a$upper,ref);pad<-diff(rg)*.18;if(pad==0)pad<-.1;rg<-rg+c(-pad,pad)
 X<-function(x)160+650*(x-rg[1])/diff(rg);pair<-length(unique(a$method))>1;step<-if(pair)86 else 66;H<-if(pair)1180 else 940
 out<-c(sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 960 %s" role="img" aria-label="各星座雙側信賴區間" style="width:100%%;min-width:650px;font-family:Microsoft JhengHei,Arial,sans-serif"><rect width="960" height="%s" fill="white"/>',H,H))
 if(!prop){ks<-pretty((rg-ref)/sigma,n=5);ticks<-ref+ks*sigma;labs<-vapply(ks,function(k)if(abs(k)<1e-10)'μ' else paste0('μ',if(k<0)'−' else '+',format(abs(k),trim=TRUE),'σ'),character(1))}else{ticks<-pretty(rg,n=5);labs<-paste0(round(ticks*100),'%')}
 for(j in seq_along(ticks))if(ticks[j]>=rg[1]&&ticks[j]<=rg[2])out<-c(out,sprintf('<path d="M%.2f 32 V%s" stroke="#e2e8f0"/>',X(ticks[j]),H-70),svg_text(X(ticks[j]),H-44,labs[j]),if(!prop)svg_text(X(ticks[j]),H-25,fmt(ticks[j]),'#64748b',11))
 out<-c(out,sprintf('<path d="M%.2f 32 V%s" stroke="black" stroke-width="1.7"/>',X(ref),H-70),svg_text(X(ref),18,paste(if(prop)'p₀ =' else 'μ =',fmt(ref,4)),'#000',14))
 for(i in 1:12){r<-a[a$z==i,];y<-62+(i-1)*step
  out<-c(out,sprintf('<g><rect x="0" y="%s" width="950" height="%s" rx="5" fill="%s"/>',y-20,step-6,if(i==selected)'#f0f7fa' else 'transparent'),svg_text(7,y+12,paste0(i,'. ',names_z[i]),size=14,anchor='start'),svg_text(7,y+32,paste('n =',r$n[1]),'#64748b',11,'start'))
  for(j in seq_len(nrow(r))){b<-r[j,];yy<-y+(j-1)*26;col<-if(b$excluded)'#c32d3b' else if(b$method=='z')'#2563eb' else '#0891b2';l<-X(b$lower);u<-X(b$upper);m<-X(b$mean)
   out<-c(out,sprintf('<path d="M%.2f %s H%.2f" stroke="%s" stroke-width="2.5" %s/><path d="M%.2f %s V%s M%.2f %s V%s" stroke="%s" stroke-width="2"/>',l,yy,u,col,if(b$method=='t')'stroke-dasharray="6 4"' else '',l,yy-4,yy+4,u,yy-4,yy+4,col),
    if(b$method=='z')sprintf('<circle cx="%.2f" cy="%s" r="3.5" fill="%s"/>',m,yy,col) else sprintf('<polygon points="%.2f,%s %.2f,%s %.2f,%s %.2f,%s" fill="%s"/>',m,yy-4,m+4,yy,m,yy+4,m-4,yy,col),
    svg_text(l-7,yy+4,if(prop)paste0(fmt(b$lower*100,2),'%') else fmt(b$lower),col,11,'end'),svg_text(u+7,yy+4,if(prop)paste0(fmt(b$upper*100,2),'%') else fmt(b$upper),col,11,'start'))}
  out<-c(out,svg_text(X(r$mean[1]),y+if(pair)49 else 25,if(prop)paste0(fmt(r$mean[1]*100,2),'%') else fmt(r$mean[1]),size=11),'</g>')}
 out<-c(out,svg_text(480,H-7,if(prop)'類別比例（百分比）' else 'μ ± kσ：母體位置刻度；下排為原始數值（非 z／t-statistic）','#536c76',11),'</svg>');paste(out,collapse='')
}
distribution_svg <- function(stat,critical,df=NA,tail='two',alpha=.05,p=.05,method='z',pview=FALSE,df2=NULL) {
 ischi<-method=='chi';isF<-method=='F';positive<-ischi||isF
 pdf<-if(isF)function(x)df(x,df,df2) else if(ischi)function(x)dchisq(x,df) else if(method=='t')function(x)dt(x,df) else dnorm
 hi<-if(isF)max(qf(.999,df,df2),stat*1.15,critical*1.6) else if(ischi)max(critical*1.6,stat*1.15,df+5*sqrt(2*df),6) else max(4.2,abs(stat)+.8,critical+.8);lo<-if(positive)0 else -hi
 xx<-seq(lo,hi,length.out=1001);density<-pdf(if(positive)pmax(xx,.001) else xx);ymax<-max(density[is.finite(density)]);X<-function(x)56+454*(x-lo)/(hi-lo);Y<-function(y)232-155*y/ymax
 extreme<-if(pview){if(tail=='two')abs(xx)>=abs(stat) else if(tail=='left')xx<=stat else xx>=stat}else{if(tail=='two')abs(xx)>=critical else if(tail=='left')xx<= -critical else xx>=critical}
 col<-if(pview)'#169ac2' else '#d48226';label<-if(isF)'F 檢定量（F-statistic）' else if(ischi)'χ² 檢定量（chi-square statistic）' else paste0(method,' 檢定量（',method,'-statistic）')
 out<-c('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 560 355" role="img" style="width:100%;font-family:Microsoft JhengHei,Arial,sans-serif"><rect width="560" height="355" fill="white"/>',svg_text(280,27,paste0('H₀ 下的 ',if(ischi)'χ²' else method,' 分配',if(isF)paste0('（df₁=',df,'；df₂=',fmt(df2,1),'）') else if(is.finite(df))paste0('（df = ',df,'）') else ''),size=14))
 run<-rle(extreme);end<-cumsum(run$lengths);start<-end-run$lengths+1
 for(i in which(run$values)){ids<-start[i]:end[i];points<-paste(sprintf('%.2f,%.2f',X(xx[ids]),Y(density[ids])),collapse=' ');out<-c(out,sprintf('<polygon points="%.2f,232 %s %.2f,232" fill="%s" fill-opacity=".3"/>',X(xx[ids[1]]),points,X(xx[ids[length(ids)]]),col))}
 out<-c(out,sprintf('<polyline points="%s" fill="none" stroke="#344b58" stroke-width="2"/><path d="M56 232 H510" stroke="#344b58"/>',paste(sprintf('%.2f,%.2f',X(xx),Y(density)),collapse=' ')))
 for(tick in pretty(c(lo,hi),n=6))if(tick>=lo&&tick<=hi)out<-c(out,sprintf('<path d="M%.2f 232 V237" stroke="#344b58"/>',X(tick)),svg_text(X(tick),254,format(tick,trim=TRUE),'#526b76',11))
 cvs<-if(tail=='two')c(-critical,critical) else if(tail=='left')-critical else critical
 for(cv in cvs)out<-c(out,sprintf('<path d="M%.2f 70 V232" stroke="#cb7017" stroke-width="1.5" stroke-dasharray="5 4"/>',X(cv)),svg_text(X(cv),64,fmt(cv),'#cb7017',12))
 tailprob<-if(tail=='two')alpha/2 else alpha
 cv_name<-if(isF)paste0('Fcv(',format(tailprob,trim=TRUE),'; ',df,', ',fmt(df2,2),')') else if(ischi)paste0('χ²cv(',format(tailprob,trim=TRUE),', ',df,')') else if(method=='t')paste0('tcv(',format(tailprob,trim=TRUE),', ',df,')') else paste0('zcv(',format(tailprob,trim=TRUE),')')
 cv_label<-paste0(cv_name,' = ',if(tail=='two')'±' else if(tail=='left')'−' else '',fmt(critical),'；α = ',alpha)
 obs_label<-paste0(if(ischi)'χ²' else method,' = ',fmt(stat),', p = ',p_fmt(p))
 obs_x<-max(135,min(425,X(stat)))
 out<-c(out,sprintf('<path d="M%.2f 80 V232 M%.2f 290 V299" stroke="black" stroke-width="1.8"/>',X(stat),X(stat)),svg_text(280,278,cv_label,'#cb7017',12),svg_text(obs_x,315,obs_label,'#000',13),svg_text(280,343,label,size=12),'</svg>');paste(out,collapse='')
}
html_table <- function(a,reject=NULL) {
 heads<-paste0('<th>',esc(names(a)),'</th>',collapse='');rows<-vapply(seq_len(nrow(a)),function(i)paste0('<tr',if(!is.null(reject)&&reject[i])' class="reject"' else '', '>',paste0('<td>',esc(unlist(a[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1))
 paste0('<div class="table-scroll"><table><thead><tr>',heads,'</tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div>')
}

