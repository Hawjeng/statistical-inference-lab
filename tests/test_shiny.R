library(shiny)
app<-source('app.R',encoding='UTF-8')$value
mock<-MockShinySession$new()
csv_download<-NULL
mock$sendCustomMessage<-function(type,message){
 if(type=='download-file')csv_download<<-message
}
testServer(app,session=mock,{
 session$setInputs(example='Ch09p217ex9.2',ba=.05,bmu=8,bsigma=.7,bm='z',bt='left',bn=10,
  kind='numeric',variable='B5',alpha=.05,zodiac='1',tail='two',method='t')
 session$flushReact()
 stopifnot(abs(book_test()$stat[1]+4.2857142857)<1e-8)
 a<-results();stopifnot(nrow(a)==24,abs(a$p[2]-.04338376718)<1e-8)
 h<-output$survey_chart;stopifnot(grepl('μ',h$html),!grepl('母體位置刻度',h$html,fixed=TRUE),grepl('1. 水瓶座',h$html,fixed=TRUE))
 ci0<-a[,c('lower','upper')];session$setInputs(tail='left');session$flushReact();stopifnot(identical(ci0,results()[,c('lower','upper')]))
 stopifnot(grepl('H₀：μ ≥',output$survey_diagram$html,fixed=TRUE),grepl('z-test',output$survey_diagram$html,fixed=TRUE),grepl('t-test',output$survey_diagram$html,fixed=TRUE))
 session$setInputs(tail='right');session$flushReact();stopifnot(grepl('H₀：μ ≤',output$survey_diagram$html,fixed=TRUE))
 session$setInputs(kind='category',variable='PhoneBrand',event='1');session$flushReact()
 stopifnot(ncol(counts())==2,sum(counts())==706,nrow(results())==12)
 aries<-results()[3,]
 stopifnot(round(aries$ci_se,6)==.059697,round(aries$se,6)==.061598,
  round(aries$lower,6)==.670230,round(aries$upper,6)==.904238)
 session$setInputs(save_csv=1)
 stopifnot(identical(csv_download$name,'PhoneBrand-results.csv'))
 tab<-read.csv(text=sub('^\\ufeff','',csv_download$content),check.names=FALSE)
 stopifnot(round(tab[['信賴區間標準誤']][3],6)==.059697,round(tab[['假設檢定標準誤']][3],6)==.061598)
 session$setInputs(variable='DataPlan');session$flushReact()
 stopifnot(ncol(counts())==2,sum(counts())==710,nrow(results())==12)
 session$setInputs(example='Ch08p166t8.1',bn=1000);session$flushReact();stopifnot(grepl('配置樣本數',output$book_result$html))
 session$setInputs(example='Ch09p210f9.1');session$flushReact();stopifnot(grepl('1.9600',output$book_result$html,fixed=TRUE))
 age_items<-Filter(function(a)grepl('研究生年齡',a$title),examples)
 stopifnot(length(age_items)==4,all(vapply(age_items,function(a)a$mu==23,logical(1))))
 session$setInputs(example='Ch09p229age',bmu=23,bsigma=2,bm='z',bt='two');session$flushReact()
 r<-book_test();stopifnot(abs(r$stat[1]-2.5)<1e-10,abs(r$p[1]-.012419330651552)<1e-10,
  abs(r$stat[2]-1.9999916667188)<1e-10,abs(r$p[2]-.05694080801679)<1e-10)
 stopifnot(grepl('z_obt',output$book_result$html),grepl('z_obt',output$book_diagram$html))
 stopifnot(length(gregexpr('<svg ',output$book_diagram$html,fixed=TRUE)[[1]])==1,!grepl('t_obt',output$book_diagram$html,fixed=TRUE))
 session$setInputs(bm='t');session$flushReact();stopifnot(grepl('t_obt',output$book_diagram$html,fixed=TRUE),!grepl('z_obt',output$book_diagram$html,fixed=TRUE));session$setInputs(bm='z');session$flushReact()
 stopifnot(grepl('H₀：μ = 23',output$book_diagram$html,fixed=TRUE),!grepl('事先設定的 α',output$book_diagram$html,fixed=TRUE))
 svg<-distribution_svg(-2.053860157,qt(.975,77),77,'two',.05,.043383767,'t',TRUE)
 stopifnot(grepl('t_obt = -2.054, p = 0.0434',svg,fixed=TRUE),grepl('tcv(0.025, 77)',svg,fixed=TRUE))
 stopifnot(grepl('alpha-hatch',svg,fixed=TRUE),grepl('#169ac2',svg,fixed=TRUE),!grepl('檢定量（',svg,fixed=TRUE))
 stopifnot(grepl('stroke="#7c3aed"',svg,fixed=TRUE),grepl('fill="#7c3aed"',svg,fixed=TRUE))
 ci<-ci_svg(results(),z_names,prop=TRUE,selected=1)
 stopifnot(grepl('stroke="black"',ci,fixed=TRUE))
 session$setInputs(example='Ch08p179pitcher',book_enable_test=FALSE,bmu=NA_real_,bsigma=10,bm='z',ba=.05,bt='two');session$flushReact()
 r<-book_test();stopifnot(!book_testing(),all(is.na(r$stat)),all(is.na(r$p)),r$n[1]==30,r$mean[1]==115)
 stopifnot(abs(r$sd[1]-8.99808408725876)<1e-10,
  abs(r$lower[1]-(115-qnorm(.975)*10/sqrt(30)))<1e-10,
  !grepl('z_obt =',output$book_result$html,fixed=TRUE),!grepl('H₀：μ',output$book_result$html,fixed=TRUE),
  grepl('雙側信賴區間估計圖',output$book_diagram$html,fixed=TRUE),grepl('s ≈ 9',output$book_result$html,fixed=TRUE))
 session$setInputs(example='Ch08p183pitcher',book_enable_test=FALSE,bmu=NA_real_,bm='t');session$flushReact()
 r<-book_test();stopifnot(abs(r$lower[2]-111.64006018275217)<1e-10,abs(r$upper[2]-118.35993981724783)<1e-10)
 session$setInputs(book_enable_test=TRUE,bmu=120);session$flushReact()
 stopifnot(book_testing(),abs(book_test()$stat[2]-(115-120)/(8.99808408725876/sqrt(30)))<1e-10,
  grepl('t_obt',output$book_diagram$html,fixed=TRUE))
})
cat('PASS Shiny reactive tests: textbook, numeric CI, tail changes, binary categories and sampling.\n')
