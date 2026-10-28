library(shiny)
# JSON uses explicit namespace to avoid masking shiny::validate.
source('stats.R',encoding='UTF-8',local=TRUE)
d<-jsonlite::fromJSON('data/summary.json',simplifyVector=FALSE)
examples<-jsonlite::fromJSON('data/textbook.json',simplifyVector=FALSE)
z_names<-unlist(d$zodiacNames)
choice_labels<-function(x)setNames(names(x),vapply(x,function(a)a$label,character(1)))
example_choices<-setNames(vapply(examples,function(a)a$id,character(1)),vapply(examples,function(a)a$title,character(1)))
# Native selects work consistently on desktop, mobile and WebR.
selectInput<-function(...)shiny::selectInput(...,selectize=FALSE)
tails<-c('雙尾 ≠'='two','左尾 <'='left','右尾 >'='right')
card<-function(...,class='')div(class=paste('lab-card',class),...)
note<-function(...)p(class='hint',...)
diagram_ui<-function(r,alpha,tail,method,df2=NULL) {
 cv<-if(method=='chi')qchisq(1-alpha,r$df) else if(method=='F')qf(1-alpha,r$df,df2) else r$critical
 fluidRow(column(6,h4('拒絕域：事先設定的 α'),HTML(distribution_svg(r$stat,cv,r$df,tail,alpha,r$p,method,FALSE,df2))),
  column(6,h4('尾端機率：資料得到的 p-value'),HTML(distribution_svg(r$stat,cv,r$df,tail,alpha,r$p,method,TRUE,df2))))
}
hypotheses<-function(tail,mu,param='μ')HTML(paste0('<div class="hypothesis">H₀：',param,if(tail=='two')' = ' else if(tail=='left')' ≥ ' else ' ≤ ',fmt(mu,4),'<br>H₁：',param,if(tail=='two')' ≠ ' else if(tail=='left')' &lt; ' else ' &gt; ',fmt(mu,4),'</div>'))
result_table<-function(a) {
 data.frame(方法=a$method,n=a$n,平均數=fmt(a$mean,4),SE=fmt(a$se,4),CI下限=fmt(a$lower,4),CI上限=fmt(a$upper,4),
  `觀察檢定量（z_obt／t_obt）`=fmt(a$stat,4),df=ifelse(is.na(a$df),'—',a$df),臨界值=fmt(a$critical,4),p值=p_fmt(a$p),結論=ifelse(a$reject,'拒絕 H₀','保留 H₀'),check.names=FALSE)
}
ui<-fluidPage(tags$head(tags$title('區間估計與假設檢定｜邱皓政統計互動教材'),tags$link(rel='stylesheet',href='style.css'),
 tags$script(HTML("function reportHeight(){parent.postMessage({type:'inference-height',height:document.documentElement.scrollHeight},location.origin)};new ResizeObserver(reportHeight).observe(document.documentElement);setInterval(reportHeight,1500);"))),
 div(class='app-top',a(href='https://hawjeng.github.io/resources.html','← 教學與專案'),a(href='https://github.com/Hawjeng/statistical-inference-lab',target='_blank','GitHub 原始碼 ↗')),
 div(class='app-title',p(class='eyebrow','STATISTICS LAB · CONFIDENCE INTERVALS & HYPOTHESIS TESTING'),h1('區間估計與假設檢定'),p('原理說明、課本範例，再以星座調查資料練習。')),
 tabsetPanel(id='main_tab',selected='課本範例',
 tabPanel('原理說明',includeHTML('www/lesson.html')),
 tabPanel('課本範例',
  card(h3('從課本的問題開始'),p('不依章次，以問題主題選擇範例；可下載 14 份對應 Excel 原檔。'),
   selectInput('example','範例主題',example_choices,width='100%'),
   div(style='display:none',textOutput('book_type')),
   conditionalPanel("output.book_type == 'mean' || output.book_type == 'critical'",numericInput('ba','顯著水準 α',.05,min=.001,max=.2,step=.001)),
   conditionalPanel("output.book_type == 'mean'",fluidRow(column(3,selectInput('bm','檢定方式',c('z：σ 已知'='z','t：σ 未知'='t'))),column(3,selectInput('bt','檢定方向',tails)),column(3,numericInput('bmu','假設參考值 μ₀',115)),column(3,numericInput('bsigma','已知母體標準差 σ',10,min=.0001)))),
   conditionalPanel("output.book_type == 'sampling' || output.book_type == 'stratified'",fluidRow(column(6,numericInput('bn','抽樣數 n',10,min=1)),column(6,actionButton('resample','重新抽樣／配置',class='btn-primary')))),
   uiOutput('book_question'),uiOutput('book_result'),uiOutput('book_download')),
  card(h3('檢定示意圖'),uiOutput('book_diagram'),note('CI 一律保持雙側 (1−α) 區間；檢定圖的橫軸明確標示 z-statistic／t-statistic。'))),
 tabPanel('星座範例',
  card(h3('逐一星座，與十二星座整體比較'),
   fluidRow(column(4,selectInput('scope','示範問題',c('逐一星座 vs 全體參考值'='single','十二星座整體比較'='overall'))),
    column(4,selectInput('kind','變數類型',c('數值變數（平均數）'='numeric','類別／二分變數'='category'))),column(4,selectInput('variable','分析變數',choice_labels(d$numeric),selected='B5'))),
   fluidRow(column(3,numericInput('alpha','顯著水準 α',.05,min=.001,max=.2,step=.001)),column(3,selectInput('zodiac','選擇星座',setNames(as.character(1:12),paste0(1:12,'. ',z_names)))),
    column(3,conditionalPanel("input.scope == 'single' && (input.kind == 'numeric' || input.category_method == 'proportion')",selectInput('tail','檢定方向',tails))),column(3,conditionalPanel("input.scope == 'single' && input.kind == 'numeric'",selectInput('method','檢定圖分配',c('t-statistic'='t','z-statistic'='z'))))),
   conditionalPanel("input.kind == 'category' && input.scope == 'single'",fluidRow(column(6,selectInput('category_method','類別示範',c('指定事件的單一比例 z'='proportion','整個類別分布的卡方適合度'='gof'))),column(6,conditionalPanel("input.category_method == 'proportion'",uiOutput('category_choice'))))),
   conditionalPanel("input.kind == 'numeric' && input.scope == 'overall'",selectInput('anova_method','整體平均數比較',c('Welch ANOVA：不要求各組變異數相等'='welch','一般單因子 ANOVA：等變異數假設'='classic'))),
   uiOutput('population'),uiOutput('scope_note')),
  fluidRow(column(8,card(h3('資料圖與雙側信賴區間'),uiOutput('chart_legend'),div(class='ci-scroll',uiOutput('survey_chart')),conditionalPanel("input.kind == 'numeric' || (input.scope == 'single' && input.category_method == 'proportion')",downloadButton('save_chart','下載圖形 SVG')))),
   column(4,card(h3('假設、檢定與結論'),uiOutput('survey_result'),note('保留 H₀ 表示未能拒絕，不表示證明相等。'),conditionalPanel("input.kind == 'category' && (input.scope == 'overall' || input.category_method == 'gof')",actionButton('mc','Monte Carlo 模擬（5,000 次）',class='btn-primary'),uiOutput('mc_result'))))),
  card(h3('檢定示意圖'),uiOutput('survey_diagram'),note('橘色：α 拒絕域；藍色：p-value 尾端面積。黑線：觀察統計量；橘色虛線：臨界值。')),
  card(h3('完整計算表'),tags$details(tags$summary('展開／收合完整計算與檢定'),uiOutput('survey_table')),downloadButton('save_csv','下載目前結果 CSV'))),
 tabPanel('教材下載與說明',card(h3('語法、資料與教材'),p('完整 Shiny 原始碼與資料放在 GitHub，新專案可下載後在 RStudio 執行。'),
  a(class='btn btn-primary',href='https://github.com/Hawjeng/statistical-inference-lab',target='_blank','查看 GitHub 原始碼'),br(),br(),
  a(href='https://hawjeng.github.io/statistical-inference-lab/downloads/zodiac_ci_calculations.xlsx',target='_blank','四變數 Excel 計算檔 ↓'),br(),
  a(href='https://hawjeng.github.io/statistical-inference-lab/downloads/zodiac_ci_report.html',target='_blank','四變數完整 HTML 報告 ↓'),br(),
  a(href='https://raw.githubusercontent.com/Hawjeng/statistical-inference-lab/main/analysis.R',target='_blank','R 分析語法 ↓'),
  includeHTML('www/limitations.html'))))
)
server<-function(input,output,session) {
 alpha<-reactive({req(input$alpha);validate(need(input$alpha>=.001&&input$alpha<=.2,'α 請輸入 0.001 到 0.20'));input$alpha})
 book<-reactive(examples[[match(input$example,vapply(examples,function(a)a$id,character(1)))]] )
 output$book_type<-renderText(book()$type)
 outputOptions(output,'book_type',suspendWhenHidden=FALSE)
 observeEvent(input$example,{b<-book();if(b$type=='mean'){updateNumericInput(session,'bmu',value=b$mu);updateNumericInput(session,'bsigma',value=b$sigma);updateSelectInput(session,'bm',selected=b$method);updateSelectInput(session,'bt',selected=b$tail)};updateNumericInput(session,'bn',value=if(b$type=='stratified')1000 else 10)},ignoreNULL=FALSE)
 output$book_question<-renderUI({b<-book();tagList(h4(b$title),if(b$type=='mean')note(if(isTRUE(b$estimationOnly))'原檔以估計為主；指定 μ₀ 的假設檢定為延伸練習。' else '先決定單尾或雙尾，再比較觀察差距與標準誤。'),if(grepl('研究生年齡',b$title))note('觀察資料：n=25、x̄=24、s≈2.5；檢定 μ₀ 預設為 23，依所附年齡檢定 Excel 公式；z 另使用已知 σ=2。樣本平均數不是假設參考值。'),note('CI 固定雙側；使用完整精度重新計算，可能與原 Excel 先將標準誤四捨五入的答案略有差異。'))})
 book_test<-reactive({b<-book();req(b$type=='mean',input$ba,input$bmu,input$bsigma);validate(need(input$ba>=.001&&input$ba<=.2,'α 須介於 .001 與 .20'),need(input$bsigma>0,'σ 須大於 0'));do.call(rbind,lapply(c('z','t'),function(m)mean_result(b$n,b$mean,b$sd,input$bmu,input$bsigma,m,input$ba,input$bt)))})
 output$book_result<-renderUI({input$resample;b<-book()
  if(b$type=='mean'){a<-book_test();r<-a[a$method==input$bm,];tagList(div(class='population',paste0('n = ',r$n,'；x̄ = ',fmt(r$mean,4),'；s = ',fmt(r$sd,4),'；σ = ',fmt(r$sigma,4))),hypotheses(input$bt,input$bmu),
   h4('1　標準誤'),p(if(r$method=='z')'SE = σ/√n' else 'SE = s/√n',' = ',fmt(r$se,4)),h4('2　雙側信賴區間'),p(paste0(fmt((1-input$ba)*100,1),'% CI = x̄ ± ',fmt(r$ci_critical,4),' × SE = [',fmt(r$lower,4),', ',fmt(r$upper,4),'] ',b$unit)),
   h4('3　觀察檢定量與決策'),p(paste0(r$method,'_obt = (x̄−μ₀)/SE = ',fmt(r$stat,4),'；p = ',p_fmt(r$p))),note('z_obt／t_obt 是觀察檢定量；zcv／tcv 是檢定臨界值。'),div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀'),
   tags$details(tags$summary('z／t 完整比較表（展開／收合）'),HTML(html_table(result_table(a),a$reject))),tags$details(tags$summary('原始有效數值'),p(paste(fmt(unlist(b$values)),collapse='、'))))
  }else if(b$type=='critical'){as<-c(input$ba,.1,.05,.01,.001);as<-unique(as);HTML(html_table(data.frame(α=as,左尾=fmt(qnorm(as),4),右尾=fmt(qnorm(1-as),4),雙尾=paste0('±',fmt(qnorm(1-as/2),4)))))
  }else if(b$type=='sampling'){ids<-unlist(b$ids);n<-as.integer(input$bn);validate(need(n>=1&&n<=length(ids),paste('樣本數須介於 1 與',length(ids))));k<-length(ids)/n;ss<-ids[floor(runif(1,0,k)+(0:(n-1))*k)+1];tagList(p('名冊 N = ',length(ids),'；抽樣 n = ',n,'；系統間距 N/n = ',fmt(k,2)),h4('簡單隨機抽樣'),p(paste(sample(ids,n),collapse='、')),h4('系統隨機抽樣'),p(paste(ss,collapse='、')),note('編號只用來辨識抽樣單位，不對學號的平均數作檢定。'))
  }else{st<-do.call(rbind,lapply(b$strata,as.data.frame));N<-sum(st$N);n<-as.integer(input$bn);validate(need(n>=1&&n<=N,'抽樣數須為正整數且不超過母體 N'));ex<-n*st$N/N;alloc<-floor(ex);left<-n-sum(alloc);if(left>0){ix<-order(ex-alloc,decreasing=TRUE)[seq_len(left)];alloc[ix]<-alloc[ix]+1};tagList(p('母體 N = ',N,'；指定樣本數 n = ',n),HTML(html_table(data.frame(分層=st$label,母體人數=st$N,比例=fmt(st$N/N,4),未取整=fmt(ex),配置樣本數=alloc))),note('最大餘數法使配置合計恰好為 n；與原檔逐層四捨五入的總數可能略有不同。'))}
 })
 output$book_diagram<-renderUI({b<-book();if(b$type!='mean')return(note('此範例用來示範抽樣配置或臨界值，未繪製單一樣本檢定圖。'));a<-book_test();r<-a[a$method==input$bm,];diagram_ui(r,input$ba,input$bt,input$bm)})
 output$book_download<-renderUI({b<-book();a(href=paste0('https://hawjeng.github.io/statistical-inference-lab/',b$file),target='_blank',class='btn btn-default','下載對應 Excel 原檔 ↓')})
 observeEvent(input$kind,{pool<-if(input$kind=='numeric')d$numeric else d$categorical;updateSelectInput(session,'variable',choices=choice_labels(pool),selected=if(input$kind=='numeric')'B5' else 'PhoneBrand')})
 category_data<-reactive({req(input$kind=='category',d$categorical[[input$variable]]);d$categorical[[input$variable]]})
 output$category_choice<-renderUI({a<-category_data();selectInput('event','指定成功事件',setNames(as.character(seq_along(a$categories)),unlist(a$categories)))})
 counts<-reactive({a<-category_data();do.call(rbind,lapply(a$counts,unlist))})
 results<-reactive({al<-alpha();z<-as.integer(input$zodiac)
  if(input$kind=='numeric'){pop<-d$numeric[[input$variable]];req(pop);do.call(rbind,lapply(seq_along(pop$groups),function(i){g<-pop$groups[[i]];a<-do.call(rbind,lapply(c('z','t'),function(m)mean_result(g$n,g$mean,g$sd,pop$mu,pop$sigma,m,al,input$tail)));a$z<-i;a}))
  }else{req(input$event);m<-counts();j<-as.integer(input$event);p0<-sum(m[,j])/sum(m);do.call(rbind,lapply(1:12,function(i){a<-prop_result(m[i,j],sum(m[i,]),p0,al,input$tail);a$z<-i;a}))}
 })
 chi<-reactive({req(input$kind=='category');chi_result(counts(),if(input$scope=='overall')'independence' else 'gof',as.integer(input$zodiac))})
 is_chi<-reactive(input$kind=='category'&&(input$scope=='overall'||input$category_method=='gof'))
 anova<-reactive({req(input$kind=='numeric',input$scope=='overall');pop<-d$numeric[[input$variable]];g<-do.call(rbind,lapply(pop$groups,as.data.frame));k<-nrow(g);N<-sum(g$n)
  if(input$anova_method=='classic'){grand<-sum(g$n*g$mean)/N;ssb<-sum(g$n*(g$mean-grand)^2);ssw<-sum((g$n-1)*g$sd^2);df2<-N-k;F<-(ssb/(k-1))/(ssw/df2)}else{w<-g$n/g$sd^2;W<-sum(w);M<-sum(w*g$mean)/W;A<-sum((1-w/W)^2/(g$n-1));F<-(sum(w*(g$mean-M)^2)/(k-1))/(1+2*(k-2)*A/(k*k-1));df2<-(k*k-1)/(3*A)}
  list(stat=F,df=k-1,df2=df2,p=pf(F,k-1,df2,lower.tail=FALSE),N=N,k=k)
 })
 output$population<-renderUI({if(input$kind=='numeric'){pop<-d$numeric[[input$variable]];req(pop);div(class='population',paste0('有效 N = ',pop$N,'；全體 μ = ',fmt(pop$mu,4),'；母體 σ = ',fmt(pop$sigma,4)))}else{a<-category_data();div(class='population',paste0('有效 N = ',a$N,'；排除無法判定／缺失 ',a$excluded,' 筆。'),if(input$variable=='PhoneBrand')note('iPhone／非 iPhone；無手機編為非 iPhone，不確定或無法辨識的其他答案列為缺失。'),if(input$variable=='DataPlan')note('吃到飽／非吃到飽；限量、預付卡、Wi-Fi only 為非吃到飽，無法判定列為缺失。'))}})
 output$scope_note<-renderUI({if(input$scope=='overall')note(if(input$kind=='numeric')'整體比較使用右尾 F 檢定；顯著表示至少一組平均數不同，不能直接指出哪個星座不同。下方各星座 CI 仍是描述性雙側區間。' else '十二星座 × 類別的卡方獨立性檢定固定右尾；顯著表示存在關聯，不直接指出差異在哪一個星座。') else note('逐一星座與全體參考值比較；CI 固定雙側。只有雙尾時，CI 涵蓋與檢定結論直接對應。μ±kσ 刻度不是 SE 或 z／t-statistic。')})
 output$chart_legend<-renderUI({if(is_chi())note('每格列出 O、E 與 χ² 貢獻；卡方不繪製平均數 CI。')else HTML('<p class="legend"><span style="color:#2563eb">● ━ z</span>　<span style="color:#0891b2">◆ ┄ t</span>　黑色實線：μ　<span style="color:#c32d3b">紅色：CI 未涵蓋參考值</span></p>')})
 chi_table<-reactive({a<-chi();labs<-unlist(category_data()$categories);rows<-list();for(i in seq_len(nrow(a$obs)))for(j in seq_len(ncol(a$obs)))rows[[length(rows)+1]]<-data.frame(星座=z_names[if(input$scope=='overall')i else as.integer(input$zodiac)],類別=labs[j],觀察O=a$obs[i,j],期望E=fmt(a$expected[i,j],4),卡方貢獻=fmt(a$contrib[i,j],4));do.call(rbind,rows)})
 output$survey_chart<-renderUI({if(is_chi())HTML(html_table(chi_table()))else HTML(ci_svg(results(),z_names,input$kind=='category',as.integer(input$zodiac)))})
 output$survey_result<-renderUI({al<-alpha()
  if(input$scope=='overall'&&input$kind=='numeric'){r<-anova();tagList(h4(if(input$anova_method=='welch')'Welch ANOVA' else '一般單因子 ANOVA'),p('H₀：十二星座的母體平均數相等。'),p('H₁：至少一組平均數不同。'),p('F-statistic = ',fmt(r$stat,4),'；df₁ = ',r$df,'；df₂ = ',fmt(r$df2,3),'；p = ',p_fmt(r$p)),div(class=if(r$p<al)'decision reject' else 'decision',if(r$p<al)'拒絕 H₀' else '保留 H₀'),note('Welch 不要求組間變異數相等；一般 ANOVA 有等變異數假設。兩者都須考慮觀察值獨立性與抽樣設計。'))
  }else if(is_chi()){r<-chi();tagList(h4('Pearson χ² 檢定'),p(if(input$scope=='overall')'H₀：星座與此類別變數獨立；H₁：存在關聯。' else 'H₀：所選星座的分布符合全體比例；H₁：至少一類比例不同。'),p('χ² = ',fmt(r$stat,4),'；df = ',r$df,'；近似 p = ',p_fmt(r$p)),div(class=if(r$p<al)'decision reject' else 'decision',if(r$p<al)'拒絕 H₀' else '保留 H₀'),p(class=if(r$tiny>0||r$small/r$cells>.2)'warning' else 'hint',paste0('E < 5：',r$small,'/',r$cells,' 格；E < 1：',r$tiny,' 格。',if(r$tiny>0||r$small/r$cells>.2)'近似可能不適合，建議模擬 p-value。' else '符合本頁期望次數判準。')))
  }else{a<-results();a<-a[a$z==as.integer(input$zodiac),];tagList(h4(paste0(input$zodiac,'. ',z_names[as.integer(input$zodiac)])),hypotheses(input$tail,a$mu[1],if(input$kind=='numeric')'μ星座' else 'π星座'),lapply(seq_len(nrow(a)),function(i){r<-a[i,];div(class='result-block',h4(paste0(r$method,'_obt（',r$method,'-statistic）')),p('SE = ',fmt(r$se,4),'；',r$method,'_obt = ',fmt(r$stat,4),'；p = ',p_fmt(r$p)),div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀'))}),if(input$kind=='category'&&any(!a$approxOK))p(class='warning','n·p₀ 或 n·(1−p₀) < 5，比例 z 常態近似可能不適合。'))}
 })
 output$survey_diagram<-renderUI({al<-alpha();if(input$scope=='overall'&&input$kind=='numeric'){r<-anova();diagram_ui(r,al,'right','F',r$df2)}else if(is_chi()){r<-chi();diagram_ui(r,al,'right','chi')}else{a<-results();method<-if(input$kind=='numeric')input$method else 'z';r<-a[a$z==as.integer(input$zodiac)&a$method==method,];diagram_ui(r,al,input$tail,method)}})
 output$survey_table<-renderUI({if(is_chi())HTML(html_table(chi_table()))else if(input$scope=='overall'&&input$kind=='numeric'){
  r<-anova();pop<-d$numeric[[input$variable]];g<-do.call(rbind,lapply(pop$groups,as.data.frame))
  tagList(h4('整體 F 檢定'),HTML(html_table(data.frame(方法=if(input$anova_method=='welch')'Welch ANOVA' else '單因子 ANOVA',F=fmt(r$stat,4),df1=r$df,df2=fmt(r$df2,4),p=p_fmt(r$p),結論=if(r$p<alpha())'拒絕 H₀' else '保留 H₀'))),
   h4('各星座摘要（不是逐一檢定結果）'),HTML(html_table(data.frame(星座=paste0(1:12,'. ',z_names),n=g$n,平均數=fmt(g$mean,4),樣本SD=fmt(g$sd,4)))))
 }else{a<-results();tab<-result_table(a);tab<-cbind(星座=paste0(a$z,'. ',z_names[a$z]),tab);HTML(html_table(tab,a$reject))}})
 mc_value<-reactiveVal(NULL)
 observeEvent(list(input$variable,input$scope,input$zodiac,input$category_method),mc_value(NULL),ignoreInit=TRUE)
 output$mc_result<-renderUI({p<-mc_value();req(!is.null(p));p('模擬 p = ',fmt(p,4),'；',if(p<alpha())'拒絕 H₀' else '保留 H₀','。5,000 次；p = (極端次數+1)/5001；固定種子 20261028。')})
 observeEvent(input$mc,{req(is_chi());set.seed(20261028);m<-counts();r<-if(input$scope=='overall')suppressWarnings(chisq.test(m,simulate.p.value=TRUE,B=5000)) else suppressWarnings(chisq.test(m[as.integer(input$zodiac),],p=colSums(m)/sum(m),simulate.p.value=TRUE,B=5000));mc_value(r$p.value)})
 output$save_csv<-downloadHandler(filename=function()paste0(input$variable,'-results.csv'),content=function(file){if(is_chi())write.csv(chi_table(),file,row.names=FALSE,fileEncoding='UTF-8')else if(input$scope=='overall'&&input$kind=='numeric'){r<-anova();write.csv(data.frame(method=input$anova_method,F=r$stat,df1=r$df,df2=r$df2,p=r$p,alpha=alpha(),decision=if(r$p<alpha())'拒絕 H0' else '保留 H0'),file,row.names=FALSE,fileEncoding='UTF-8')}else write.csv(results(),file,row.names=FALSE,fileEncoding='UTF-8')})
 output$save_chart<-downloadHandler(filename=function()paste0(input$variable,'-CI.svg'),content=function(file){req(!is_chi());writeLines(enc2utf8(ci_svg(results(),z_names,input$kind=='category',as.integer(input$zodiac))),file,useBytes=TRUE)})
}
shinyApp(ui,server)

