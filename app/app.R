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
null_label<-function(tail,mu,param='μ')paste0('H₀：',param,if(tail=='left')' ≥ ' else if(tail=='right')' ≤ ' else ' = ',format(round(mu,4),trim=TRUE,scientific=FALSE))
diagram_ui<-function(r,alpha,tail,method,h0=null_label(tail,r$mu)) {
 HTML(distribution_svg(r$stat,r$critical,r$df,tail,alpha,r$p,method,h0=h0,ci=r))
}
comparison_ui<-function(a,alpha,tail,param='μ')fluidRow(lapply(c('z','t'),function(m){r<-a[a$method==m,];column(6,h4(paste0(m,'-test')),diagram_ui(r,alpha,tail,m,h0=null_label(tail,r$mu,param)))}))
hypotheses<-function(tail,mu,param='μ')HTML(paste0('<div class="hypothesis">H₀：',param,if(tail=='two')' = ' else if(tail=='left')' ≥ ' else ' ≤ ',fmt(mu,4),'<br>H₁：',param,if(tail=='two')' ≠ ' else if(tail=='left')' &lt; ' else ' &gt; ',fmt(mu,4),'</div>'))
result_table<-function(a) {
 data.frame(方法=a$method,n=a$n,平均數=fmt(a$mean,4),SE=fmt(a$se,4),CI下限=fmt(a$lower,4),CI上限=fmt(a$upper,4),
  `觀察檢定量（z_obt／t_obt）`=fmt(a$stat,4),df=ifelse(is.na(a$df),'—',a$df),臨界值=fmt(a$critical,4),p值=p_fmt(a$p),結論=ifelse(a$reject,'拒絕 H₀','保留 H₀'),check.names=FALSE)
}
ui<-fluidPage(tags$head(tags$title('區間估計與假設檢定｜邱皓政統計互動教材'),tags$link(rel='stylesheet',href='style.css'),tags$script(src='downloads.js'),
 tags$script(HTML("function reportHeight(){parent.postMessage({type:'inference-height',height:document.documentElement.scrollHeight},location.origin)};new ResizeObserver(reportHeight).observe(document.documentElement);setInterval(reportHeight,1500);"))),
 div(class='app-top',a(href='https://hawjeng.github.io/resources.html',class='btn btn-default btn-sm','← 返回教學與專案'),a(href='https://github.com/Hawjeng/statistical-inference-lab',target='_blank','GitHub 原始碼 ↗')),
 div(class='app-title',p(class='eyebrow','STATISTICS LAB · CONFIDENCE INTERVALS & HYPOTHESIS TESTING'),h1('區間估計與假設檢定'),p('原理說明、課本範例，再以星座調查資料練習。')),
 tabsetPanel(id='main_tab',selected='課本範例',
 tabPanel('原理說明',includeHTML('www/lesson.html')),
 tabPanel('課本範例',
  card(h3('從課本的問題開始'),p('不依章次，以問題主題選擇範例；可下載 14 份對應 Excel 原檔。'),
   selectInput('example','範例主題',example_choices,width='100%'),
   div(style='display:none',textOutput('book_type'),textOutput('book_estimation'),textOutput('book_testing')),
   conditionalPanel("output.book_type == 'mean' || output.book_type == 'critical'",numericInput('ba','顯著水準 α',.05,min=.001,max=.2,step=.001)),
   conditionalPanel("output.book_estimation == 'yes'",checkboxInput('book_enable_test','延伸練習：自行指定 μ₀，啟用假設檢定',FALSE)),
   conditionalPanel("output.book_type == 'mean'",div(class='book-controls',
    selectInput('bm','估計／檢定方式',c('z：σ 已知'='z','t：σ 未知'='t')),
    conditionalPanel("output.book_testing == 'yes'",selectInput('bt','檢定方向',tails)),
    conditionalPanel("output.book_testing == 'yes'",numericInput('bmu','假設參考值 μ₀（須由題目或使用者指定）',NA)),
    conditionalPanel("input.bm == 'z'",numericInput('bsigma','已知母體標準差 σ',10,min=.0001)))),
   conditionalPanel("output.book_type == 'sampling' || output.book_type == 'stratified'",fluidRow(column(6,numericInput('bn','抽樣數 n',10,min=1)),column(6,actionButton('resample','重新抽樣／配置',class='btn-primary')))),
   uiOutput('book_question'),uiOutput('book_result'),uiOutput('book_download')),
  card(h3(textOutput('book_figure_title',inline=TRUE)),uiOutput('book_diagram'),note('CI 一律保持雙側 (1−α) 區間。未指定 μ₀ 的估計題不顯示觀察檢定量或 p-value。'))),
 tabPanel('星座範例',
  card(h3('各星座的估計與檢定'),
   fluidRow(column(6,selectInput('kind','變數類型',c('數值變數（平均數）'='numeric','類別／二分變數'='category'),selected='category')),
    column(6,selectInput('variable','分析變數',choice_labels(d$categorical),selected='PhoneBrand'))),
   fluidRow(column(3,numericInput('alpha','顯著水準 α',.05,min=.001,max=.2,step=.001)),column(3,selectInput('zodiac','選擇星座',setNames(as.character(1:12),paste0(1:12,'. ',z_names)))),
    column(3,selectInput('tail','檢定方向',tails)),column(3,conditionalPanel("input.kind == 'numeric'",note('檢定圖：左 z-test、右 t-test')))),
   conditionalPanel("input.kind == 'category'",fluidRow(column(6,uiOutput('category_choice')))),
   uiOutput('population'),uiOutput('scope_note')),
  fluidRow(column(8,card(h3('資料圖與雙側信賴區間'),uiOutput('chart_legend'),div(class='ci-scroll',uiOutput('survey_chart')),tags$button(type='button',class='btn btn-default',onclick="downloadCurrentSVG()",'下載圖形 SVG'))),
   column(4,uiOutput('survey_ci_steps'),card(h3('假設、檢定與結論'),uiOutput('survey_result'),note('保留 H₀ 表示未能拒絕，不表示證明相等。')))),
  card(h3('檢定示意圖'),uiOutput('survey_diagram'),note('橘色：α 拒絕域；藍色：p-value 尾端面積。紫色實線與文字：觀察檢定量及其 p-value；橘色虛線：臨界值。')),
  card(h3('完整計算表'),tags$details(tags$summary('展開／收合完整計算與檢定'),uiOutput('survey_table')),actionButton('save_csv','下載目前結果 CSV'))),
 tabPanel('教材下載與說明',card(h3('語法、資料與教材'),p('完整 Shiny 原始碼與資料放在 GitHub，新專案可下載後在 RStudio 執行。'),
  a(class='btn btn-primary',href='https://github.com/Hawjeng/statistical-inference-lab',target='_blank','查看 GitHub 原始碼'),br(),br(),
  a(href='https://hawjeng.github.io/statistical-inference-lab/downloads/zodiac_ci_calculations.xlsx',target='_blank','四變數 Excel 計算檔 ↓'),br(),
  a(href='https://hawjeng.github.io/statistical-inference-lab/downloads/zodiac_ci_report.html',target='_blank','四變數完整 HTML 報告 ↓'),br(),
  a(href='https://raw.githubusercontent.com/Hawjeng/statistical-inference-lab/main/analysis.R',target='_blank','R 分析語法 ↓'),
  includeHTML('www/limitations.html')))),
 tags$footer(class='hint',
  tags$hr(),
  p('出自：邱皓政、林碧芳（2022）。統計學：原理與應用。五南。'),
  p(a(href='https://hawjeng.github.io/books/statistics-2022.html','文獻連結')))
)
server<-function(input,output,session) {
 alpha<-reactive({req(input$alpha);validate(need(input$alpha>=.001&&input$alpha<=.2,'α 請輸入 0.001 到 0.20'));input$alpha})
 book<-reactive({b<-examples[[match(input$example,vapply(examples,function(a)a$id,character(1)))]];if(b$type=='mean'){v<-unlist(b$values);b$n<-length(v);b$mean<-mean(v);b$sd<-sd(v)};b})
 book_testing<-reactive(book()$type=='mean'&&(!isTRUE(book()$estimationOnly)||isTRUE(input$book_enable_test)))
 output$book_type<-renderText(book()$type)
 output$book_estimation<-renderText(if(book()$type=='mean'&&isTRUE(book()$estimationOnly))'yes' else 'no')
 output$book_testing<-renderText(if(book_testing())'yes' else 'no')
 outputOptions(output,'book_type',suspendWhenHidden=FALSE)
 outputOptions(output,'book_estimation',suspendWhenHidden=FALSE)
 outputOptions(output,'book_testing',suspendWhenHidden=FALSE)
 observeEvent(input$example,{b<-book();updateCheckboxInput(session,'book_enable_test',value=FALSE);if(b$type=='mean'){updateNumericInput(session,'bmu',value=if(is.null(b$mu))NA else b$mu);updateNumericInput(session,'bsigma',value=b$sigma);updateSelectInput(session,'bm',selected=b$method);updateSelectInput(session,'bt',selected=b$tail)};updateNumericInput(session,'bn',value=if(b$type=='stratified')1000 else 10)},ignoreNULL=FALSE)
 output$book_question<-renderUI({b<-book();tagList(h4(b$title),if(b$type=='mean')note(if(isTRUE(b$estimationOnly))'原檔為區間估計，預設只顯示 CI。若要延伸檢定，須勾選並另外指定 μ₀。' else '先決定單尾或雙尾，再比較觀察差距與標準誤。'),if(grepl('研究生年齡',b$title))note('觀察資料：n=25、x̄=24、s≈2.5；檢定題 μ₀=23，依所附年齡檢定 Excel 公式；z 使用已知 σ=2。樣本平均數不是假設參考值。'),note('CI 固定雙側；由原始觀察值即時計算，與原檔先將標準誤四捨五入的答案可能略有差異。'))})
 book_test<-reactive({b<-book();req(b$type=='mean',input$ba,input$bsigma);validate(need(input$ba>=.001&&input$ba<=.2,'α 須介於 .001 與 .20'),need(input$bsigma>0,'σ 須大於 0'));mu<-NA_real_;if(book_testing()){validate(need(length(input$bmu)==1&&is.finite(input$bmu),'請依題意自行輸入假設參考值 μ₀；不能自動用樣本平均數代替。'));mu<-input$bmu};do.call(rbind,lapply(c('z','t'),function(m)mean_result(b$n,b$mean,b$sd,mu,input$bsigma,m,input$ba,if(book_testing())input$bt else 'two')))})
 output$book_result<-renderUI({input$resample;b<-book()
  if(b$type=='mean'){a<-book_test();r<-a[a$method==input$bm,];testing<-book_testing();tagList(div(class='population',paste0('n = ',r$n,'；x̄ = ',fmt(r$mean,4),if(grepl('投手',b$title))'；s ≈ 9' else paste0('；s = ',fmt(r$sd,4)),if(r$method=='z')paste0('；已知 σ = ',fmt(r$sigma,4)) else '；母體 σ 未知，以樣本 s 估計')),if(grepl('投手',b$title))note('原始 30 筆資料的 s = 8.998084，原題近似為 9；計算保留原始精度。'),if(testing)hypotheses(input$bt,input$bmu),
   h4('1　標準誤'),p(if(r$method=='z')'SE = σ/√n' else 'SE = s/√n',' = ',fmt(r$se,4)),h4('2　雙側信賴區間'),p(paste0(fmt((1-input$ba)*100,1),'% CI = x̄ ± ',fmt(r$ci_critical,4),' × SE = [',fmt(r$lower,4),', ',fmt(r$upper,4),'] ',b$unit)),
   if(testing)tagList(h4('3　觀察檢定量與決策'),p(paste0(r$method,'_obt = (x̄−μ₀)/SE = ',fmt(r$stat,4),'；p = ',p_fmt(r$p))),note('z_obt／t_obt 是觀察檢定量；zcv／tcv 是檢定臨界值。'),div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀')) else note('這是區間估計題，沒有指定 H₀，不計算觀察檢定量或 p-value。'),
   tags$details(tags$summary('完整計算表（展開／收合）'),if(testing)HTML(html_table(result_table(a),a$reject))else HTML(html_table(data.frame(方法=r$method,n=r$n,平均數=fmt(r$mean,4),SE=fmt(r$se,4),CI臨界值=fmt(r$ci_critical,4),CI下限=fmt(r$lower,4),CI上限=fmt(r$upper,4))))),tags$details(tags$summary('原始有效數值'),p(paste(fmt(unlist(b$values)),collapse='、'))))
  }else if(b$type=='critical'){as<-c(input$ba,.1,.05,.01,.001);as<-unique(as);HTML(html_table(data.frame(α=as,左尾=fmt(qnorm(as),4),右尾=fmt(qnorm(1-as),4),雙尾=paste0('±',fmt(qnorm(1-as/2),4)))))
  }else if(b$type=='sampling'){ids<-unlist(b$ids);n<-as.integer(input$bn);validate(need(n>=1&&n<=length(ids),paste('樣本數須介於 1 與',length(ids))));k<-length(ids)/n;ss<-ids[floor(runif(1,0,k)+(0:(n-1))*k)+1];tagList(p('名冊 N = ',length(ids),'；抽樣 n = ',n,'；系統間距 N/n = ',fmt(k,2)),h4('簡單隨機抽樣'),p(paste(sample(ids,n),collapse='、')),h4('系統隨機抽樣'),p(paste(ss,collapse='、')),note('編號只用來辨識抽樣單位，不對學號的平均數作檢定。'))
  }else{st<-do.call(rbind,lapply(b$strata,as.data.frame));N<-sum(st$N);n<-as.integer(input$bn);validate(need(n>=1&&n<=N,'抽樣數須為正整數且不超過母體 N'));ex<-n*st$N/N;alloc<-floor(ex);left<-n-sum(alloc);if(left>0){ix<-order(ex-alloc,decreasing=TRUE)[seq_len(left)];alloc[ix]<-alloc[ix]+1};tagList(p('母體 N = ',N,'；指定樣本數 n = ',n),HTML(html_table(data.frame(分層=st$label,母體人數=st$N,比例=fmt(st$N/N,4),未取整=fmt(ex),配置樣本數=alloc))),note('最大餘數法使配置合計恰好為 n；與原檔逐層四捨五入的總數可能略有不同。'))}
 })
 output$book_figure_title<-renderText(if(book()$type=='mean'&&!book_testing())'區間估計圖' else '檢定示意圖')
 output$book_diagram<-renderUI({b<-book();if(b$type!='mean')return(note('此範例用來示範抽樣配置或臨界值，未繪製單一樣本檢定圖。'));a<-book_test();r<-a[a$method==input$bm,];if(!book_testing())return(HTML(single_ci_svg(r,input$ba,b$unit)));div(style='max-width:850px;margin:auto',h4(paste0(input$bm,'-test')),diagram_ui(r,input$ba,input$bt,input$bm))})
 output$book_download<-renderUI({b<-book();a(href=paste0('https://hawjeng.github.io/statistical-inference-lab/',b$file),target='_blank',class='btn btn-default','下載對應 Excel 原檔 ↓')})
 observeEvent(input$kind,{pool<-if(input$kind=='numeric')d$numeric else d$categorical;updateSelectInput(session,'variable',choices=choice_labels(pool),selected=if(input$kind=='numeric')'CT' else 'PhoneBrand')})
 category_data<-reactive({req(input$kind=='category',d$categorical[[input$variable]]);d$categorical[[input$variable]]})
 output$category_choice<-renderUI({a<-category_data();selectInput('event','指定成功事件',setNames(as.character(seq_along(a$categories)),unlist(a$categories)))})
 counts<-reactive({a<-category_data();do.call(rbind,lapply(a$counts,unlist))})
 results<-reactive({al<-alpha();z<-as.integer(input$zodiac)
  if(input$kind=='numeric'){pop<-d$numeric[[input$variable]];req(pop);do.call(rbind,lapply(seq_along(pop$groups),function(i){g<-pop$groups[[i]];a<-do.call(rbind,lapply(c('z','t'),function(m)mean_result(g$n,g$mean,g$sd,pop$mu,pop$sigma,m,al,input$tail)));a$z<-i;a}))
  }else{req(input$event);m<-counts();j<-as.integer(input$event);req(j %in% seq_len(ncol(m)));p0<-sum(m[,j])/sum(m);do.call(rbind,lapply(1:12,function(i){a<-prop_result(m[i,j],sum(m[i,]),p0,al,input$tail);a$z<-i;a}))}
 })
 output$population<-renderUI({
  if(input$kind=='numeric'){
   pop<-d$numeric[[input$variable]];req(pop)
   variance<-pop$sigma^2
   div(class='population',paste0('有效 N = ',pop$N,'；全體 μ = ',fmt(pop$mu,4),
    '；母體變異數 Var.p = ',fmt(variance,4),'；母體標準差 Stdev.p = ',fmt(pop$sigma,4)))
  }else{
   a<-category_data()
   div(class='population',paste0('有效 N = ',a$N,'；排除無法判定／缺失 ',a$excluded,' 筆。'),
    if(input$variable=='PhoneBrand')note('iPhone／非 iPhone；無手機編為非 iPhone，不確定或無法辨識的其他答案列為缺失。'),
    if(input$variable=='DataPlan')note('吃到飽／非吃到飽；限量、預付卡、Wi-Fi only 為非吃到飽，無法判定列為缺失。'))
  }
 })
 output$scope_note<-renderUI({
  if(input$kind=='numeric')note('逐一星座與全體參考值比較。CI 固定採雙側區間。只有雙尾檢定時，CI 涵蓋與檢定結論直接對應。')
  else note('比例 CI 固定採雙側區間，以樣本比例 p̂ 估計標準誤。單一比例 z 檢定以全體參考比例 p₀ 計算標準誤。')
 })
 output$chart_legend<-renderUI({
  numeric<-input$kind=='numeric'
  p(class='legend',span(style='color:#2563eb',if(numeric)'● ━ z' else '● ━ 比例 CI'),
   if(numeric)span(style='color:#0891b2','◆ ┄ t'),
   if(numeric)'黑色實線：μ' else '黑色實線：p₀',
   span(style='color:#c32d3b','紅色：CI 未涵蓋參考值'))
 })
 output$survey_chart<-renderUI(HTML(ci_svg(results(),z_names,input$kind=='category',as.integer(input$zodiac))))
 selected_results<-reactive({
  req(input$zodiac)
  a<-results()
  a[a$z==as.integer(input$zodiac),]
 })
 output$survey_ci_steps<-renderUI({
  a<-selected_results()
  selected<-as.integer(input$zodiac)
  al<-alpha()
  if(input$kind=='category'){
   event<-as.integer(input$event)
   successes<-counts()[selected,event]
   event_label<-category_data()$categories[[event]]
   n<-a$n
   phat<-fmt(a$mean,4)
   zcv<-fmt(a$ci_critical,4)
   ci_se<-fmt(sqrt(a$mean*(1-a$mean)/n),4)
   return(card(
    h3(paste0(fmt((1-al)*100,1),'% 雙側比例信賴區間')),
    p(paste0(selected,'. ',z_names[selected],'｜成功事件：',event_label)),
    p(paste0('p̂ = x/n = ',successes,'/',n,' = ',phat)),
    p(paste0('α/2 = ',fmt(al/2,4),'｜zcv = ',zcv)),
    p('SE = √[p̂(1 − p̂)/n]'),
    p(paste0('= √[',phat,' × (1 − ',phat,')/',n,'] = ',ci_se)),
    p('CI = p̂ ± zcv × SE'),
    p(paste0('= ',phat,' ± ',zcv,' × ',ci_se)),
    p(paste0('= [',fmt(a$lower*100,2),'%, ',fmt(a$upper*100,2),'%]')),
    note('本區間採 Wald CI，端點以未四捨五入的數值計算。')
   ))
  }
  card(
   h3(paste0(fmt((1-al)*100,1),'% 雙側信賴區間計算')),
   p(paste0(selected,'. ',z_names[selected],'｜n = ',a$n[1],'｜x̄ = ',fmt(a$mean[1],4))),
   p(paste0('α = ',fmt(al,3),'｜每尾 α/2 = ',fmt(al/2,4))),
   lapply(seq_len(nrow(a)),function(i){
    r<-a[i,]
    is_z<-r$method=='z'
    cv_label<-if(is_z)'zcv' else 'tcv'
    df_detail<-if(!is_z)paste0('（df = n − 1 = ',r$n,' − 1 = ',r$df,'）')
    spread_label<-if(is_z)'σ' else 's'
    spread<-if(is_z)r$sigma else r$sd
    div(class='result-block',
     h4(if(is_z)'z 信賴區間' else 't 信賴區間'),
     p(paste0(cv_label,' = ',fmt(r$ci_critical,4),df_detail)),
     p(paste0('SE = ',spread_label,'/√n = ',fmt(spread,4),'/√',r$n,' = ',fmt(r$se,4))),
     p(paste0('CI = x̄ ± ',if(is_z)'zcv' else 'tcv(df)',' × SE')),
     p(paste0('= ',fmt(r$mean,4),' ± ',fmt(r$ci_critical,4),' × ',fmt(r$se,4),
      ' = [',fmt(r$lower,4),', ',fmt(r$upper,4),']'))
    )
   }),
   note('端點以未四捨五入的數值計算。')
  )
 })
 output$survey_result<-renderUI({
  a<-selected_results()
  zodiac_name<-z_names[as.integer(input$zodiac)]
  param<-paste0(if(input$kind=='numeric')'μ' else 'π',zodiac_name)
  tagList(
   h4(paste0(input$zodiac,'. ',zodiac_name)),
   hypotheses(input$tail,a$mu[1],param),
   lapply(seq_len(nrow(a)),function(i){
    r<-a[i,]
    div(class='result-block',h4(paste0(r$method,'_obt（',r$method,'-statistic）')),
     p('SE = ',fmt(r$se,4),'；',r$method,'_obt = ',fmt(r$stat,4),'；p = ',p_fmt(r$p)),
     div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀'))
   }),
   if(input$kind=='category'&&any(!a$approxOK))p(class='warning','n·p₀ 或 n·(1−p₀) < 5，比例 z 常態近似可能不適合。')
  )
 })
 output$survey_diagram<-renderUI({
  a<-selected_results()
  al<-alpha()
  if(input$kind=='numeric')comparison_ui(a,al,input$tail)
  else diagram_ui(a,al,input$tail,'z',h0=null_label(input$tail,a$mu,'π'))
 })
 output$survey_table<-renderUI({
  a<-results()
  tab<-cbind(星座=paste0(a$z,'. ',z_names[a$z]),result_table(a))
  HTML(html_table(tab,a$reject))
 })
 observeEvent(input$save_csv,{
  a<-results()
  tab<-cbind(星座=paste0(a$z,'. ',z_names[a$z]),a)
  lines<-capture.output(write.csv(tab,row.names=FALSE))
  session$sendCustomMessage('download-file',list(name=paste0(input$variable,'-results.csv'),mime='text/csv;charset=utf-8',content=paste0('\ufeff',paste(lines,collapse='\r\n'))))
 })

}
shinyApp(ui,server)

