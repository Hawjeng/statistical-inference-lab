library(shiny)
# JSON uses explicit namespace to avoid masking shiny::validate.
source('stats.R',encoding='UTF-8',local=TRUE)
source('code_examples.R',encoding='UTF-8',local=TRUE)
d<-jsonlite::fromJSON('data/summary.json',simplifyVector=FALSE)
examples<-jsonlite::fromJSON('data/textbook.json',simplifyVector=FALSE)
z_names<-unlist(d$zodiacNames)
z_labels<-c('全體樣本',paste0(seq_along(z_names),'. ',z_names))
choice_labels<-function(x)setNames(names(x),vapply(x,function(a)a$label,character(1)))
demo_examples<-Filter(function(a)!a$id %in% c('Ch08p179pitcher','Ch08p183pitcher'),examples)
example_choices<-setNames(vapply(demo_examples,function(a)a$id,character(1)),vapply(demo_examples,function(a)a$title,character(1)))
code_choices<-list(
 '課本範例'=setNames(paste0('book:',example_choices),names(example_choices)),
 '星座調查・連續變數'=setNames(paste0('numeric:',names(d$numeric)),names(choice_labels(d$numeric))),
 '星座調查・類別變數'=setNames(paste0('category:',names(d$categorical)),names(choice_labels(d$categorical))))
# Native selects work consistently on desktop, mobile and WebR.
selectInput<-function(...)shiny::selectInput(...,selectize=FALSE)
tails<-c('雙尾 ≠'='two','左尾 <'='left','右尾 >'='right')
card<-function(...,class='')div(class=paste('lab-card',class),...)
note<-function(...)p(class='hint',...)
null_label<-function(tail,mu,param='μ')paste0('H₀：',param,if(tail=='left')' ≥ ' else if(tail=='right')' ≤ ' else ' = ',format(round(mu,4),trim=TRUE,scientific=FALSE))
diagram_ui<-function(r,alpha,tail,method,h0=null_label(tail,r$mu)) {
 HTML(distribution_svg(r$stat,r$critical,r$df,tail,alpha,r$p,method,h0=h0,ci=r))
}
comparison_ui<-function(a,alpha,tail,param='μ')div(class='test-stack',lapply(c('z','t'),function(m){r<-a[a$method==m,];div(class='test-panel',h4(paste0(m,'-test')),diagram_ui(r,alpha,tail,m,h0=null_label(tail,r$mu,param)))}))
hypotheses<-function(tail,mu,param='μ')HTML(paste0('<div class="hypothesis">H₀：',param,if(tail=='two')' = ' else if(tail=='left')' ≥ ' else ' ≤ ',fmt(mu,4),'<br>H₁：',param,if(tail=='two')' ≠ ' else if(tail=='left')' &lt; ' else ' &gt; ',fmt(mu,4),'</div>'))
result_table<-function(a) {
 data.frame(方法=a$method,n=a$n,平均數=fmt(a$mean,4),信賴區間標準誤=fmt(a$ci_se,4),CI下限=fmt(a$lower,4),CI上限=fmt(a$upper,4),
  假設檢定標準誤=fmt(a$se,4),
  `觀察檢定量（z_obt／t_obt）`=fmt(a$stat,4),df=ifelse(is.na(a$df),'—',a$df),臨界值=fmt(a$critical,4),p值=ifelse(is.na(a$p),'—',p_fmt(a$p)),結論=ifelse(is.na(a$reject),'—',ifelse(a$reject,'拒絕 H₀','保留 H₀')),check.names=FALSE)
}
ui<-fluidPage(tags$head(tags$title('區間估計與假設檢定｜邱皓政統計互動教材'),tags$link(rel='stylesheet',href='style.css?v=stacked-test-diagrams'),tags$script(src='downloads.js'),
 tags$script(HTML("function reportHeight(){parent.postMessage({type:'inference-height',height:document.documentElement.scrollHeight},location.origin)};new ResizeObserver(reportHeight).observe(document.documentElement);setInterval(reportHeight,1500);"))),
 tags$head(tags$link(rel='stylesheet',href='https://hawjeng.github.io/assets/lab-return.css')),
 div(class='app-top',a(href='https://hawjeng.github.io/resources.html#statistics-apps',class='lab-return','← 返回教學與專案'),a(href='https://github.com/Hawjeng/statistical-inference-lab',target='_blank','GitHub 原始碼 ↗')),
 div(class='app-title',p(class='eyebrow','STATISTICS LAB · CONFIDENCE INTERVALS & HYPOTHESIS TESTING'),h1('區間估計與假設檢定'),p('原理說明、課本範例，再以星座調查資料練習。')),
 tabsetPanel(id='main_tab',selected='課本範例',
 tabPanel('原理說明',includeHTML('www/lesson.html')),
 tabPanel('課本範例',
  card(h3('從課本的問題開始'),p('不依章次，以問題主題選擇範例；投手球速的兩種 95% CI 請見原理說明，並可下載對應 Excel 原檔。'),
   div(class='book-topic',selectInput('example','範例主題',example_choices)),
   div(style='display:none',textOutput('book_type'),textOutput('book_estimation'),textOutput('book_testing')),
   div(class='book-controls',
    conditionalPanel("output.book_type == 'mean' || output.book_type == 'critical'",
     numericInput('ba','顯著水準 α',.05,min=.001,max=.2,step=.001)),
    conditionalPanel("output.book_type == 'mean'",
     selectInput('bm','估計／檢定方式',c('z：σ 已知'='z','t：σ 未知'='t'))),
    conditionalPanel("output.book_type == 'mean' && input.bm == 'z'",
     numericInput('bsigma','已知母體標準差 σ',2,min=.0001))),
   conditionalPanel("output.book_estimation == 'yes'",checkboxInput('book_enable_test','延伸練習：自行指定 μ₀，啟用假設檢定',FALSE)),
   conditionalPanel("output.book_testing == 'yes'",div(class='book-controls',
    selectInput('bt','檢定方向',tails),
    numericInput('bmu','假設參考值 μ₀（須由題目或使用者指定）',NA))),
   conditionalPanel("output.book_type == 'sampling' || output.book_type == 'stratified'",fluidRow(column(6,numericInput('bn','抽樣數 n',10,min=1)),column(6,actionButton('resample','重新抽樣／配置',class='btn-primary')))),
   uiOutput('book_question'),uiOutput('book_result'),uiOutput('book_download')),
  card(h3(textOutput('book_figure_title',inline=TRUE)),uiOutput('book_diagram'),note('CI 一律保持雙側 (1−α) 區間。未指定 μ₀ 的估計題不顯示觀察檢定量或 p-value。'))),
 tabPanel('星座範例',
  card(h3('各星座的估計與檢定'),
   div(class='survey-inputs',
    selectInput('zodiac','選擇星座',setNames(0:12,z_labels),selected='0'),
    selectInput('kind','變數類型',c('連續變數'='numeric','類別／二分變數'='category'),selected='numeric'),
    selectInput('variable','分析變數',choice_labels(d$numeric),selected='CT')),
   div(class='survey-inputs',
    numericInput('alpha','顯著水準 α',.05,min=.001,max=.2,step=.001),
    conditionalPanel("input.zodiac != '0'",selectInput('tail','檢定方向',tails)),
    conditionalPanel("input.kind == 'category'",uiOutput('category_choice')),
    lapply(names(d$numeric),function(key){
     pop<-d$numeric[[key]]
     condition<-sprintf("input.kind == 'numeric' && input.variable == '%s'",key)
     tagList(
      conditionalPanel(condition,numericInput(paste0('survey_mu_',key),'母體平均數 μ₀',round(pop$mu,4),step='any')),
      conditionalPanel(condition,numericInput(paste0('survey_sigma_',key),'母體標準差 σ',round(pop$sigma,4),step='any')))
    })),
   uiOutput('population'),uiOutput('scope_note')),
  card(h3('資料圖與雙側信賴區間'),checkboxInput('show_all_groups','展開其他資料',FALSE),uiOutput('chart_legend'),div(class='ci-scroll',uiOutput('survey_chart')),tags$button(type='button',class='btn btn-default',onclick="downloadCurrentSVG()",'下載圖形 SVG')),
  div(class='survey-workspace',
   div(class='survey-calculations',
    tags$details(class='lab-card survey-details',open=NA,
     tags$summary(textOutput('survey_ci_title',inline=TRUE),span(class='survey-toggle',`aria-hidden`='true')),uiOutput('survey_ci_steps')),
    tags$details(class='lab-card survey-details',open=NA,
     tags$summary(span('假設、檢定與結論'),span(class='survey-toggle',`aria-hidden`='true')),uiOutput('survey_result'),
     note('保留 H₀ 表示未能拒絕，不表示證明相等。'))),
   conditionalPanel("input.zodiac != '0'",card(h3('檢定示意圖'),uiOutput('survey_diagram'),class='survey-diagrams'))),
  card(h3('完整計算表'),tags$details(tags$summary('展開／收合完整計算與檢定'),uiOutput('survey_table')),actionButton('save_csv','下載目前結果 CSV'))),
 tabPanel('R／Python 語法',
  card(h3('分析語法與結果'),
   p('可直接選擇課本題目或星座調查變數。顯著水準、星座等設定與分析分頁共用。'),
   div(class='code-controls',
    selectInput('code_topic','分析題目',code_choices),
    selectInput('code_lang','語言',c('R'='r','Python'='python'))),
   uiOutput('code_example'))),
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
 book_result<-reactive({input$resample;b<-book()
  if(b$type=='mean'){a<-book_test();r<-a[a$method==input$bm,];testing<-book_testing();tagList(div(class='population',paste0('n = ',r$n,'；x̄ = ',fmt(r$mean,4),if(grepl('投手',b$title))'；s ≈ 9' else paste0('；s = ',fmt(r$sd,4)),if(r$method=='z')paste0('；已知 σ = ',fmt(r$sigma,4)) else '；母體 σ 未知，以樣本 s 估計')),if(grepl('投手',b$title))note('原始 30 筆資料的 s = 8.998084，原題近似為 9；計算保留原始精度。'),if(testing)hypotheses(input$bt,input$bmu),
   h4('1　標準誤'),p(if(r$method=='z')'SE = σ/√n' else 'SE = s/√n',' = ',fmt(r$se,4)),h4('2　雙側信賴區間'),p(paste0(fmt((1-input$ba)*100,1),'% CI = x̄ ± ',fmt(r$ci_critical,4),' × SE = [',fmt(r$lower,4),', ',fmt(r$upper,4),'] ',b$unit)),
   if(testing)tagList(h4('3　觀察檢定量與決策'),p(paste0(r$method,'_obt = (x̄−μ₀)/SE = ',fmt(r$stat,4),'；p = ',p_fmt(r$p))),note('z_obt／t_obt 是觀察檢定量；zcv／tcv 是檢定臨界值。'),div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀')) else note('這是區間估計題，沒有指定 H₀，不計算觀察檢定量或 p-value。'),
   tags$details(tags$summary('完整計算表（展開／收合）'),if(testing)HTML(html_table(result_table(a),a$reject))else HTML(html_table(data.frame(方法=r$method,n=r$n,平均數=fmt(r$mean,4),SE=fmt(r$se,4),CI臨界值=fmt(r$ci_critical,4),CI下限=fmt(r$lower,4),CI上限=fmt(r$upper,4))))),tags$details(tags$summary('原始有效數值'),p(paste(fmt(unlist(b$values)),collapse='、'))))
  }else if(b$type=='critical'){as<-c(input$ba,.1,.05,.01,.001);as<-unique(as);HTML(html_table(data.frame(α=as,左尾=fmt(qnorm(as),4),右尾=fmt(qnorm(1-as),4),雙尾=paste0('±',fmt(qnorm(1-as/2),4)))))
  }else if(b$type=='sampling'){ids<-unlist(b$ids);n<-as.integer(input$bn);validate(need(n>=1&&n<=length(ids),paste('樣本數須介於 1 與',length(ids))));k<-length(ids)/n;ss<-ids[floor(runif(1,0,k)+(0:(n-1))*k)+1];tagList(p('名冊 N = ',length(ids),'；抽樣 n = ',n,'；系統間距 N/n = ',fmt(k,2)),h4('簡單隨機抽樣'),p(paste(sample(ids,n),collapse='、')),h4('系統隨機抽樣'),p(paste(ss,collapse='、')),note('編號只用來辨識抽樣單位，不對學號的平均數作檢定。'))
  }else{st<-do.call(rbind,lapply(b$strata,as.data.frame));N<-sum(st$N);n<-as.integer(input$bn);validate(need(n>=1&&n<=N,'抽樣數須為正整數且不超過母體 N'));ex<-n*st$N/N;alloc<-floor(ex);left<-n-sum(alloc);if(left>0){ix<-order(ex-alloc,decreasing=TRUE)[seq_len(left)];alloc[ix]<-alloc[ix]+1};tagList(p('母體 N = ',N,'；指定樣本數 n = ',n),HTML(html_table(data.frame(分層=st$label,母體人數=st$N,比例=fmt(st$N/N,4),未取整=fmt(ex),配置樣本數=alloc))),note('最大餘數法使配置合計恰好為 n；與原檔逐層四捨五入的總數可能略有不同。'))}
 })
 output$book_result<-renderUI(book_result())
 output$book_figure_title<-renderText(if(book()$type=='mean'&&!book_testing())'區間估計圖' else '檢定示意圖')
 output$book_diagram<-renderUI({b<-book();if(b$type!='mean')return(note('此範例用來示範抽樣配置或臨界值，未繪製單一樣本檢定圖。'));a<-book_test();r<-a[a$method==input$bm,];if(!book_testing())return(HTML(single_ci_svg(r,input$ba,b$unit)));div(style='max-width:850px;margin:auto',h4(paste0(input$bm,'-test')),diagram_ui(r,input$ba,input$bt,input$bm))})
 output$book_download<-renderUI({b<-book();a(href=paste0('https://hawjeng.github.io/statistical-inference-lab/',b$file),target='_blank',class='btn btn-default','下載對應 Excel 原檔 ↓')})
 code_topic<-reactive({
  req(input$code_topic)
  strsplit(input$code_topic,':',fixed=TRUE)[[1]]
 })
 observeEvent(input$code_topic,{
  topic<-code_topic()
  if(topic[1]=='book'){
   updateSelectInput(session,'example',selected=topic[2])
  }else{
   updateSelectInput(session,'kind',selected=topic[1])
   updateSelectInput(session,'variable',choices=choice_labels(d[[if(topic[1]=='numeric')'numeric' else 'categorical']]),selected=topic[2])
  }
 },ignoreInit=TRUE)
 observeEvent(input$main_tab,{
  if(input$main_tab!='R／Python 語法')return()
  topic<-code_topic()
  if(topic[1]=='book')updateSelectInput(session,'code_topic',selected=paste0('book:',input$example))
  else updateSelectInput(session,'code_topic',selected=paste(input$kind,input$variable,sep=':'))
 },ignoreInit=TRUE)
 observeEvent(input$kind,{
  pool<-if(input$kind=='numeric')d$numeric else d$categorical
  selected<-if(input$kind=='numeric')'CT' else 'PhoneBrand'
  if(identical(input$main_tab,'R／Python 語法')&&!is.null(input$code_topic)){
   topic<-code_topic()
   if(topic[1]==input$kind)selected<-topic[2]
  }
  updateSelectInput(session,'variable',choices=choice_labels(pool),selected=selected)
 })
 category_data<-reactive({req(input$kind=='category',d$categorical[[input$variable]]);d$categorical[[input$variable]]})
 output$category_choice<-renderUI({a<-category_data();selectInput('event','指定成功事件',setNames(as.character(seq_along(a$categories)),unlist(a$categories)))})
 outputOptions(output,'category_choice',suspendWhenHidden=FALSE)
 counts<-reactive({a<-category_data();do.call(rbind,lapply(a$counts,unlist))})
 survey_reference<-reactive({
  key<-input$variable
  pop<-d$numeric[[key]];req(pop)
  mu<-input[[paste0('survey_mu_',key)]]
  sigma<-input[[paste0('survey_sigma_',key)]]
  if(is.null(mu)||isTRUE(mu==round(pop$mu,4)))mu<-pop$mu
  if(is.null(sigma)||isTRUE(sigma==round(pop$sigma,4)))sigma<-pop$sigma
  validate(need(is.finite(mu),'請輸入有效的母體平均數 μ₀。'),
   need(is.finite(sigma)&&sigma>0,'母體標準差 σ 必須大於 0。'))
  list(mu=mu,sigma=sigma)
 })
 results<-reactive({al<-alpha()
  if(input$kind=='numeric'){pop<-d$numeric[[input$variable]];req(pop);ref<-survey_reference();do.call(rbind,lapply(seq_along(pop$groups),function(i){g<-pop$groups[[i]];a<-do.call(rbind,lapply(c('z','t'),function(m)mean_result(g$n,g$mean,g$sd,ref$mu,ref$sigma,m,al,input$tail)));a$z<-i;a}))
  }else{req(input$event);m<-counts();j<-as.integer(input$event);req(j %in% seq_len(ncol(m)));p0<-sum(m[,j])/sum(m);do.call(rbind,lapply(1:12,function(i){a<-prop_result(m[i,j],sum(m[i,]),p0,al,input$tail);a$z<-i;a}))}
 })
 pooled_results<-reactive({
  al<-alpha()
  if(input$kind=='numeric'){
   pop<-d$numeric[[input$variable]];req(pop)
   ref<-survey_reference()
   s<-pop$sigma*sqrt(pop$N/(pop$N-1))
   a<-do.call(rbind,lapply(c('z','t'),function(m)mean_result(pop$N,pop$mu,s,ref$mu,ref$sigma,m,al,'two')))
  }else{
   m<-counts();req(input$event);j<-as.integer(input$event);req(j %in% seq_len(ncol(m)))
   x<-sum(m[,j]);n<-sum(m)
   a<-prop_result(x,n,x/n,al,'two')
  }
  a[c('se','critical','stat','p','reject')]<-NA
  a$z<-0L
  a
 })
 all_results<-reactive(rbind(pooled_results(),results()))
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
  if(input$kind=='numeric')note('μ₀ 與 σ 預設採用全體資料，可自行修改。z 法使用指定的 σ，t 法使用樣本標準差。CI 固定採雙側區間。')
  else note('比例 CI 固定採雙側區間，以樣本比例 p̂ 估計標準誤。單一比例 z 檢定以全體參考比例 p₀ 計算標準誤。')
 })
 output$chart_legend<-renderUI({
  numeric<-input$kind=='numeric'
  p(class='legend',span(style='color:#2563eb',if(numeric)'● ━ z' else '● ━ 比例 CI'),
   if(numeric)span(style='color:#0891b2','◆ ┄ t'),
   if(numeric)'黑色實線：μ' else '黑色實線：p₀',
   span(style='color:#c32d3b','紅色：CI 未涵蓋參考值'))
 })
 selected_id<-reactive({req(input$zodiac);as.integer(input$zodiac)})
 selected_results<-reactive({
  id<-selected_id()
  if(id==0L)return(pooled_results())
  a<-results()
  a[a$z==id,]
 })
 output$survey_chart<-renderUI({
  a<-if(isTRUE(input$show_all_groups))all_results() else selected_results()
  HTML(ci_svg(a,z_labels,input$kind=='category',selected_id()))
 })
 output$survey_ci_title<-renderText({
  paste0(fmt((1-alpha())*100,1),'% 雙側',if(input$kind=='category')'比例','信賴區間計算')
 })
 output$survey_ci_steps<-renderUI({
  a<-selected_results()
  selected<-selected_id()
  al<-alpha()
  variable<-if(input$kind=='category')category_data()$label else d$numeric[[input$variable]]$label
  sample_heading<-h4(class='survey-sample-heading',strong(z_labels[selected+1L]),span(paste0('｜',variable)))
  if(input$kind=='category'){
   event<-as.integer(input$event)
   successes<-if(selected==0L)sum(counts()[,event]) else counts()[selected,event]
   event_label<-category_data()$categories[[event]]
   n<-a$n
   phat<-fmt(a$mean,4)
   zcv<-fmt(a$ci_critical,4)
   ci_se<-fmt(a$ci_se,4)
   return(tagList(
    sample_heading,
    p(paste0('成功事件：',event_label)),
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
  tagList(
   sample_heading,
   p(paste0('n = ',a$n[1],'｜x̄ = ',fmt(a$mean[1],4))),
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
     p(paste0('SE = ',spread_label,'/√n = ',fmt(spread,4),'/√',r$n,' = ',fmt(r$ci_se,4))),
     p(paste0('CI = x̄ ± ',if(is_z)'zcv' else 'tcv(df)',' × SE')),
     p(paste0('= ',fmt(r$mean,4),' ± ',fmt(r$ci_critical,4),' × ',fmt(r$ci_se,4),
      ' = [',fmt(r$lower,4),', ',fmt(r$upper,4),']'))
    )
   }),
   note('端點以未四捨五入的數值計算。')
  )
 })
 output$survey_result<-renderUI({
  if(selected_id()==0L)return(note('全體樣本提供估計值與信賴區間。選擇個別星座即可檢定。'))
  a<-selected_results()
  zodiac_name<-z_names[selected_id()]
  param<-paste0(if(input$kind=='numeric')'μ' else 'π',zodiac_name)
  tagList(
   h4(paste0(input$zodiac,'. ',zodiac_name)),
   hypotheses(input$tail,a$mu[1],param),
   lapply(seq_len(nrow(a)),function(i){
    r<-a[i,]
    statistic<-paste0(r$method,'_obt')
    steps<-if(input$kind=='numeric'){
     is_t<-r$method=='t'
     spread<-if(is_t)r$sd else r$sigma
     spread_symbol<-if(is_t)'s' else 'σ'
     distribution<-if(is_t)paste0('T(',r$df,')') else 'Z'
     observed<-fmt(r$stat,4)
     probability<-switch(input$tail,
      two=paste0('2 × P(',distribution,' ≥ ',fmt(abs(r$stat),4),')'),
      left=paste0('P(',distribution,' ≤ ',observed,')'),
      right=paste0('P(',distribution,' ≥ ',observed,')'))
     tagList(
      p(paste0('SE = ',spread_symbol,' / √n = ',fmt(spread,4),' / √',r$n,' = ',fmt(r$se,4))),
      if(is_t)p(paste0('df = n − 1 = ',r$n,' − 1 = ',r$df)),
      p(paste0(statistic,' = (x̄ − μ₀) / SE')),
      p(paste0('= (',fmt(r$mean,4),' − ',fmt(r$mu,4),') / ',fmt(r$se,4),' = ',observed)),
      p(paste0('p = ',probability,if(r$p<.0001)' ' else ' = ',p_fmt(r$p))),
      p(paste0('p ',if(r$reject)'<' else '≥',' α = ',fmt(alpha(),3))))
    }else p('假設檢定標準誤 = ',fmt(r$se,4),'；',statistic,' = ',fmt(r$stat,4),'；p = ',p_fmt(r$p))
    div(class='result-block',h4(paste0(statistic,'（',r$method,'-statistic）')),
     steps,
     div(class=if(r$reject)'decision reject' else 'decision',if(r$reject)'拒絕 H₀' else '保留 H₀'))
   }),
   if(input$kind=='category'&&any(!a$approxOK))p(class='warning','n·p₀ 或 n·(1−p₀) < 5，比例 z 常態近似可能不適合。')
  )
 })
 output$survey_diagram<-renderUI({
  if(selected_id()==0L)return(NULL)
  a<-selected_results()
  al<-alpha()
  if(input$kind=='numeric')comparison_ui(a,al,input$tail)
  else diagram_ui(a,al,input$tail,'z',h0=null_label(input$tail,a$mu,'π'))
 })
 output$survey_table<-renderUI({
  a<-all_results()
  tab<-cbind(星座=z_labels[a$z+1L],result_table(a))
  HTML(html_table(tab,a$reject %in% TRUE))
 })
 output$code_example<-renderUI({
  req(input$code_lang)
  topic<-code_topic()
  if(topic[1]=='book'){
   req(input$example==topic[2])
   b<-book()
   title<-b$title
   if(b$type=='mean'){
    a<-book_test()
    a<-a[a$method==input$bm,]
    scripts<-inference_scripts(a,input$ba,if(book_testing())input$bt else 'two',values=unlist(b$values))
    result<-HTML(html_table(result_table(a),a$reject %in% TRUE))
   }else{
    result<-book_result()
    scripts<-book_exercise_scripts(b,as.integer(input$bn),input$ba)
    if(b$type=='sampling')result<-tagList(note('重新執行語法會重新隨機抽樣。'),result)
   }
  }else{
   req(input$kind==topic[1],input$variable==topic[2])
   a<-selected_results()
   id<-selected_id()
   successes<-NULL
   if(input$kind=='category'){
    m<-counts();j<-as.integer(input$event)
    successes<-if(id==0L)sum(m[,j]) else m[id,j]
    variable<-paste0(category_data()$label,'｜',category_data()$categories[[j]])
   }else variable<-d$numeric[[input$variable]]$label
   title<-paste(z_labels[id+1L],variable,sep='｜')
   scripts<-inference_scripts(a,alpha(),input$tail,successes)
   result<-HTML(html_table(result_table(a),a$reject %in% TRUE))
  }
  ext<-if(input$code_lang=='r')'R' else 'py'
  language<-if(input$code_lang=='r')'R' else 'Python'
  stem<-paste(c(topic[2],if(topic[1]!='book')c(paste0('sample-',id),if(input$kind=='category')paste0('event-',j))),collapse='-')
  files<-analysis_download(scripts,input$code_lang,stem,title)
  filename<-paste0(stem,'.',ext)
  command<-paste(if(input$code_lang=='r')'Rscript' else 'python',filename)
  console<-if(input$code_lang=='r'){
   runner<-new.env()
   runner$read.csv<-function(file,...){
    stopifnot(identical(file,files$data_file))
    con<-textConnection(files$csv,encoding='UTF-8')
    on.exit(close(con))
    utils::read.csv(con,...)
   }
   script<-textConnection(files$code,encoding='UTF-8')
   on.exit(close(script),add=TRUE)
   transcript<-capture.output(source(script,local=runner,echo=TRUE,print.eval=TRUE,
    prompt.echo='> ',continue.echo='+ ',max.deparse.length=Inf))
   tags$pre(class='analysis-code analysis-console',paste(transcript,collapse='\n'))
  }else tagList(
   tags$button(type='button',class='btn btn-default',onclick='runAnalysisPython(this)','執行 Python'),
   tags$pre(class='analysis-code analysis-console python-console',role='status','執行後會顯示原始輸出。'),
   div(class='python-help',hidden=NA,
    p('若錯誤出現在載入階段，請重新整理教材後再試。也可換用新版 Chrome、Edge 或 Safari，或切換到能連線至 cdn.jsdelivr.net 的網路。'),
    p('若仍無法執行，請下載 Python 語法與 CSV，放在同一個資料夾。需要 SciPy 的題目，先在該資料夾的終端機安裝套件，再執行下列指令。'),
    tags$pre('python -m pip install scipy'),tags$pre(command)))
  tagList(h4(title),
   div(class='code-columns',
    div(h5(paste('可執行的',language,'語法')),
     div(class='code-actions',
      tags$button(type='button',class='btn btn-primary',onclick='copyAnalysisCode(this)',paste('複製',language,'語法')),
      tags$button(type='button',class='btn btn-default',onclick='downloadAnalysisCode(this)',`data-filename`=filename,paste0('下載 .',ext)),
      tags$button(type='button',class='btn btn-default',onclick='downloadAnalysisData(this)',
       `data-filename`=files$data_file,'下載資料 CSV')),
     p('將語法與 CSV 放在同一資料夾，在該資料夾的終端機執行。'),
     p(tags$code(command)),
     tags$pre(class='analysis-code',tags$code(files$code)),
     tags$pre(class='analysis-data',hidden=NA,files$csv)),
    div(h5(paste(language,'主控台原始輸出')),console,
     tags$details(tags$summary('整理後的分析結果'),result))))
 })
 observeEvent(input$save_csv,{
  a<-all_results()
  tab<-cbind(星座=z_labels[a$z+1L],a)
  names(tab)[match(c('ci_se','se'),names(tab))]<-c('信賴區間標準誤','假設檢定標準誤')
  lines<-capture.output(write.csv(tab,row.names=FALSE))
  session$sendCustomMessage('download-file',list(name=paste0(input$variable,'-results.csv'),mime='text/csv;charset=utf-8',content=paste0('\ufeff',paste(lines,collapse='\r\n'))))
 })

}
shinyApp(ui,server)
