# Build standalone scripts from the same inputs used by the interactive analysis.
inference_scripts <- function(a, alpha, tail, successes=NULL, values=NULL) {
 rows<-lapply(seq_len(nrow(a)),function(i){
  r<-a[i,]
  list(method=r$method,n=r$n,estimate=r$mean,sd=r$sd,sigma=r$sigma,
   reference=if(is.na(r$stat))NA_real_ else r$mu,successes=if(is.null(successes))NA_real_ else successes)
 })
 json<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,na='null',digits=NA))
 rnum<-function(x)if(is.null(x)||is.na(x))'NA_real_' else sprintf('%.17g',x)
 rrows<-vapply(rows,function(r)sprintf("  list(method='%s', n=%s, estimate=%s, s=%s, sigma=%s, ref=%s, x=%s)",
  r$method,rnum(r$n),rnum(r$estimate),rnum(r$sd),rnum(r$sigma),rnum(r$reference),rnum(r$successes)),character(1))
 r<-c(sprintf("alpha <- %s",alpha),sprintf("tail <- '%s'",tail),
  if(!is.null(values))paste0('values <- c(',paste(values,collapse=', '),')'),
  'groups <- list(',paste(rrows,collapse=',\n'),')',
  'for (g in groups) {',
  '  n <- g$n',
  '  estimate <- g$estimate',
  if(!is.null(values))'  n <- length(values); estimate <- mean(values); g$s <- sd(values)',
  '  is_t <- g$method == "t"',
  '  quantile <- if (is_t) function(p) qt(p, n-1) else qnorm',
  '  cdf <- if (is_t) function(x) pt(x, n-1) else pnorm',
  '  if (!is.na(g$x)) {',
  '    estimate <- g$x/n',
  '    ci_se <- sqrt(estimate*(1-estimate)/n)',
  '    test_se <- sqrt(g$ref*(1-g$ref)/n)',
  '  } else {',
  '    ci_se <- (if (is_t) g$s else g$sigma)/sqrt(n)',
  '    test_se <- ci_se',
  '  }',
  '  margin <- quantile(1-alpha/2)*ci_se',
  '  out <- data.frame(method=g$method, n=n, estimate=estimate,',
  '                    ci_se=ci_se, lower=estimate-margin, upper=estimate+margin)',
  '  if (!is.na(g$ref)) {',
  '    stat <- (estimate-g$ref)/test_se',
  '    p <- switch(tail, two=2*cdf(-abs(stat)), left=cdf(stat), right=cdf(-stat))',
  '    out <- transform(out, test_se=test_se, statistic=stat, p=p, reject=p<alpha)',
  '  }',
  '  print(out, row.names=FALSE)',
  '}')
 py<-c('# Install dependency: python -m pip install scipy','import json','from math import sqrt',
  'from statistics import mean, stdev','from scipy.stats import norm, t','',
  paste0('alpha = ',alpha),paste0('tail = ',json(tail)),
  if(!is.null(values))paste0('values = [',paste(values,collapse=', '),']'),
  paste0("groups = json.loads('",json(rows),"')"),
  'for g in groups:',
  '    n, estimate = g["n"], g["estimate"]',
  if(!is.null(values))'    n, estimate, g["sd"] = len(values), mean(values), stdev(values)',
  '    dist = t(df=n-1) if g["method"] == "t" else norm',
  '    ref = g["reference"]',
  '    if g["successes"] is not None:',
  '        estimate = g["successes"]/n',
  '        ci_se = sqrt(estimate*(1-estimate)/n)',
  '        test_se = sqrt(ref*(1-ref)/n) if ref is not None else None',
  '    else:',
  '        ci_se = (g["sd"] if g["method"] == "t" else g["sigma"])/sqrt(n)',
  '        test_se = ci_se',
  '    margin = dist.ppf(1-alpha/2)*ci_se',
  '    result = dict(method=g["method"], n=n, estimate=estimate, ci_se=ci_se,',
  '                  lower=estimate-margin, upper=estimate+margin)',
  '    if ref is not None:',
  '        stat = (estimate-ref)/test_se',
  '        p = {"two": 2*dist.sf(abs(stat)), "left": dist.cdf(stat), "right": dist.sf(stat)}[tail]',
  '        result.update(test_se=test_se, statistic=stat, p=p, reject=p<alpha)',
  '    print(result)')
 list(r=paste(r,collapse='\n'),python=paste(py,collapse='\n'))
}

book_exercise_scripts <- function(b, n, alpha) {
 if(b$type=='critical'){
  levels<-unique(c(alpha,.1,.05,.01,.001))
  args<-paste(levels,collapse=', ')
  return(list(
   r=sprintf('alpha <- c(%s)\nprint(data.frame(alpha, left=qnorm(alpha), right=qnorm(1-alpha), two=qnorm(1-alpha/2)))',args),
   python=sprintf('# Install dependency: python -m pip install scipy\nfrom scipy.stats import norm\nfor alpha in [%s]:\n    print(alpha, norm.ppf(alpha), norm.isf(alpha), norm.isf(alpha/2))',args)))
 }
 if(b$type=='sampling'){
  ids<-paste(as.character(jsonlite::toJSON(unlist(b$ids),auto_unbox=FALSE)))
  rids<-paste(vapply(as.character(unlist(b$ids)),encodeString,character(1),quote='"'),collapse=', ')
  return(list(
   r=sprintf('ids <- c(%s)\nn <- %s\nk <- length(ids)/n\nprint(sample(ids, n))\nprint(ids[floor(runif(1, 0, k)+(0:(n-1))*k)+1])',rids,n),
   python=sprintf('from random import random, sample\nids = %s\nn = %s\nk = len(ids)/n\nprint(sample(ids, n))\nstart = random()*k\nprint([ids[int(start + i*k)] for i in range(n)])',ids,n)))
 }
 strata<-do.call(rbind,lapply(b$strata,as.data.frame))
 pop<-paste(strata$N,collapse=', ')
 list(
  r=sprintf('N <- c(%s)\nn <- %s\nquota <- n*N/sum(N)\nallocation <- floor(quota)\nextra <- head(order(quota-allocation, decreasing=TRUE), n-sum(allocation))\nallocation[extra] <- allocation[extra]+1\nprint(data.frame(N, quota, allocation))',pop,n),
  python=sprintf('N = [%s]\nn = %s\nquota = [n*size/sum(N) for size in N]\nallocation = [int(q) for q in quota]\norder = sorted(range(len(N)), key=lambda i: quota[i]-allocation[i], reverse=True)\nfor i in order[:n-sum(allocation)]:\n    allocation[i] += 1\nfor row in zip(N, quota, allocation):\n    print(*row)',pop,n))
}
