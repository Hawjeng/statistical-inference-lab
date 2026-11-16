# Produce a script and the CSV needed to reproduce its analysis.
inference_scripts <- function(a, alpha, tail, successes=NULL, values=NULL) {
 raw<-!is.null(values)
 data<-if(raw)data.frame(value=values) else data.frame(
  method=a$method,n=a$n,estimate=a$mean,sample_sd=a$sd,population_sd=a$sigma,
  reference=ifelse(is.na(a$stat),NA_real_,a$mu),
  successes=if(is.null(successes))NA_real_ else successes)
 rnum<-function(x)if(is.na(x))'NA_real_' else sprintf('%.17g',x)
 r<-c(
  '# 1. Read the accompanying CSV from the current working directory.',
  'data <- read.csv(DATA_FILE)',
  '',
  '# 2. Set the confidence level and direction of the hypothesis test.',
  sprintf('alpha <- %s',alpha),sprintf('tail <- "%s"',tail),
  if(raw)c(
   '# Calculate the sample summaries from the original observations.',
   sprintf('groups <- data.frame(method="%s", n=length(data$value), estimate=mean(data$value),',a$method),
   sprintf('  sample_sd=sd(data$value), population_sd=%s, reference=%s, successes=NA_real_)',
    rnum(a$sigma),rnum(if(is.na(a$stat))NA_real_ else a$mu))
  ) else c('# The CSV contains the survey summaries or success counts used in the calculations.','groups <- data'),
  '',
  '# 3. Calculate a two-sided confidence interval for each method.',
  'for (i in seq_len(nrow(groups))) {',
  '  g <- groups[i, ]',
  '  n <- g$n',
  '  use_t <- g$method == "t"',
  '  quantile <- if (use_t) function(p) qt(p, n - 1) else qnorm',
  '  cdf <- if (use_t) function(x) pt(x, n - 1) else pnorm',
  '',
  '  if (!is.na(g$successes)) {',
  '    # Wald interval for a proportion. The test uses the null proportion.',
  '    estimate <- g$successes / n',
  '    ci_se <- sqrt(estimate * (1 - estimate) / n)',
  '    test_se <- sqrt(g$reference * (1 - g$reference) / n)',
  '  } else {',
  '    # Mean interval: use the population SD for z and sample SD for t.',
  '    estimate <- g$estimate',
  '    sd_used <- if (use_t) g$sample_sd else g$population_sd',
  '    ci_se <- sd_used / sqrt(n)',
  '    test_se <- ci_se',
  '  }',
  '  margin <- quantile(1 - alpha / 2) * ci_se',
  '  result <- data.frame(method=g$method, n=n, estimate=estimate,',
  '    ci_se=ci_se, lower=estimate-margin, upper=estimate+margin)',
  '',
  '  # 4. Test against the reference value when one is specified.',
  '  if (!is.na(g$reference)) {',
  '    stat <- (estimate - g$reference) / test_se',
  '    p <- switch(tail, two=2*cdf(-abs(stat)), left=cdf(stat), right=cdf(-stat))',
  '    critical <- quantile(1 - alpha / if (tail == "two") 2 else 1)',
  '    result <- transform(result, test_se=test_se, statistic=stat,',
  '      critical=critical, p=p, reject=p<alpha)',
  '  }',
  '  print(result, row.names=FALSE)',
  '}')
 py<-c(
  '# Install the dependency once: python -m pip install scipy',
  'import csv','from math import sqrt','from statistics import mean, stdev','from scipy.stats import norm, t','',
  '# 1. Read the accompanying CSV from the current working directory.',
  'with open(DATA_FILE, encoding="utf-8-sig", newline="") as file:',
  '    data = list(csv.DictReader(file))',
  '',
  '# 2. Set the confidence level and direction of the hypothesis test.',
  paste0('alpha = ',alpha),sprintf('tail = "%s"',tail),
  if(raw)c(
   '# Calculate the sample summaries from the original observations.',
   'values = [float(row["value"]) for row in data]',
   sprintf('groups = [dict(method="%s", n=len(values), estimate=mean(values),',a$method),
   sprintf('               sample_sd=stdev(values), population_sd=%s, reference=%s, successes="")]',
    a$sigma,if(is.na(a$stat))'""' else sprintf('%.17g',a$mu))
  ) else c('# Each row contains the survey summaries or success counts for one method.','groups = data'),
  '',
  '# 3. Calculate a two-sided confidence interval for each method.',
  'for group in groups:',
  '    n = int(group["n"])',
  '    use_t = group["method"] == "t"',
  '    distribution = t(df=n-1) if use_t else norm',
  '    reference = float(group["reference"]) if group["reference"] != "" else None',
  '    if group["successes"] != "":',
  '        # Wald interval for a proportion. The test uses the null proportion.',
  '        estimate = int(group["successes"]) / n',
  '        ci_se = sqrt(estimate * (1 - estimate) / n)',
  '        test_se = sqrt(reference * (1 - reference) / n) if reference is not None else None',
  '    else:',
  '        # Mean interval: use the population SD for z and sample SD for t.',
  '        estimate = float(group["estimate"])',
  '        sd_used = float(group["sample_sd"] if use_t else group["population_sd"])',
  '        ci_se = sd_used / sqrt(n)',
  '        test_se = ci_se',
  '    margin = distribution.ppf(1 - alpha / 2) * ci_se',
  '    result = dict(method=group["method"], n=n, estimate=estimate, ci_se=ci_se,',
  '                  lower=estimate-margin, upper=estimate+margin)',
  '',
  '    # 4. Test against the reference value when one is specified.',
  '    if reference is not None:',
  '        stat = (estimate - reference) / test_se',
  '        p = {"two": 2*distribution.sf(abs(stat)), "left": distribution.cdf(stat),',
  '             "right": distribution.sf(stat)}[tail]',
  '        critical = distribution.ppf(1 - alpha / (2 if tail == "two" else 1))',
  '        result.update(test_se=test_se, statistic=stat, critical=critical, p=p, reject=p<alpha)',
  '    print(result)')
 list(r=r,python=py,data=data)
}

book_exercise_scripts <- function(b, n, alpha) {
 r_read<-'data <- read.csv(DATA_FILE, fileEncoding="UTF-8")'
 py_read<-c('import csv','with open(DATA_FILE, encoding="utf-8-sig", newline="") as file:',
  '    data = list(csv.DictReader(file))')
 if(b$type=='critical'){
  return(list(
   data=data.frame(alpha=unique(c(alpha,.1,.05,.01,.001))),
   r=c('# Read the significance levels and calculate standard normal cutoffs.',r_read,
    'alpha <- data$alpha',
    'print(data.frame(alpha, left=qnorm(alpha), right=qnorm(1-alpha),',
    '                 two_sided=qnorm(1-alpha/2)), row.names=FALSE)'),
   python=c('# Install the dependency once: python -m pip install scipy','from scipy.stats import norm',
    '# Read the significance levels from the CSV.',py_read,
    '# Report the left, right and positive two-sided critical values.',
    'for row in data:','    alpha = float(row["alpha"])',
    '    print(alpha, norm.ppf(alpha), norm.isf(alpha), norm.isf(alpha/2))')))
 }
 if(b$type=='sampling'){
  return(list(
   data=data.frame(id=unlist(b$ids)),
   r=c('# Read the roster, preserving identifiers as strings.',sub('fileEncoding','colClasses="character", fileEncoding',r_read,fixed=TRUE),
    'ids <- data$id',sprintf('n <- %s',n),
    '# Simple random sample without replacement.','print(sample(ids, n))',
    '# Systematic sample with a random start and spacing N/n.',
    'k <- length(ids) / n','start <- runif(1, 0, k)',
    'print(ids[floor(start + (seq_len(n) - 1) * k) + 1])'),
   python=c('from random import random, sample','# Read the roster identifiers.',py_read,
    'ids = [row["id"] for row in data]',sprintf('n = %s',n),
    '# Simple random sample without replacement.','print(sample(ids, n))',
    '# Systematic sample with a random start and spacing N/n.',
    'k = len(ids) / n','start = random() * k','print([ids[int(start + i*k)] for i in range(n)])')))
 }
 strata<-do.call(rbind,lapply(b$strata,as.data.frame))
 list(data=data.frame(stratum=strata$label,population=strata$N),
  r=c('# Read the population count for each stratum.',r_read,sprintf('n <- %s',n),
   '# Allocate in proportion to population size, rounding down first.',
   'quota <- n * data$population / sum(data$population)','allocation <- floor(quota)',
   '# Assign the remaining observations to the largest fractional remainders.',
   'extra <- head(order(quota - allocation, decreasing=TRUE), n - sum(allocation))',
   'allocation[extra] <- allocation[extra] + 1',
   'print(transform(data, quota=quota, allocation=allocation), row.names=FALSE)'),
  python=c('# Read the population count for each stratum.',py_read,sprintf('n = %s',n),
   'population = [int(row["population"]) for row in data]',
   '# Allocate in proportion to population size, rounding down first.',
   'quota = [n * size / sum(population) for size in population]',
   'allocation = [int(q) for q in quota]',
   '# Assign the remaining observations to the largest fractional remainders.',
   'order = sorted(range(len(data)), key=lambda i: quota[i]-allocation[i], reverse=True)',
   'for i in order[:n-sum(allocation)]:','    allocation[i] += 1',
   'for row, q, count in zip(data, quota, allocation):',
   '    print(row["stratum"], row["population"], q, count)'))
}

analysis_download <- function(scripts, lang, stem, title) {
 data_file<-paste0(stem,'.csv')
 code<-gsub('DATA_FILE',sprintf('"%s"',data_file),scripts[[lang]],fixed=TRUE)
 list(
  code=paste(c(paste0('# ',title),'# Open a terminal in the folder containing this script and the CSV.',code),collapse='\n'),
  data_file=data_file,
  csv=paste(capture.output(write.csv(scripts$data,row.names=FALSE,na='')),collapse='\n'))
}
