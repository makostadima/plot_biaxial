library(tercen)

getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "f18104d6-6d29-47ce-80be-c24681a08117",
                   workflowId = "abe673836a3f732410737d630179dffd")
  return(ctx)
}

custom_log10 = function(breaks) {
  breaks <-
    sapply(breaks, function(x)
      if (x > 0)
        log10(x)
      else-log10(abs(x)))
  math_format(10 ^ .x)(breaks)
}

getValues <- function(session) {
  ctx <- getCtx(session)
  
  df <- ctx %>% select(.x, .y, .ri, .ci) %>%
    group_by(.ri)
  
  print(head(df))
  
  colors <- 0
  if (length(ctx$colors))
    colors <- ctx$select(ctx$colors[[1]])[[1]]
  if (!all(is.numeric(colors)))
    colors = as.factor(colors)
  
  tcn.labels <- NA
  sizes = NA
  if (length(ctx$labels))
    tcn.labels <- ctx$select(ctx$labels[[1]])[[1]]
  if (!all(is.numeric(tcn.labels))) {
    labels = as.factor(tcn.labels)
  } else {
    sizes = tcn.labels
  }
  
  df = df %>% data.frame(labels, colors, sizes)
  
  cnames = ctx$cselect()[[1]]
  
  rnames = ctx$rselect()[[1]]
  df = df %>%
    mutate(colors = colors, labels = labels) %>%
    left_join(data.frame(.ri = 0:(length(rnames) - 1), rnames), by = ".ri") %>%
    left_join(data.frame(.ci = 0:(length(cnames) - 1), cnames), by = ".ci")
  
  df$colors <- factor(df$colors, levels = levels(df$colors))
  
  return(df)
}

 ## Code taken from
custom_logicle_breaks <- function(x, n = 6, equal.space = FALSE, trans.fun, inverse.fun) {
  
  rng.raw <- range(x, na.rm = TRUE)
  x.trans <- trans.fun(x)
  x.inv.trans <- inverse.fun(x.trans)
  
  v_min_max = c(minimum = x.inv.trans[1], 
                maximum = x.inv.trans[length(x.inv.trans)])
  
  log_min_max = c()
  for (i in c("minimum", "maximum")) {
    value = v_min_max[i]
    if ( i == "minimum" && sign(value) == 1 ) {
      log_min_max[i] = sign(value)*floor(log10(abs(value)))
    } else {
      log_min_max[i] = sign(value)*ceiling(log10(abs(value)))
    }
  }
  power = log_min_max["minimum"]:log_min_max["maximum"]
  sn = sign(power)
  pow = abs(power)
  
  decades = sn*10^pow; # node that decades includes 0. e.g.: -100, -10, -1, 0, 1, 10.
  decades.trans = trans.fun(decades)
  n_decades = length(decades)
  n_ticks = (n_decades-1)*9 + 1 + 2*sum(decades==0) #if we have 0 included in our decades, add 2 to the number of ticks because we will tick at -1 and 1
  obj.Tick = rep(0, n_ticks)
  obj.TickLabel = rep('', n_ticks)
  tick_index = 1
  previous_labeled_decade=NA
  
  for(k in 1:n_decades) {
    if ( !is.na(previous_labeled_decade) && decades.trans[k]-decades.trans[previous_labeled_decade] < 0.02) { # if the distance between this decade and the last is less than 0.02, do not label this decade because we may overlap the labels
      obj.TickLabel[tick_index] = ''
    } else {
      if (sn[k] == 0) {
        obj.TickLabel[tick_index] = '0'
      } else {
        if (sn[k] == -1) {
          sign_string = '-'
        } else {
          sign_string = ''
        }
        obj.TickLabel[tick_index] = paste(sign_string, "10^", as.character(pow[k]), sep="") 
      }
      previous_labeled_decade = k
    }
    
    if (k == n_decades) {
      # write Tick for final decade
      obj.Tick[tick_index] = decades.trans[k]
      # Fill any subsequent ticks
      # which may be labelled '' so that the Tick vector is
      # monotonically increasing;
      if (tick_index < n_ticks) {
        obj.Tick[(tick_index+1):length(obj.Tick)] = seq(obj.Tick[tick_index]+0.1, 
                                                        obj.Tick[tick_index]+0.2, 
                                                        length.out = n_ticks - tick_index)
      }
      break;
    }
    
    # write Tick for this decade in 9 increments if the ends of
    # the decades are powers of 10 increments if the right hand
    # end of the gap is 0 (i.e.10^{-inf})
    if (decades[k+1] == 0) {
      n_increments = 11
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else if (decades[k]==0) {
      n_increments = 9
      lhs = 1
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    } else {
      n_increments = 9
      lhs = decades[k]
      rhs = decades[k+1] - min(abs(c(lhs,decades[k+1])))
    }
    obj.Tick[tick_index:(tick_index+n_increments-1)] = trans.fun(seq(from = lhs, to = rhs, length.out = n_increments))
    
    # write empty TickLabel for the next 8 or 9 linear
    # increments within this decade
    for (i in (tick_index+1):(tick_index+n_increments-1)){
      obj.TickLabel[i] = '';
    }
    
    tick_index = tick_index + n_increments;
  }
  
  return(list(obj.Tick, obj.TickLabel))
}
