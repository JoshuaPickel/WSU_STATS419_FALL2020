# Custom Summary function
doSummary = function(x)
{
  Length = length(x)
  naNum = sum(is.na(x))
  Mean = mean(as.numeric(Filter(is.numeric,x)))
  Mode = doMode(x)
  NaiveVariance = doSampleVariance(x,'naive')
  print(NaiveVariance)
  TwoPassVariance = doSampleVariance(x,'Two Pass')
  Sd = sqrt(TwoPassVariance$variance)
  results =
    data.frame(Mode,Mean,naNum,Length,TwoPassVariance$variance,NaiveVariance$variance,Sd)
  results;
}
# Custom Mode Function
doMode = function(x)
{
  ModeFrame = as.data.frame(x[1])
  num_vals = as.data.frame(table(as.numeric(c(Filter(is.numeric,x)))))
  Max = max(num_vals$Freq)
  Mode = (num_vals$Var1[num_vals$Freq == Max])
  if (length(Mode) > 1)
  {
    for (i in Mode)
    {
      colname = paste('mode' , i, sep = '')
      ModeFrame$blank = i
      names(ModeFrame)[names(ModeFrame) == 'blank'] = colname
    }
  }
  else
  {
    ModeFrame$mode = Mode
  }
  ModeFrame;
}
# Custom Variance Function
doSampleVariance = function(x, method)
{
  num_vals = as.data.frame(as.numeric(c(Filter(is.numeric,x))))
  Mean = mean(as.numeric(Filter(is.numeric,x)))
  len = nrow(num_vals)
  if (method == 'naive')
  {
    n = 0
    Sum = 0
    SumSq = 0
    for(i in 1:nrow(num_vals))
    {
      n = n + 1
      Sum = Sum + num_vals[i,1]
      SumSq = SumSq + num_vals[i,1] * num_vals[i,1]
    }
    if (n < 2) {return(NULL);}
    variance = (SumSq - (Sum * Sum)/n)/(n-1)
    result = data.frame(SumSq, Sum, variance)
  }
  else
  {
    n = 0
    Sum1 = 0
    Sum2 = 0
    for(i in 1:nrow(num_vals))
    {
      n = n + 1
      Sum1 = Sum1 + num_vals[i,1]
    }
    Mean = Sum1/n
    for (i in 1:nrow(num_vals))
    {
      Sum2 = Sum2 + (num_vals[i,1] - Mean) * (num_vals[i,1] - Mean)
    }
    if (n < 2) {return(NULL);}
    variance = Sum2/(n-1)
    result = data.frame(Sum1, Sum2, variance)
  }
  result
}

zScores = function(x)
{
  num_vals = num_vals = as.numeric(c(Filter(is.numeric,x)))
  dsv = doSampleVariance(x,"two pass")
  x_bar = mean(num_vals)
  s_hat = sqrt(dsv$variance)
  z = (num_vals - x_bar) / s_hat
  list("z.score"=z,"raw.scores" = num_vals);
}
