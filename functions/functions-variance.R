doSummary = function(x)
{
  # length
  # number of NAs
  # mean
  # median
  # mode # custom function
  # variance # custom function ...
  # sd ... built in function but compare it to the custom function ...

  varN = doSampleVariance(x,"naive");
  var2 = doSampleVariance(x,"jdfkldsjfklj");

  #  x
  #  z = (x-xc)/oc ... xc = mean ... oc = sd ... linear ... ...
  # x,y ... plot ( cbind(x,y) ) ;  .... # corr(x,y); # 1
  # abline(v=0);
  # ... one big vector ... hist(everything) ...


}

doSampleVariance = function(x, method)
{
  if(method=="naive")
  {


  }
  else
  {
    # two-pass algorithm # testing

  }


}

doMode = function(x)
{
  result = c();
  # freq ... # high freqencies
  # ties ... store all of the ties
  # which.min() or which.max()

  result;
}
