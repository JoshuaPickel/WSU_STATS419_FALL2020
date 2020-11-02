
# this is "not" recursive at the moment ...
createDir = function (folder)
  {
  ifelse(!dir.exists(folder), dir.create(folder), "Folder exists already");
  }

# logging functions ... functions-log.R


<<<<<<< HEAD
storeToFile = function (str,myfile)
	{
	cat(str, file=myfile,append=F);	
	}
	
	
=======
>>>>>>> 1ae200379d183d9f3b3c5347a5519ae0e90837c8
