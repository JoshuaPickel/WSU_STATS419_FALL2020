convertDates = function(frame,column)
  {
  frame$week = strftime(as.Date(column,format="%m/%d/%Y %H:%M"), format = "%V")
  frame$year = strftime(as.Date(column,format="%m/%d/%Y %H:%M"), format = "%Y")
  yearIndx = which(colnames(frame) == 'year')
  weekIndx = which(colnames(frame) == 'week')
  frame = frame[order(frame[,yearIndx],frame[, weekIndx],decreasing = T),]
  frame;
  }

removeColumn = function(frame,column)
  {
  frame[column] = NULL
  frame;
  }

removeDuplicates = function(frame,column)
  {
  frame = frame[match(unique(column),column),]
  frame;
  }


  