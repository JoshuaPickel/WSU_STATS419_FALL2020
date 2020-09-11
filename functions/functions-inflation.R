# data from 1920 to 2020 ... 101 years ...
# bigger dollar gives a more accurate percent ...

# read the values in "year"/"dollars" using rvest ...
library(rvest);	

grabInflation = function(){
infl = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";
  
infl.html = read_html(infl);

infl.table = infl.html %>%
  html_node(".expand-table-parent") %>%
  html_node(".table-striped") %>%
  html_node("tbody") %>%
  html_nodes("tr");

result = data.frame( matrix(nrow=length(infl.table), ncol=3));
colnames(result) = c("year","dollar","inflation");

for(i in 1:length(infl.table) )
{
  infl.row = infl.table[i]	%>% 
    html_nodes("td") %>%
    html_text();
  
  year = as.numeric(infl.row[1]);
  temp = gsub('$','',infl.row[2],fixed=T);
  temp = gsub(',','',temp,fixed=T);
  dollar = as.numeric(temp);
  temp = gsub('%','',infl.row[3],fixed=T);
  inflation = as.numeric(temp);	
  
  result$year[i] = year;
  result$dollar[i] = dollar;
  result$inflation[i] = inflation;

}
result;
}



convertDollars = function(actor,inflation_data) 
{
  change_rate = inflation_data[which(inflation_data$year ==2000),]$dollar/inflation_data$dollar
  conversion_frame = data.frame(year = inflation_data$year,change_rate)
  actor$movies.50$mill.2000 = conversion_frame$change_rate[match(actor$movies.50$year,conversion_frame$year)]*actor$movies.50$millions
  actor;
  
}

