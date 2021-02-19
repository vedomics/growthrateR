# functions for my stupid little script


get_growth_rate_estimate = function(input_well, time1, time2, plate_reader_data){
  
  # plate_reader_data = read.csv("./10_29_19_clean.csv", header = TRUE, stringsAsFactors = FALSE)
  # input_well = "Newman"
  #set up known constants and lookup table
  i <- which(colnames(plate_reader_data) ==input_well)
  
  timewell = colnames(plate_reader_data)[grepl("time", colnames(plate_reader_data), ignore.case = TRUE)]
  indexed_time = which(colnames(plate_reader_data)==timewell)
  
  # Add natural log data to our selected well 
  growth_curve = plate_reader_data[,c(i, indexed_time)]
  colnames(growth_curve)[2] = "times"
  growth_curve$log_e= log(growth_curve[,1])
  
  # final_od = round(as.numeric(growth_curve[nrow(growth_curve),1]), digits =3)
  final_od = round(as.numeric(quantile(growth_curve[,1], .99)), digits =3)
  
  mini_curve = subset(growth_curve, times < time2 & times > time1)
  
  linear_reg = lm(log_e ~ times, data = mini_curve)
  
  mini_curve$predicted_fit = predict(linear_reg, mini_curve)
  rsq = round(summary(linear_reg)$r.squared, digits=3)
  gre = round(as.numeric(coef(linear_reg)[2]), digits = 3)
  
  return(c(rsq, gre, final_od))
}

 