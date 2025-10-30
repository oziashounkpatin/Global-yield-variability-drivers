es_data<-data.frame(ES=c(-0.2,0,0.2,0.4,0.6))


perc<- function(data){
  data_conv=100*(exp(data) - 1)
  return(data_conv)
}

perc(es_data)
