library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(leaflet)
library(lubridate)
library(forecast)


#### load data ####
data <- readRDS(file = "atm_all_cleaned_v2.rds")

##### Map functions ####
data_map <- data%>%
    mutate(error = ifelse(!is.na(message_code),1,0))%>%
    group_by(atm_id,Date,atm_manufacturer,currency,atm_lat,atm_lon)%>%
    mutate(n_error = sum(error))%>%
    mutate(n_trans = length(year))%>%
    ungroup()%>%
    select(atm_id,Date,atm_manufacturer,currency,atm_lat,atm_lon,n_error,n_trans,atm_location)%>%
    distinct()

draw_error = function(manufacturer,startdate,enddate){
    data_error <- data_map%>%
        filter(Date>= startdate & Date<= enddate)%>%
        group_by(atm_id)%>%
        mutate(error_rate = sum(n_error) /sum(n_trans)*100) %>%
        select(atm_id,atm_manufacturer,atm_lat,atm_lon,error_rate,atm_location) %>%
        distinct()
    maxvalue = max(data_error$error_rate)

    data_error_m = filter(data_error,atm_manufacturer == manufacturer)
    pal <- colorNumeric(palette='Oranges',domain=c(0,maxvalue))
    leaflet(data = data_error_m) %>%
        addProviderTiles('CartoDB')  %>%
        setView(10.486821,56.40783, zoom =7)%>%
        addCircleMarkers(~atm_lon, ~atm_lat, popup = ~paste("ATM ID: ",as.character(atm_id),"<br/>",
                                                            "ATM location: ",atm_location,"<br/>",
                                                            "Failure rate: ",round(error_rate,2)),
                         label = ~paste("ATM ID: ", as.character(atm_id)),radius = 6,
                         color = ~pal(error_rate),stroke = FALSE, fillOpacity = 1)%>%
        leaflet::addLegend("topright", pal = pal, values = c(0,maxvalue),
                           title = paste("ATM Manufacturer <br>",manufacturer,"<br>Failure rate (%)"),
                           opacity = 1)
}

draw_trans = function(curren,startdate,enddate){
    data_trans <- data_map%>%
        filter(Date>= startdate & Date<= enddate)%>%
        group_by(atm_id,currency)%>%
        mutate(N = sum(n_trans)) %>%
        select(atm_id,currency,atm_lat,atm_lon,N,atm_location) %>%
        distinct()
    maxvalue = max(data_trans$N)

    data_trans_c = filter(data_trans,currency == curren)
    pal <- colorNumeric(palette='Oranges',domain=c(0,maxvalue))
    leaflet(data = data_trans_c) %>%
        addProviderTiles('CartoDB')  %>%
        setView(10.486821,56.40783, zoom =7)%>%
        addCircleMarkers(~atm_lon, ~atm_lat, popup = ~paste("ATM ID: ",as.character(atm_id),"<br/>",
                                                            "ATM location: ",atm_location,"<br/>",
                                                            #"Street name: ",atm_streetname,"<br/>",
                                                            "Number of Transaction: ",N),
                         label = ~paste("ATM ID: ", as.character(atm_id)),radius = 6,
                         color = ~pal(N),stroke = FALSE, fillOpacity = 1)%>%
        leaflet::addLegend("topright", pal = pal, values = c(0,maxvalue),
                           title = paste("Currency <br>",curren,"<br>Number of Transaction"),
                           opacity = 1)
}

data_unique <- data %>%
    select(atm_id,atm_lat,atm_lon,atm_location) %>%
    distinct()

data_calender <- data %>%
    count(date,year,month,weekday,week,sort = FALSE)%>%
    group_by(month)%>%
    mutate(monthweek = 1+week-min(week))

plot_calender  <- function(data_calender) {#normalizing the week to start at 1 for every month
    p1 <- ggplot(data_calender, aes(monthweek,weekday,fill=n))+ theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    p2<-p1+
        geom_tile(colour='white',aes(text=paste('Date: ',date,'<br>Day of week:',weekday,'<br>Number of transaction:',n)))+
        facet_grid(year~month)+
        scale_fill_distiller(palette = "Blues",direction = 1,guide = guide_colorbar(barwidth=15))+
        xlab("Month") + ylab("") +
        labs(fill = "Number of transactions")+
        theme(strip.background = element_rect(fill="white"),
              legend.position = "bottom",text = element_text(size=15))
    return(p2)
}

data_hour <- data %>%
    group_by(hour)%>%
    count(weekday, sort = FALSE)

plot_hour<-function(data_hour){
    p5 <- ggplot(data_hour, aes(hour,weekday,fill=n))+
        geom_tile(colour='white',
                  aes(text=paste('Weekday: ',weekday,'<br>Hour: ',
                                 hour,'<br>Number of transaction:',n)))+
        scale_fill_distiller(palette = "Blues",direction = 1,guide = guide_colorbar(barwidth=15))+
        xlab("Hour") + ylab("")+
        labs(fill = "Number of transactions")+
        theme(strip.background = element_rect(fill="white"),
              legend.position = "bottom",text = element_text(size=15))
    return(p5)
}


data_calender_id <- data %>%
    count(atm_id,date,year,month,weekday,week, sort = FALSE)%>%
    group_by(atm_id,month)%>%
    mutate(monthweek = 1+week-min(week))

plot_calender_byid<-function(data_calender_id,atmid){
    data4 <- filter(data_calender_id,atm_id == atmid)
    p3 <- ggplot(data4, aes(monthweek,weekday,fill=n))+ theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    p4<-p3+
        geom_tile(colour='white',aes(text=paste('Date: ',date,'<br>Day of week:',weekday,'<br>Number of transaction:',n)))+
        facet_grid(year~month)+
        scale_fill_distiller(palette = "Blues",direction = 1,guide = guide_colorbar(barwidth=15))+
        xlab("Month") + ylab("") +
        labs(fill = "Number of Transactions")+
        theme(strip.background = element_rect(fill="white"),
              legend.position = "bottom",text = element_text(size=15))
    return(p4)
}


data_hour_byid <- data %>%
    group_by(hour,weekday)%>%
    count(atm_id, sort = FALSE)

plot_hour_byid <- function(data_hour_byid, atmid){
    data6 <- filter(data_hour_byid, atm_id == atmid)
    p6 <- ggplot(data6, aes(hour,weekday,fill=n))+
        geom_tile(colour='white',
                  aes(text=paste('ATM id:',atm_id,
                                 '<br>Weekday: ',weekday,
                                 '<br>Hour: ',hour,
                                 '<br>Number of transaction:',n)))+
        scale_fill_distiller(palette = "Blues",direction = 1,guide = guide_colorbar(barwidth=15))+
        xlab("Hour") + ylab("")+
        labs(fill = "Number of transactions")+
        theme(strip.background = element_rect(fill="white"),
              legend.position = "bottom",text = element_text(size=15))
    return(p6)
}

#### funnel plot code chunk ####
# # sum of transactions in 1 year, by ATM id
num_trans_y <-
    data %>%
    group_by(atm_id) %>%
    summarise(Trans_count =dplyr::n()) %>%
    ungroup()

# # sum of failures in 1 year, by ATM id
num_failure_y <-
    data %>%
    select(atm_id,message_text) %>%
    dplyr::filter(message_text != '') %>%
    group_by(atm_id) %>%
    summarise(Failure_count=dplyr::n()) %>%
    ungroup()

# failRate dataframe
failRate_y <- merge(x=num_trans_y,y=num_failure_y,by = c("atm_id"))

failRate_y$Failure_Rate <- round(((failRate_y$Failure_count/failRate_y$Trans_count)),4)
#normalised data
failRate_y$sqrtFR <- sqrt(failRate_y$Failure_Rate)
failRate_y$cbrtFR <- (failRate_y$Failure_Rate)^(1/3)
failRate_y$log10FR <- log(failRate_y$Failure_Rate,10)

CI99<-2.58
CI95<-1.96
ci99 <-function(df,fit_mean){
    lkup<-data.frame(id=seq(1, max(df$Trans_count), 1))
    lkup$Upper99<- fit_mean + CI99*sqrt(fit_mean*(1-fit_mean)/lkup$id)
    lkup$Lower99<- fit_mean - CI99*sqrt(fit_mean*(1-fit_mean)/lkup$id)
    lkup<-gather(lkup, key, value,-id)
    return(lkup)
}
ci95 <- function(df,fit_mean){
    lkup2<-data.frame(id=seq(1, max(df$Trans_count), 1))
    lkup2$Upper95<- fit_mean + CI95*sqrt(fit_mean*(1-fit_mean)/lkup2$id)
    lkup2$Lower95<- fit_mean - CI95*sqrt(fit_mean*(1-fit_mean)/lkup2$id)
    lkup2<-gather(lkup2, key, value,-id)
    return(lkup2)
}
show_hist <- function(df,norm_on){
    if (norm_on == 'no_trans'){
        y <- df$Failure_Rate
        g<- ggplot(df, aes(x=Failure_Rate)) + geom_histogram(fill= "wheat3",bins = 30)+
            labs(
                # title=paste0('Histogram of failure rate','<br>','<sup>','based on original data','</sup>'),
                 x="Failure rate", y="number of ATMs")+
            theme(panel.border = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "grey100"))
        return(ggplotly(g))
    }
    else if (norm_on == 'log10'){
        y <- df$log10FR
        g<- ggplot(df, aes(x=log10FR)) + geom_histogram(fill= "wheat3",bins = 30)+
            labs(
                # title=paste0('Histogram of failure rate','<br>','<sup>','based on log10 transformation','</sup>'),
                 x="Failure rate", y="number of ATMs")+
            theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "grey100"))
        return(ggplotly(g))
    }
    else if (norm_on == 'sqrt'){
        y <- df$sqrtFR
        g<- ggplot(df, aes(x=sqrtFR)) + geom_histogram(fill= "wheat3",bins = 30)+
            labs(
                # title=paste0('Histogram of failure rate','<br>','<sup>','based on square-root transformation','</sup>'),
                 x="Failure rate", y="number of ATMs")+
            theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "grey100"))
        return(ggplotly(g))
    }
    else if (norm_on == 'cbrt'){
        y <- df$cbrtFR
        g<- ggplot(df, aes(x=cbrtFR)) + geom_histogram(fill= "wheat3",bins = 30)+
            labs(
                # title=paste0('Histogram of failure rate','<br>','<sup>','based on cubic-root transformation','</sup>'),
                 x="Failure rate", y="number of ATMs")+
            theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "grey100"))
        return(ggplotly(g))
    }
}
## funnel plot year
funnel_plot_y <- function(norm_on){
    if (norm_on == 'no_trans'){
        y <- failRate_y$Failure_Rate
    }
    else if (norm_on == 'log10'){
        y <- failRate_y$log10FR
        fit_mean = mean(y)
        # standard error
        y.se <- (y-fit_mean)^2
        sd <- sum(y.se/length(y.se))
    }
    else if (norm_on == 'sqrt'){
        y <- failRate_y$sqrtFR
    }
    else if (norm_on == 'cbrt'){
        y <- failRate_y$cbrtFR
    }
    fit_mean = mean(y)
    # plot
    x<-"Number of transactions"
    y<-"ATM Failure rate(%)"
    # title<-"Funnel plot on failure rate on ATM"
    if (norm_on == "no_trans"){
        text_t <- paste0('Funnel plot on failure rate on ATM',
                         '<br>',
                         '<sup>','Based on original data','</sup>')
        lkup <-ci99(failRate_y,fit_mean)
        lkup2<-ci95(failRate_y,fit_mean)
        p1<-ggplot(failRate_y, aes(x=Trans_count, y = Failure_Rate*100))+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            geom_hline(aes(yintercept=fit_mean*100))+
            labs(x=x, y=y)+
            theme(panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

        p2<-p1+ geom_line(aes(x=id, y=value*100,col=key), data=lkup)+
            geom_line(aes(x=id, y=value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'log10'){
        text_t <- paste0('Funnel plot on failure rate on ATM',
                         '<br>',
                         '<sup>','Based on log10 normalisation','</sup>')
        lkup<-data.frame(id=seq(1, max(failRate_y$Trans_count), 1))
        lkup$Upper99<- fit_mean + CI99*(sd/sqrt(lkup$id))
        lkup$Lower99<- fit_mean - CI99*(sd/sqrt(lkup$id))
        lkup<-gather(lkup, key, value,-id)

        lkup2<-data.frame(id=seq(1, max(failRate_y$Trans_count), 1))
        lkup2$Upper95<- fit_mean + CI95*(sd/sqrt(lkup2$id))
        lkup2$Lower95<- fit_mean - CI95*(sd/sqrt(lkup2$id))
        lkup2<-gather(lkup2, key, value,-id)

        p1<-ggplot(failRate_y, aes(x=Trans_count, y = Failure_Rate*100))+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            geom_hline(aes(yintercept=10^fit_mean*100))+
            labs(x=x, y=y)

        p2<-p1+ geom_line(aes(x=id, y=10^value*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=10^value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'sqrt'){
        text_t <- paste0('Funnel plot on failure rate on ATM',
                         '<br>',
                         '<sup>','Based on square-root normalisation','</sup>')
        lkup <-ci99(failRate_y,fit_mean)
        lkup2<-ci95(failRate_y,fit_mean)
        lkup<-mutate(lkup,value=ifelse(value>0,value^2*100,-(value^2*100)))
        lkup2<-mutate(lkup2,value=ifelse(value>0,value^2*100,-(value^2*100)))
        p1<-ggplot(failRate_y, aes(x=Trans_count, y = Failure_Rate*100))+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            geom_hline(aes(yintercept=fit_mean^2*100))+
            labs(x=x, y=y)

        p2<-p1+ geom_line(aes(x=id, y=value, col=key), data=lkup)+
            geom_line(aes(x=id, y=value, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'cbrt'){
        text_t<-paste0('Funnel plot on failure rate on ATM',
                       '<br>',
                       '<sup>','Based on cubic-root normalisation','</sup>')

        lkup <-ci99(failRate_y,fit_mean)
        lkup2<-ci95(failRate_y,fit_mean)
        p1<-ggplot(failRate_y, aes(x=Trans_count, y = Failure_Rate*100))+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            geom_hline(aes(yintercept=fit_mean^3*100))+
            labs(x=x, y=y)

        p2<-p1+ geom_line(aes(x=id, y=value^3*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=value^3*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    return(ggplotly(p2, tooltip = "text"))
}

#### funnel plot month ####
num_trans_m <-
    data %>%
    group_by(atm_id,month) %>%
    summarise(Trans_count =dplyr::n()) %>%
    ungroup()

# sum of failures in 1 year, by ATM id
num_failure_m <-
    data %>%
    select(atm_id,message_text, month) %>%
    dplyr::filter(message_text != '')%>%
    group_by(atm_id,month) %>%
    summarise(Failure_count=dplyr::n()) %>%
    ungroup()

failRate_m <- merge(x=num_trans_m,y=num_failure_m,by = c("atm_id","month"),all=TRUE) %>%
    replace_na(list(Failure_count=0))

failRate_m$Failure_Rate <- round(((failRate_m$Failure_count/failRate_m$Trans_count)),4)
failRate_m$cbrtFR <- (failRate_m$Failure_Rate)^(1/3)
failRate_m$sqrtFR <- sqrt(failRate_m$Failure_Rate)
failRate_m$log10FR <- log(failRate_m$Failure_Rate,10)

funnelMonth <-function(monthin, norm_on){
    failRate_temp <-failRate_m %>% dplyr::filter(month==monthin)
    if (norm_on == "no_trans"){
        fit_mean<-mean(failRate_temp$Failure_Rate)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)
        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~month)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 8000)+
            geom_hline(aes(yintercept=fit_mean*100))

        p2<-p1+ geom_line(aes(x=id, y=value*100,col=key), data=lkup)+
            geom_line(aes(x=id, y=value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'cbrt'){
        fit_mean = mean(failRate_temp$cbrtFR)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)
        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~month)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 8000)+
            geom_hline(aes(yintercept=fit_mean^3*100))
        p2<-p1+ geom_line(aes(x=id, y=value^3*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=value^3*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'sqrt'){
        fit_mean = mean(failRate_temp$sqrtFR)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)

        lkup<-mutate(lkup,value=ifelse(value>0,value^2*100,-(value^2*100)))
        lkup2<-mutate(lkup2,value=ifelse(value>0,value^2*100,-(value^2*100)))

        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~month)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 8000)+
            geom_hline(aes(yintercept=fit_mean^2*100))
        p2<-p1+ geom_line(aes(x=id, y=value, col=key), data=lkup)+
            geom_line(aes(x=id, y=value, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())

    }
    else if (norm_on == 'log10'){
        failRate_temp$log10FR[failRate_temp$log10FR==-Inf]<-0
        y <- failRate_temp$log10FR
        fit_mean = mean(y)
        y.se <- (y-fit_mean)^2
        sd <- sum(y.se/length(y.se))
        lkup<-data.frame(id=seq(1, max(failRate_temp$Trans_count), 1))
        lkup$Upper99<- fit_mean + CI99*(sd/sqrt(lkup$id))
        lkup$Lower99<- fit_mean - CI99*(sd/sqrt(lkup$id))
        lkup<-gather(lkup, key, value,-id)

        lkup2<-data.frame(id=seq(1, max(failRate_temp$Trans_count), 1))
        lkup2$Upper95<- fit_mean + CI95*(sd/sqrt(lkup2$id))
        lkup2$Lower95<- fit_mean - CI95*(sd/sqrt(lkup2$id))
        lkup2<-gather(lkup2, key, value,-id)

        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~month)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 8000)+
            geom_hline(aes(yintercept=10^fit_mean*100))

        p2<-p1+ geom_line(aes(x=id, y=10^value*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=10^value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    figo<-ggplotly(p2, tooltip = "text")
    return(figo)
}

#### funnel plot week ####
num_trans_w <-
    data %>%
    group_by(atm_id,weekday) %>%
    summarise(Trans_count =dplyr::n()) %>%
    ungroup()

# sum of failures in 1 year, by ATM id
num_failure_w <-
    data %>%
    select(atm_id,message_text, weekday) %>%
    dplyr::filter(message_text != '')%>%
    group_by(atm_id,weekday) %>%
    summarise(Failure_count=dplyr::n()) %>%
    ungroup()

failRate_w <- merge(x=num_trans_w,y=num_failure_w,by = c("atm_id","weekday"),all=TRUE) %>%
    replace_na(list(Failure_count=0))

failRate_w$Failure_Rate <- round(((failRate_w$Failure_count/failRate_w$Trans_count)),4)
failRate_w$cbrtFR <- (failRate_w$Failure_Rate)^(1/3)
failRate_w$sqrtFR <- sqrt(failRate_w$Failure_Rate)
failRate_w$log10FR <- log(failRate_w$Failure_Rate,10)

funnelWeek <- function(weekin,norm_on){
    failRate_temp <- failRate_w %>% dplyr::filter(weekday==weekin)
    if (norm_on == "no_trans"){
        fit_mean<-mean(failRate_temp$Failure_Rate)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)
        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~weekday)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 13000)+
            geom_hline(aes(yintercept=fit_mean*100))

        p2<-p1+ geom_line(aes(x=id, y=value*100,col=key), data=lkup)+
            geom_line(aes(x=id, y=value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'cbrt'){
        fit_mean = mean(failRate_temp$cbrtFR)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)
        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~weekday)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 13000)+
            geom_hline(aes(yintercept=fit_mean^3*100))
        p2<-p1+ geom_line(aes(x=id, y=value^3*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=value^3*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    else if (norm_on == 'sqrt'){
        fit_mean = mean(failRate_temp$sqrtFR)
        lkup <-ci99(failRate_temp,fit_mean)
        lkup2<-ci95(failRate_temp,fit_mean)

        lkup<-mutate(lkup,value=ifelse(value>0,value^2*100,-(value^2*100)))
        lkup2<-mutate(lkup2,value=ifelse(value>0,value^2*100,-(value^2*100)))

        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~weekday)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 13000)+
            geom_hline(aes(yintercept=fit_mean^2*100))
        p2<-p1+ geom_line(aes(x=id, y=value, col=key), data=lkup)+
            geom_line(aes(x=id, y=value, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())

    }
    else if (norm_on == 'log10'){
        failRate_temp$log10FR[failRate_temp$log10FR==-Inf]<-0
        y <- failRate_temp$log10FR
        fit_mean = mean(y)
        y.se <- (y-fit_mean)^2
        sd <- sum(y.se/length(y.se))
        lkup<-data.frame(id=seq(1, max(failRate_temp$Trans_count), 1))
        lkup$Upper99<- fit_mean + CI99*(sd/sqrt(lkup$id))
        lkup$Lower99<- fit_mean - CI99*(sd/sqrt(lkup$id))
        lkup<-gather(lkup, key, value,-id)

        lkup2<-data.frame(id=seq(1, max(failRate_temp$Trans_count), 1))
        lkup2$Upper95<- fit_mean + CI95*(sd/sqrt(lkup2$id))
        lkup2$Lower95<- fit_mean - CI95*(sd/sqrt(lkup2$id))
        lkup2<-gather(lkup2, key, value,-id)

        p1<-ggplot(failRate_temp, aes(x=Trans_count, y = Failure_Rate*100))+
            facet_wrap(~weekday)+
            geom_point(aes(text=paste("ATM :",atm_id,"<br>Failure rate:",Failure_Rate*100,"%")))+
            xlim(-100, 13000)+
            geom_hline(aes(yintercept=10^fit_mean*100))

        p2<-p1+ geom_line(aes(x=id, y=10^value*100, col=key), data=lkup)+
            geom_line(aes(x=id, y=10^value*100, col=key), data=lkup2)+
            scale_color_manual(values=c('#E69F00','#999999','#E69F00','#999999'))+
            lims(y=c(-1,2))+
            theme(legend.title=element_blank())
    }
    figo<-ggplotly(p2, tooltip = "text")
    return(figo)
}
#### Forecast functions ####
data_selected <- data %>%
    select(atm_id,message_text,year,month,day,hour)

id_unique <- unique(sort(data_selected$atm_id))

get_data <- function(time_input){
    
    if(time_input=="Month"){
        df1<- data_selected %>% 
            group_by(atm_id,year,month)%>%
            summarise(Total_Number = n())%>%
            ungroup()
        
        
        df2<- data_selected %>% 
            dplyr::filter(message_text != '')%>%
            group_by(atm_id,year,month)%>%
            summarise(Failure_Number = n())%>%
            ungroup()
        
        
        df3 <- merge(x=df1,y=df2,by = c("atm_id","year","month"),
                     all.x = TRUE)
        
        df3$Failure_Rate = round(((df3$Failure_Number/df3$Total_Number)*100),4)
        
        df3[is.na(df3)] <- 0
        
        colnames(df3)<- c("Atm_Id","Year","Month","Transanction","Failure_Trans","Failure_Rate")
    }
    
    else if(time_input=="Day"){
        df1<- data_selected %>% 
            group_by(atm_id,year,month,day)%>%
            summarise(Total_Number = n())%>%
            ungroup()
        
        
        df2<- data_selected %>% 
            dplyr::filter(message_text != '')%>%
            group_by(atm_id,year,month,day)%>%
            summarise(Failure_Number = n())%>%
            ungroup()
        
        
        df3 <- merge(x=df1,y=df2,by = c("atm_id","year","month","day"),
                     all.x = TRUE)
        
        df3$Failure_Rate = round(((df3$Failure_Number/df3$Total_Number)*100),4)
        
        df3[is.na(df3)] <- 0
        
        colnames(df3)<- c("Atm_Id","Year","Month","Day","Transanction","Failure_Trans","Failure_Rate")
    }
    
    else if(time_input=="Hour"){
        df1<- data_selected %>% 
            group_by(atm_id,year,month,day,hour)%>%
            summarise(Total_Number = n())%>%
            ungroup()
        
        
        df2<- data_selected %>% 
            dplyr::filter(message_text != '')%>%
            group_by(atm_id,year,month,day,hour)%>%
            summarise(Failure_Number = n())%>%
            ungroup()
        
        
        df3 <- merge(x=df1,y=df2,by = c("atm_id","year","month","day","hour"),
                     all.x = TRUE)
        
        df3$Failure_Rate = round(((df3$Failure_Number/df3$Total_Number)*100),4)
        
        df3[is.na(df3)] <- 0
        
        colnames(df3)<- c("Atm_Id","Year","Month","Day","Hour","Transanction","Failure_Trans","Failure_Rate")
    }
    
    return (df3)
}

get_trans <- function(df_var, time_input, id_input, var_input, freq = 2){
    
    id_table <- df_var%>%
        dplyr::filter(Atm_Id==id_input)
    
    if (time_input == 'Hour')
    {
        id_table$date <- with(id_table, ymd_h(paste(Year, Month, Day, Hour,sep= ' ')))
    }
    else if(time_input == 'Month')
    {
        id_table$date <- with(id_table, ymd_h(paste(Year,Month, sep= ' ')))
    }
    else if(time_input == 'Day')
    {
        id_table$date <- with(id_table, ymd_h(paste(Year,Month, Day, sep= ' ')))
    }
    
    # id_table$date <- with(id_table, ymd_h(paste(Year,Month,Day,sep= ' ')))
    id_table <- id_table[order(as.Date(id_table$date)),]
    
    if (var_input == 'Transaction')
    {
        trans = ts(id_table$Transanction, frequency = freq)
    }
    else
    {
        trans = ts(id_table$Failure_Rate,  frequency = freq)
    }
    
    return (trans)
}

trend_plot <-function(time_input, id_input, var_input,frequency=1)
{
    df_var <- get_data(time_input)
    trans <- get_trans(df_var,time_input, id_input, var_input,frequency)
    default_title <- paste("Time Series Plot for ATM No.", id_input,"group by",frequency,time_input)
    plot(trans,
         type="l",
         col = "Steelblue",
         lwd=2,
         ylab = var_input,
         xlab = time_input,
         main = default_title)
}

trend_decompose_plot <-function(time_input, id_input, var_input, freq=2)
{
    df_var <- get_data(time_input)
    trans <- get_trans(df_var,time_input, id_input, var_input, freq)
    components.ts = decompose(trans)
    plot(components.ts)
    
}


predict_plot <-function(time_input, id_input, var_input, period_input, fun_input, freq = 2)
{
    df_var <- get_data(time_input)
    trans <- get_trans(df_var, time_input, id_input, var_input, freq)
    
    if (fun_input == 'ETS')
    {
        fit <- ets(trans, model = "MAA")
        plot(forecast(fit, h = period_input),
             ylab=var_input,
             xlab = time_input)
    }
    else if(fun_input == 'ARIMA')
    {
        d.arima <- auto.arima(trans, seasonal = TRUE)
        d.forecast <- forecast(d.arima, h = period_input)
        plot(d.forecast,
             ylab=var_input,
             xlab = time_input)
        
    }
    else
    {
        print("Please select predict function.")
        return(-1)
    }
    
}

residual_plot <-function(time_input, id_input, var_input, fun_input, freq = 2)
{
    df_var <- get_data(time_input)
    trans <- get_trans(df_var, time_input, id_input, var_input, freq)
    
    
    if (fun_input == 'ETS')
    {
        fit <- ets(trans, model = "MAA")
        checkresiduals(fit)
    }
    else if(fun_input == 'ARIMA')
    {
        d.arima <- auto.arima(trans, seasonal = TRUE)
        checkresiduals(d.arima)
        
    }
    else
    {
        print("Please select predict function.")
        return(-1)
    }
    
}



#### ui ####
ui <- dashboardPage(
                    skin = "yellow",
                    dashboardHeader(title = "ATM Analyse"
                                    ),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem(
                                "User Guide",
                                tabName = 'intro',
                                icon = icon("info-circle")
                            ),
                            menuItem(
                              "Map Overview",
                              tabName = "map",
                              icon = icon("map-marker-alt")
                            ),
                            menuItem(
                              "Time-Series Visualisation",
                              tabName = "TSplot",
                              icon = icon("calendar")
                            ),
                            menuItem(
                                "Funnel Plot",
                                tabName = "funnelplot",
                                icon = icon("filter")
                            ),
                            menuItem(
                                "Forecasting",
                                tabName = "Forecasting",
                                # icon = icon("poll")
                                icon = icon("chart-line")
                            ),    
                            menuItem(
                                "Data Source",
                                tabName = "data",
                                icon = icon("table")
                            )
                        )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(
                              tabName = "intro",
                              h1("Thank you for using ATM Analyse"),
                              h3(" - An interative web dashboard for ATM performance analytics"),
                              box(width = 12,
                                  p("This dashboard could be used for analysing ATM performance. It attempts to help banks to look at ATM transaction data from below view, hence provides insights in their ATM operations."),
                                  tags$li("Map Overview: To be able to show transaction volumes by currency, failure rate by manufacturers comparison, and display with geo-locational information"),
                                  tags$li("Time-Series Visualiastion : To be able to show clear time series patterns based on ATMs"),
                                  tags$li("Funnel Plot : To provide statistical visualisations for identify failure rates of ATMs"),
                                  tags$li("Forecasting : To be able to give time series forecast on performance (transaction or failure rate) by ATM"),
                                  # p("Dataset used is from Spar Nord, ATM transactions in 2017.")
                              )
                            ),
                            #### First tab content ####
                            tabItem(tabName = "map",
                                    # h2("Location of ATMs"),
                                    fluidRow(
                                        column(
                                            width = 2,
                                            box(
                                            title = "Setting",
                                            width = 12,
                                            sliderInput(inputId = "dateSlider",
                                                        label = "Select Date Range:",
                                                        min = as.Date("2017-01-01","%Y-%m-%d"),
                                                        max = as.Date("2017-12-01","%Y-%m-%d"),
                                                        value=c(as.Date("2017-01-01"),as.Date("2017-12-01")),
                                                        timeFormat="%Y-%m-%d",
                                                        ticks = FALSE),
                                            radioButtons(inputId = "mapview", label ="Choose view",
                                                         choices = c("Failure rate" = "error",
                                                                     "Number of transactions" = "trans"),
                                                         selected = "error")
                                        )
                                               ),
                                        column(
                                          width = 10,
                                          box(
                                              width = 12,
                                            align = "center",
                                            status = "warning",
                                            collapsible = T,
                                            htmlOutput(outputId = "maptitle"),
                                            fluidRow(
                                                column(width = 6,height = NULL,
                                                       leafletOutput(outputId = "map_1",width =450, height = 500)
                                                ),
                                                column(width = 6,height = NULL,
                                                       leafletOutput(outputId = "map_2",width =450, height = 500)
                                                )
                                            )
                                        )
                                        )

                                    )
                            ),
                            # Second tab content
                            tabItem(tabName = "TSplot",
                                    # h2("Time-Series Visualisation"),
                                    fluidRow(
                                        column(width = 3,height = 800,
                                                   box(width = 12,
                                                       title = "Setting",
                                                       radioButtons(inputId ='tsunit', label = "Choose a unit",
                                                                    choices = c("All ATM" = "all",
                                                                                "One ATM" = "one"),
                                                                    inline = FALSE),
                                                       radioButtons(inputId ='tsview', label = "Choose view",
                                                                    choices = c("month-weekday" = "month-weekday",
                                                                                "weekday-hour" = "weekday-hour"),
                                                                    inline = FALSE),
                                                       htmlOutput(outputId = "map2title"),
                                                       leafletOutput(outputId = "map2")
                                                   )
                                        ),
                                        column(width = 9,
                                               box(background = NULL,width = 12,
                                                   status = "warning",
                                                   collapsible = T,
                                                   align = "center",
                                                   htmlOutput(outputId = "tstitle"),
                                                   plotOutput(outputId = "tsplot",height = 550))
                                        )
                                    )
                            ),
                            # Third tab content
                            tabItem(
                                tabName = "funnelplot",
                                # h2("Funnel Plot on Failure Rate"),
                                fluidRow(
                                    column(width = 2,
                                      box(width = 12,
                                        title="Setting",
                                        radioButtons('funnelView', "Choose view", choices = c("yearly" = "yearly",
                                                                                              "monthly" = "monthly",
                                                                                              "weekly" = "weekly"),
                                                     inline = FALSE),
                                        radioButtons('normalise', "Choose normalisation", choices = c("No transformation" = "no_trans",
                                                                                                      "sqrt" = "sqrt",
                                                                                                      "cbrt" = "cbrt",
                                                                                                      "log10" = "log10"),
                                                     inline = FALSE),
                                        actionButton("plotFunnel", "Let's Go!", icon("play"))
                                    )
                                    ),
                                    column(width = 10,
                                    box(width = 12,
                                               status = "warning",
                                               collapsible = T,
                                               align = "center",
                                               htmlOutput("title_dist"),
                                        plotlyOutput("distribution", height = 200)
                                    ),
                                    box(width = 12,
                                        status = "warning",
                                        collapsible = T,
                                        align = "center",
                                        htmlOutput("title_funnel"),
                                        plotlyOutput("funnelOutputPlot", height =600)
                                    )
                                    )
                                )
                            ),
                            ##### Forth tab content#####
                            tabItem(
                                tabName = "Forecasting",
                                # h2("Forecasting Model"),
                                fluidRow(
                                    
                                    box(width = 2,
                                        h4("Setting"),
                                        selectInput('atmId', "Select ATM", choices = id_unique, selected = NULL, multiple = FALSE,
                                                    selectize = TRUE, width = NULL, size = NULL),
                                        hr(),
                                        radioButtons('predictChoice', "Predict on", choices = c("No. of Transaction" = "Transaction",
                                                                                                "Failure Rate" = "Failure_rate"),
                                                    
                                                     inline = FALSE, width = NULL),
                                        hr(), 
                                        radioButtons('timeChoice', "Sample Unit", choices = c("Days" = "Day","Hours" = "Hour"),
                                                     inline = FALSE, width = NULL),
                                        
                                        selectInput('freChoice', "Sample Frequency", choices = c("7"="7"), multiple = FALSE,
                                                    selectize = TRUE, width = NULL, size = NULL),
                                        hr(),
                                        selectInput('funChoice', "Select Forecsating Model", choices = c("ETS"="ETS", "ARIMA"="ARIMA"), selected = NULL, multiple = FALSE,
                                                    selectize = TRUE, width = NULL, size = NULL),
                                        
                                        numericInput("obs", "Forecast Future Period (in days/hours)", 10),
                                        actionButton("plotPredict",  "Let's Go!", icon("play"))
                                    ),
                                    box(title = "Model Summary",
                                        width = 10,
                                        status = "warning",collapsible = TRUE,
                                        #verbatimTextOutput('treeSum'),
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Trend", plotOutput('graph1',width = "100%", height = "400px")),
                                                    # verbatimTextOutput("info"),
                                                    tabPanel("Decomposition", plotOutput('graph2', width = "100%", height = "400px")),
                                                    # verbatimTextOutput("info"),
                                                    
                                                    tabPanel("Forecasting",plotOutput('graph3', width = "100%", height = "400px")),
                                                    
                                                    tabPanel("Residual", plotOutput('graph4', width = "100%", height = "400px"),
                                                             verbatimTextOutput("info"))
                                        )   
                                    )
                                )
                            ),
                            ####Fifth tab content####
                            tabItem(tabName = "data",
                                      h2("Data Source"),
                                      h4("This is open-source data from bank Spar Nord in Denmark"),
                                    fluidRow(
                                        column(width = 12,
                                      DT::dataTableOutput("mytable")
                                        )
                                    )
                            )
                        )
                    )
)


server <- function(input,output,session) {

    # #### map & calendar ####
    # 
    mapview_select <- reactive({input$mapview})
    output$maptitle <- renderText({
        if(mapview_select()=="error"){
            paste("<h3>Map Overview of ATM Machine Failure Rate by Manufacturer</h3>")
        }
        else if (mapview_select()=="trans"){
            paste("<h3>Map Overview of ATM Machine Number of Transaction by Currency</h3>")
        }
    })
    output$map_1 = renderLeaflet({
        if(mapview_select()=="error"){
            draw_error("NCR",input$dateSlider[1],input$dateSlider[2])
        }
        else if (mapview_select()=="trans"){
            draw_trans("DKK",input$dateSlider[1],input$dateSlider[2])
        }
    })

    output$map_2 = renderLeaflet({
        if(mapview_select()=="error"){
            draw_error("Diebold Nixdorf",input$dateSlider[1],input$dateSlider[2])
        }
        else if (mapview_select()=="trans"){
            draw_trans("EUR",input$dateSlider[1],input$dateSlider[2])
        }
    })
    # 
    tsunit_select <- reactive({input$tsunit})
    tsview_select <- reactive({input$tsview})
    # 
    output$map2 = renderLeaflet({
        if (tsunit_select() == "one"){
            leaflet(data = data_unique) %>% addTiles() %>%
                addMarkers(~atm_lon, ~atm_lat, layerId =~as.character(atm_id), popup = ~paste("ATM ID: ",as.character(atm_id),"<br/>",
                                                                                              "ATM location: ",atm_location),
                           label = ~paste("ATM ID: ", as.character(atm_id)))
        }
        else if (tsunit_select() == "all"){
        }
    })
    output$map2title <- renderText({
        if (tsunit_select() == "one"){
            paste("<p class='control-label'><b>Select an ATM from map</b></p>")
        }
        else if (tsunit_select() == "all"){
        }
    })

    observeEvent(tsunit_select(), {
        if (tsunit_select() == "one"){
            observeEvent(tsview_select(), {
                if (tsview_select() == "month-weekday"){
                    observeEvent(input$map2_marker_click, {
                        p <- input$map2_marker_click
                        if(is.null(p)==FALSE){
                            atmid = p$id
                            output$tstitle = renderText({paste0("<h3>Time-Series Calendar Heatmap: ATM ",atmid," Transaction</h3>")})
                            output$tsplot = renderPlot({plot_calender_byid(data_calender_id,atmid)})
                        }})
                }
                if (tsview_select() == "weekday-hour"){
                    observeEvent(input$map2_marker_click, {
                        p <- input$map2_marker_click
                        if(is.null(p)==FALSE){
                            atmid = p$id
                            output$tstitle = renderText({paste0("<h3>Time-Series Hourly Heatmap: ATM ",atmid," Transaction</h3>")})
                            output$tsplot = renderPlot({plot_hour_byid(data_hour_byid,atmid)})
                        }})
                }
            })
        }
        else if(tsunit_select() == "all"){
            # p<-NULL
            observeEvent(tsview_select(), {
                if (tsview_select() == "month-weekday"){
                    output$tstitle = renderText({"<h3>Time-Series Calendar Heatmap: ATM Transaction</h3>"})
                    output$tsplot = renderPlot({plot_calender(data_calender)})
                }
                if (tsview_select() == "weekday-hour"){
                    output$tstitle = renderText({"<h3>Time-Series Hourly Heatmap: ATM Transaction</h3>"})
                    output$tsplot = renderPlot({plot_hour(data_hour)})
                }
            })
        }
    })

    #### funnel get radio ####
    

    funnelplot <- eventReactive({
        input$normalise 
        input$funnelView},{
        if(input$funnelView == "yearly"){
            funnel_plot_y(input$normalise)
        }
        else if (input$funnelView == "monthly"){
            fig1<-funnelMonth("Jan",input$normalise)
            fig2<- funnelMonth("Feb",input$normalise)
            fig3<- funnelMonth("Mar",input$normalise)
            fig4<- funnelMonth("Apr",input$normalise)
            fig5<- funnelMonth("May",input$normalise)
            fig6<- funnelMonth("Jun",input$normalise)
            fig7<- funnelMonth("Jul",input$normalise)
            fig8<- funnelMonth("Aug",input$normalise)
            fig9<- funnelMonth("Sept",input$normalise)
            fig10<- funnelMonth("Oct",input$normalise)
            fig11<- funnelMonth("Nov",input$normalise)
            fig12<- funnelMonth("Dec",input$normalise)
            
            figmonth<-subplot(style(fig1, showlegend = F),style(fig2, showlegend = F),
                                        style(fig3, showlegend = F),style(fig4, showlegend = F),
                                        style(fig5, showlegend = F),style(fig6, showlegend = F),
                                        style(fig7, showlegend = F),style(fig8, showlegend = F),
                                        style(fig9, showlegend = F),style(fig10, showlegend = F),
                                        style(fig11, showlegend = F),style(fig12, showlegend = F),
                                        nrows=3)
            # figmonth
            
        }
        else if (input$funnelView == "weekly") {
            fig111<-funnelWeek("Monday",input$normalise)
            fig121<-funnelWeek("Tuesday",input$normalise)
            fig13<-funnelWeek("Wednesday",input$normalise)
            fig14<-funnelWeek("Thursday",input$normalise)
            fig15<-funnelWeek("Friday",input$normalise)
            fig16<-funnelWeek("Saturday",input$normalise)
            fig17<-funnelWeek("Sunday",input$normalise)
            
            figweek <-subplot(style(fig111, showlegend = F),style(fig121, showlegend = F),
                              style(fig13, showlegend = F),style(fig14, showlegend = F),
                              style(fig15, showlegend = F),style(fig16, showlegend = F),
                              style(fig17, showlegend = F),
                              nrows=3)
            # figweek
        }
    })
    title1 <- eventReactive(input$normalise,{
        if (input$normalise == "no_trans"){
            isolate({paste("<h4>Histogram of failure rate","<br>based on original data </h4>")})
        }
        else if (input$normalise == "sqrt"){
            isolate({paste("<h4>Histogram of failure rate","<br>based on square-root normalisation </h4>")})
        }
        else if (input$normalise == "cbrt"){
            isolate({paste("<h4>Histogram of failure rate","<br>based on cubic-root normalisation </h4>")})
        }
        else if (input$normalise == "log10"){
            isolate({paste("<h4>Histogram of failure rate","<br>based on log10 normalisation </h4>")})
        }
    })
    title2 <- eventReactive({
        input$normalise 
        input$funnelView},{
        if (input$normalise  == "no_trans" && input$funnelView=="yearly"){
                    paste("<h4>Yearly failure rate","<br>based on original data </h4>")
                }
                else if(input$normalise  == "no_trans" && input$funnelView=="monthly"){
                    paste("<h4>Monthly failure rate","<br>based on original data </h4>")
                }
                else if(input$normalise  == "no_trans" && input$funnelView=="weekly"){
                    paste("<h4>Monthly failure rate","<br>based on original data </h4>")
                }
                else if (input$normalise  == "sqrt" && input$funnelView=="yearly"){
                    paste("<h4>Yearly failure rate","<br>based on square-root normalisation </h4>")
                }
                else if (input$normalise  == "sqrt" && input$funnelView=="monthly"){
                    paste("<h4>Monthly failure rate","<br>based on square-root normalisation </h4>")
                }
                else if (input$normalise  == "sqrt" && input$funnelView=="weekly"){
                    paste("<h4>Weekly failure rate","<br>based on square-root normalisation </h4>")
                }
                else if (input$normalise  == "cbrt" && input$funnelView=="yearly"){
                    paste("<h4>Yearly failure rate","<br>based on cubic-root normalisation </h4>")
                }
                else if (input$normalise  == "cbrt" && input$funnelView=="monthly"){
                    paste("<h4>Monthly failure rate","<br>based on cubic-root normalisation </h4>")
                }
                else if (input$normalise  == "cbrt" && input$funnelView=="weekly"){
                    paste("<h4>Weekly failure rate","<br>based on cubic-root normalisation </h4>")
                }
                else if (input$normalise  == "log10" && input$funnelView=="yearly"){
                    paste("<h4>Yearly failure rate","<br>based on log10 normalisation </h4>")
                }
                else if (input$normalise  == "log10" && input$funnelView=="monthly"){
                    paste("<h4>Monthly failure rate","<br>based on log10 normalisation </h4>")
                }
                else if (input$normalise  == "log10" && input$funnelView=="weekly"){
                    paste("<h4>Weekly failure rate","<br>based on log10 normalisation </h4>")
                }
    })
    
    observeEvent(input$plotFunnel,{
        output$title_dist <- renderText({
            isolate({title1()})
        })
        output$title_funnel <- renderText({
            isolate({title2()})
        })
        
        output$distribution <- renderPlotly({
            isolate({show_hist(failRate_y,input$normalise)})
        })
        output$funnelOutputPlot <- renderPlotly({
            isolate({funnelplot()})
            
        })
    })
    
    
    # #### forecasting section ####
    datasetInput <- reactive({
        switch(input@choice,
               "Transaction" = Transactions,
               "Failure_rate" = Failure_rate)
    })

    output$info <- renderText({
        paste0("Notes: \n1. The time plot shows some changing variation over time but is otherwise relatively unremarkable. The residual more close to zero is better. \n2. ACF is an (complete) auto-correlation function which gives us values of auto-correlation of any series with its lagged values. \nWe plot these values along with the confidence band. In short terms, the lines not cross the upper and lower line is better. \n3. The histogram of residuals is to check whether the residuals are normally distributed.")
    })

    observe({
        tc1 <- input$timeChoice
        if(tc1 == 'Month')
        {
            choice1 <- c('1')
        }
        else if(tc1 == 'Day')
        {
            choice1 <- c(seq(2,30,1))
        }
        else if (tc1 == 'Hour')
        {
            choice1 <- c('12','24','48','72')
        }
        updateSelectInput(session,inputId="freChoice",choices=choice1)
        
        pc1 <- reactive({input$predictChoice})
        if(pc1() == 'Transaction')
        {
            choice2 <- c('ARIMA', 'ETS')
        }
        else
        {
            choice2 <- c('ARIMA')
        }
        updateSelectInput(session,"funChoice",choices=choice2)
    })

    observeEvent(input$plotPredict,{
        output$graph1 <- renderPlot({
            isolate({
                trend_plot(input$timeChoice,  as.numeric(input$atmId), input$predictChoice,  as.numeric(input$freChoice))
            })
        })    
        output$graph2 <- renderPlot({
            isolate({
                trend_decompose_plot(input$timeChoice,  as.numeric(input$atmId), input$predictChoice,  as.numeric(input$freChoice))
            })
        })
        output$graph3 <- renderPlot({
            isolate({
                predict_plot(input$timeChoice, as.numeric(input$atmId), input$predictChoice, input$obs, input$funChoice, as.numeric(input$freChoice))
            })
        })
        output$graph4 <- renderPlot({
            isolate({
                    residual_plot(input$timeChoice, as.numeric(input$atmId), input$predictChoice, input$funChoice, as.numeric(input$freChoice))
                })
            })
    })
   
    ### show data source####
    output$mytable = DT::renderDataTable(
        {data},
        options= list(scrollX=TRUE) 
    )
}





# Run the application 
shinyApp(ui = ui, server = server)
