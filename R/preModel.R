## author:Sokolheavy
#  update:2019-06-16



#' @title Add binned variables to the dataset
#'
#' @description
#' \code{binning} binning numeric variables
#' 
#' @param df A dataframe.
#' @param variable_fc Number of first column of variable for binning
#' @param target_calc Number of column target`s variable
#'
#' @return A dataframe with binned variables
#' 
#' @importFrom smbinning smbinning
#' @importFrom classInt classIntervals
#' 
#' @export
#' 
binning <- function(work_data, variable_fc, target_calc){
  # binning every variable with different type of binning('equal', 'smbinning', '')
  bin_col <- match(names(work_data[ ,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], 
                                                                                      function(x) is.numeric(x))))], names(work_data))
  
  # load(file = 'digit.rda')
  begin_ncol <- ncol(work_data)
  digit<- c()
  digit[begin_ncol]<-NA
  
  # Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc
  for (i in bin_col)
  { 
    if(is.na(digit[i]))
    {
      print(paste(i, ':', names(work_data[i])), quote = FALSE)
      print(paste('from', min(as.numeric(work_data[,i])), 'to', max(as.numeric(work_data[,i])), 'with', nrow(unique(work_data[i])), 'unique values in', nrow(work_data[i]), 'records'), quote = FALSE)
      digit[i] <- as.numeric(readline(prompt="Enter precision: "))
    }
  }
  save(digit, file = "digit.rda")
  
  
  #loop with binning and creating cat vatiables in one
  for (i in bin_col) { 
    error_list <- c()
    print(i)
    print(names(work_data[i]), quote = FALSE)
    # equal 
    # Idea: equal number of observation in every class
    print('calculating equal_depth method...', quote = FALSE)
    # create intervals
    eq_d<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile')
    
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_Eq_depth", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                                c(min(work_data[,i])-1,unique(eq_d$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    
    print('calculating equal_width method...', quote = FALSE)
    # create intervals
    eq_w<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'equal')
    
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_eq_width", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                                c(min(work_data[,i])-1,unique(eq_w$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    
    print('calculating jenks method...', quote = FALSE)
    # create intervals
    jk<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'jenks')
    
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_jenks", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                                c(min(work_data[,i])-1,unique(jk$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    
    
    # smbinning
    print('calculating smbinning method...', quote = FALSE)
    # Idea: 'optimal binning' (maximization IV)
    sb_data<-work_data[c(target_calc,i)]
    sb_data[2]<-ceiling(sb_data[2]/digit[i])*digit[i]
    sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]), FALSE)
    if (length(sb) > 1) ##check this condition !!!!
    {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
                                                right = TRUE, left = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    }
    if (length(grep('Error', sb)) > 0 )
    {
      error_list <- c(error_list, i)
    }
    
    # save binnings intervals into 'rda' files
    save(eq_w, eq_d, jk, sb, file = paste(names(work_data[i]),".rda", sep = ""))
    
  }
  work_data <<- work_data
} 




#' @title Buildng plots for cheak binning
#'
#' @description
#' \code{bin_analysis} will buildig plots and analysis 
#'
#' @param variable_fc_bin Number of column first binned variable in dataset
#' @param target_calc_bin Number of column target`s variable
#' @param bin_data Dataset with binned data
#' @param isOpen = T Open html file automatically
#'
#' @return A dataframe with binned variables
#' 
#' 
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr summarise_all
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom gtable gtable_add_grob
#' @importFrom gtable gtable_add_cols
#' @importFrom ggpubr ggtexttable 
#' @importFrom ggpubr ggparagraph 
#' @importFrom ggpubr ggarrange 
#' @importFrom R2HTML HTMLStart
#' @importFrom R2HTML HTMLInsertGraph
#' @importFrom R2HTML HTMLStop
#' @importFrom R2HTML HTMLStop
#' @importFrom utils browseURL
#' 
#' @export
#' 
#' 
bin_analysis <- function(variable_fc_bin, target_calc_bin, bin_data, IsOpen = T){
  
  # set name of folder
  folder_name <- "html_plots"
  
  # create folder
  dir.create(folder_name)
  
  initial_path <- getwd()
  setwd(paste0(getwd(), "/", folder_name))
  
  bin_ncol<-ncol(bin_data)
  
  ## BR analysis
  for (i in variable_fc_bin:bin_ncol){
    #create 'br_table'. It consists of 2 column("BR" + name_of_variables, BR_value)
    var_for_group <- names(bin_data)[i]
    column_br <- paste("BR", 
                       names(bin_data)[i]
                       , sep="_")
    
    br_table <- bin_data %>%
      select(c(i,target)) %>%
      group_by(.dots = var_for_group) %>%
      summarise_all(funs(!!column_br := (n() - sum(.))/n()))
    
    # join 'br_table' to the table with bining variables
    bin_data <- left_join(bin_data, br_table,by=names(bin_data)[i])
  }
  
  
  k <- 1
  i <- 1
  Total<-length(bin_data$target_for_calc)
  Good<-sum(bin_data$target_for_calc)
  Bad<-Total-Good
  
  for (j in variable_fc_bin:bin_ncol) {
    
    plot1_hist <- ggplot(bin_data, aes(bin_data[,j])) + 
      geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
      scale_y_continuous(labels=scales::percent)+ 
      geom_text(aes( y = ((..count..)/sum(..count..)),
                     label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      theme(axis.text.x = element_text(angle=10, vjust=0.9),
            plot.margin = unit(c(1,1,1,1), "cm") ) + 
      labs( y = "Class", x = "")
    
    plot2_BR_line <- ggplot(bin_data, aes(x=bin_data[,j],y=bin_data[,j-variable_fc_bin+bin_ncol+1],group=1)) + 
      geom_line(color="indianred3",size=1)+
      geom_point(color="indianred3") +
      theme(axis.text.x = element_text(angle=10, vjust=0.9),
            plot.margin = unit(c(1,1,1,1), "cm") ) + 
      scale_y_continuous(limits=c(0, 0.5),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3, .35, .4, .45, .5), 
                         labels = function(x) paste0(x*100, "%"))+
      labs( y = "BR", x = "")
    
    # union 2 graphics(plot1_hist, plot2_BR_line) in 1 
    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(plot1_hist))
    g2 <- ggplot_gtable(ggplot_build(plot2_BR_line))
    
    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                         pp$l, pp$b, pp$l)
    
    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    
    #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
    options(warn = -1) 
    
    # calc statistic values for every column
    aggregate_table<-aggregate(. ~ bin_data[,j], data = bin_data[c(names(bin_data)[target_calc_bin],names(bin_data)[j])],
                               FUN = function(x) c(good = sum(x),
                                                   bad=length(x)-sum(x),
                                                   total = length(x),
                                                   good2=  round((sum(x)*100)/Good,2),
                                                   bad2=round((length(x)-sum(x))*100/Bad,2),
                                                   total2=round((length(x)*100)/Total,2),
                                                   BR=round((length(x)-sum(x))*100/length(x),2),
                                                   WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
    
    #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
    aggregate_table<-cbind(aggregate_table[,1],data.frame(aggregate_table[,2]))
    names(aggregate_table)<-c(names(bin_data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
    
    table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lRedWhite"))
    
    # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
    text1 <- paste0("
                    ",k,". ",names(bin_data)[j])
    
    
    # set style of 'text1'
    title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
    
    # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
    print(paste0(k,". ", names(bin_data)[j]))
    k <- k+1
    png(file = paste0(i,".png"),width = 1200, height=1200)
    i <- i+1
    # union 4 object in one file: 
    print(ggarrange(title1, g, table , 
                    ncol = 1, nrow = 3,heights = c(0.1, 0.3, 0.2)))
    
    dev.off() 
  }
  
  # create HTML file
  
  setwd(initial_path)
  HTMLStart(folder_name,  Title="Visualization and analytic of every variable")
  
  for (j in 1:i){
    name <- paste0(j,".png")
    HTMLInsertGraph(GraphFileName = name)
  }
  
  HTMLStop()
  
  if (IsOpen == T){browseURL(paste0('file://', file.path(getwd(), 'index.html')))}
}












#' @title Select Types of Variables
#'
#' @description
#' \code{convertVar} convert variables to other types.
#'
#' @details
#' when \code{vars=-1}, which variables to be converted depends on \code{toType}:
#' if \code{toType="fac"}, all the character variables will be converted;
#' if \code{toType="cha"}, all the factor variables will be converted;
#' if \code{toType="int"}, all the numeric variables will be converted.
#'
#' @param df A dataframe to be converted.
#' @param vars Vector of column names or numbers to convert, \code{-1} means to convert all matched variables based on \code{toType} automatically, see details.
#' @param toType The type converted to be, values must be one of \code{c("fac","cha","int")}. If \code{toType="int"}, the converted result is intercepting the integer part of specified variable, not rounding.
#'
#' @return A new dataframe with the same order of variables
#' @importFrom dplyr select
#' @export

convertVar <- function(df,toType,vars=-1) {
  # library(dplyr)
  ncl <- ncol(df)
  nm <- names(df)
  if(length(toType) != 1) stop("the length of parameter 'toType' must be equal to 1")
  if(!toType %in% c("fac","cha","int")) stop("parameter 'toType' must be one of c('fac','cha','int')")
  if(sum(is.na(vars)) > 0) stop("parameter 'vars' contains NA")
  if(is.character(vars)) {
    if(sum(!vars %in% nm) > 0) stop(paste(paste(vars[which(!vars %in% nm)],collapse=","),"is not in the variable names of 'df'"))
    df.spec <- df[vars]
    df.unspec <- df[!nm %in% vars]
    if(toType == "fac") {
      df.factor <- as.data.frame(sapply(df.spec,as.factor))
      res <- cbind(df.unspec,df.factor)
    } else if(toType == "cha") {
      df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
      res <- cbind(df.unspec,df.cha)
    } else {
      df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
      res <- cbind(df.unspec,df.int)
    }
  } else if(is.numeric(vars)) {
    if(!all(as.integer(vars) == vars)) stop("parameter 'vars' is float type, not allowed")
    if(length(vars) == 1) {
      if(vars < 1 & vars != -1) stop("parameter 'vars' is less than 1")
      if(vars > ncl) stop("parameter 'vars' is over the number of dataframe's columns")
      if(vars == -1) {
        if(toType == "fac") {
          if(sum(!sapply(df,is.numeric)) == 0) stop("no character variables can be converted to factors")
          df.num <- df[sapply(df,is.numeric)]
          df.str <- df[!sapply(df,is.numeric)]
          df.factor <- as.data.frame(sapply(df.str,as.factor))
          res <- cbind(df.num,df.factor)
        } else if(toType == "cha") {
          if(sum(!sapply(df,is.factor)) == 0) stop("no factors can be converted to character variables")
          df.factor <- df[sapply(df,is.factor)]
          df.unfac <- df[!sapply(df,is.factor)]
          df.cha <- as.data.frame(sapply(df.factor, as.character), stringsAsFactors = FALSE)
          res <- cbind(df.unfac,df.cha)
        } else {
          if(sum(sapply(df,is.numeric)) == 0) stop("no numeric variables can be converted to integer variables")
          df.num <- df[sapply(df,is.numeric)]
          df.str <- df[!sapply(df,is.numeric)]
          df.int <- as.data.frame(sapply(df.num, as.integer), stringsAsFactors = FALSE)
          res <- cbind(df.str,df.int)
        }
      } else {
        df.spec <- df[vars]
        df.unspec <- df[-vars]
        if(toType == "fac") {
          df.factor <- as.data.frame(sapply(df.spec,as.factor))
          res <- cbind(df.unspec,df.factor)
        } else if(toType == "cha") {
          df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
          res <- cbind(df.unspec,df.cha)
        } else {
          df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
          res <- cbind(df.unspec,df.int)
        }
      }
    } else if(length(vars) > 1) {
      if(min(vars,na.rm=TRUE) < 1) stop("the min element in 'vars' is less than 1")
      if(max(vars,na.rm=TRUE) > ncl) stop("the max element in 'vars' is over the number of dataframe's columns")
      df.spec <- df[vars]
      df.unspec <- df[-vars]
      if(toType == "fac") {
        df.factor <- as.data.frame(sapply(df.spec,as.factor))
        res <- cbind(df.unspec,df.factor)
      } else if(toType == "cha") {
        df.cha <- as.data.frame(sapply(df.spec, as.character), stringsAsFactors = FALSE)
        res <- cbind(df.unspec,df.cha)
      } else {
        df.int <- as.data.frame(sapply(df.spec, as.integer), stringsAsFactors = FALSE)
        res <- cbind(df.unspec,df.int)
      }
    } else {
      stop("the length of vector 'vars' must be more than or equal to 1")
    }
  } else {
    stop("parameter 'vars' must be an integer or character vector")
  }
  res <- select(res,nm)    #sort the variables
  return(res)
}


#' @title Get the Max Percent of the Variable's values
#'
#' @param df A dataframe.
#' @param x_nm Name of variable.
#'
#' @return A numeric
#' 
#' @export

maxPercVar_x <- function(df,x_nm) {
  x <- df[,x_nm]
  len <- length(x)
  if(len==0) stop(paste("error in 'maxPercVar_x' Function: vector '",x_nm,"' contains zero element",sep=""))
  cnts <- as.vector(table(x,useNA="ifany"))
  percents <- cnts/len
  return(max(percents,na.rm=TRUE))
}



#' @title Get the Max Percents of All Variable's of one value
#'
#' @description
#' \code{maxPercVar} will return the max percent vector of every variable's 
#'
#' @param df A dataframe.
#'
#' @return A vector
#' @export

maxPercVar <- function(df) {
  nm <- names(df)
  myres <- sapply(nm,maxPercVar_x,df=df)
  return(myres)
}




#' @title Delete Variables Based on Single-Value Percent
#'
#' @description
#' \code{delVar} will delete variables whose single-value percent is more than or equal to the given threshold, then return a new dataframe.
#'
#' @param df A dataframe.
#' @param percent The given threshold, default \code{0.9}.
#' @param exclude Vector of column names or numbers to exclude, default \code{NULL}.
#'
#' @return A dataframe after deleted
#' @export

delVar <- function(df,percent=0.9,exclude=NULL) {
  df2 <- df
  nm2 <- names(df2)
  if(!is.null(exclude)) df <- excludeCol(df,exclude)
  nm <- names(df)
  sinvalper <- maxSinvalPercent(df)
  delvars <- nm[which(sinvalper >= percent)]
  delflag <- nm2 %in% delvars
  return(df2[!delflag])
}
