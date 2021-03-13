#________________________________________________________
# require libraries
  library(tidyverse)
#________________________________________________________

#________________________________________________________
# plot timeseries data (default by filename)
# takes a data frame and a column name of variable to plot
timeseries_plot <- function(df, y_var, x_var, facet_var="HHID",size_var = "lpg_cooking") {
  
  ggplot(df, aes_string(y = y_var, x = x_var,size = size_var)) +
    geom_point(alpha = 0.3) +
    facet_wrap(~df[[facet_var]], ncol = 1, scales = "free") +
    theme_minimal() +
    theme(legend.position = "top") +
    # labs(y=bquote(''~C^o), x="") +
    # scale_x_datetime(date_breaks = "2 day",date_labels = "%e-%b") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10)) 
}


timeseries_plot_simple <- function(df, y_var, x_var) {
  ggplot(df, aes_string(y = y_var, x = x_var)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", formula = 'y ~ x',
                color = 'black') +
    theme_minimal() +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10)) 
}

timeseries_plot_ambient <- function(df2,df, y_var, x_var, facet_var="HHID",color_var="qc", marker_shape="location") {
  
  ggplot(df, aes_string(y = y_var, x = x_var, color = color_var,shape = marker_shape)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~df[[facet_var]], ncol = 1, scales = "free") +
    theme_minimal() +
    theme(legend.position = "top") +
    # labs(y=bquote(''~C^o), x="") +
    scale_x_datetime(date_breaks = "2 day",date_labels = "%e-%b") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1,size=10))+
    ggplot(df2,y = )
}
#________________________________________________________

#________________________________________________________
# plot timeseries data by a fill color
# takes a data frame and a column name of variable to plot
box_plot <- function(df, y_var, fill_var = "qc", x_var = "HHID", y_units = "units",title) {

  ggplot(df, aes_string(y = y_var, x = x_var)) + #, fill = fill_var)) +
    geom_boxplot(alpha = 0.25) +
    geom_jitter(height = 0,width = 0.2,alpha = 0.2) +
    scale_fill_discrete(drop = FALSE) +
    theme_minimal() +
    theme(legend.title=element_blank(),axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 30)) +
    theme(legend.position = "top") +
    ylab(paste0(y_units)) +
    xlab(x_var) + 
    ggtitle(title)

}


#________________________________________________________
# boxplot with facet
# takes a data frame and a column name of variable to plot
box_plot_facet <- function(df, y_var, fill_var = "qc", facet_var = "pm_monitor_type", x_var = "HHID", y_units = "units",title) {
  
  ggplot(df, aes_string(y = y_var, x = x_var)) + #, fill = fill_var)) +
    geom_boxplot(alpha=0) +
    geom_jitter(height = 0,width = 0.2,alpha = 0.2) +
    scale_fill_discrete(drop = FALSE) +
    theme_minimal() +
    theme(legend.title=element_blank(),axis.title.x = element_blank()) +
    theme(axis.text.x = element_text(angle = 30)) +
    theme(legend.position = "top") +
    facet_wrap(as.formula(paste("~", facet_var)), scales = "free", ncol = 2) +
    ylab(paste0(y_units)) +
    xlab(x_var) + 
    ggtitle(title)
  
}
#________________________________________________________

#________________________________________________________
# plot field data
pointplot <- function(df, y_var, color_var = "qc", x_var = "HHID", y_units = "units") {

  ggplot(df, aes_string(y = y_var, x = x_var, color = color_var)) +
    geom_point() +
    scale_fill_discrete(drop = FALSE) +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
    xlab(x_var)

}


# plot dot plot with geom smooth

plot_dot_line <- function(df, y_var, x_var) {
  
  m <- df %>%
       dplyr::do(model = lm(paste(eval(y_var), "~", eval(x_var)), .)) %>%
       dplyr::mutate(eqn = get_lm_eqn(model))

  eqn <- data.frame(eqn = unclass(m$eqn),
                    stove = m$stove)

  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(alpa = 0.2) +
    geom_smooth(method = "lm", formula = 'y ~ x',
                color = 'black') +
    geom_text(aes(x = -Inf, y = Inf, label = eqn),
              data = eqn, color = 'black', size = 7,
              parse = TRUE, vjust = "inward", hjust = "inward") + 
    theme_bw() + 
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________
#
#________________________________________________________
## plot dot plot with geom smooth

plot_dot_line <- function(df, y_var, y_label, x_var = "firepower",
                                x_label = "firepower (kW)", facet_1 = "var", plot_color = "HHID") {

  m <- df %>%
       dplyr::group_by_(facet_1) %>%
       dplyr::do(model = lm(paste(eval(y_var), "~", eval(x_var)), .)) %>%
       dplyr::mutate(eqn = get_lm_eqn(model))
  
  eqn <- data.frame(eqn = unclass(m$eqn),
                    var = m$var)
  
  ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(aes_string(color = plot_color), size = 2) +
    geom_smooth(method = "lm", formula = 'y ~ x',
                color = 'black') +
    geom_text(aes(x = -Inf, y = Inf, label = eqn),
              data = eqn, color = 'black', size = 7,
              parse = TRUE, vjust = "inward", hjust = "inward") + 
    theme_bw() + 
    facet_wrap(as.formula(paste("~", facet_1)), scales = "free", ncol = 2) +
    ylab(y_label) +
    xlab(x_label) +
    theme(text = element_text(size = 18), legend.position = "top")
}

#________________________________________________________

#________________________________________________________
# plot timeseries data (default by qc color)
# takes a data frame and a column name of variable to plot
timeseries_plot_co <- function(df, y_var, x_var = "datetime",color_var="qc", marker_shape="location",facet_var = "HHID", y_units = "units") {
  
  ggplot(df, aes_string(y = y_var, x = x_var, color = color_var,shape = marker_shape)) +
    geom_line() +
    scale_fill_discrete(drop = FALSE) +
    facet_wrap(~df[[facet_var]], ncol = 1, scales = "free") +
    theme_minimal() +
    theme(legend.position = "top") +
    ylab(paste0(y_var, " (", df[[1, y_units]], ")")) +
    xlab(x_var)
  
}
#________________________________________________________

