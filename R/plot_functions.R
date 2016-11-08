#------------------------------------------------------------------------------------
# Functions to reproduce Figure 1, 2, and 3
#------------------------------------------------------------------------------------

################
### FIGURE 1 ###
################

plot_fig1 <- function(df_to_plot){
  plt <- ggplot(df_to_plot, aes(category, pop)) +
    geom_point(size = 5, colour="grey", fill = "grey", shape = 21) +
    geom_point(aes(category, samp), size = 5, colour="black", shape = 21) +
    scale_y_continuous(limits = c(0, 1)) +
    ylab("Proportion") +
    xlab("") +
    facet_wrap(~label, scales = "free_x") +
    theme_bw() +
    theme(
      text = element_text(size=10.5),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(face="bold", vjust=-0.5),
      axis.title.y = element_text(face="bold"),
      strip.text = element_text(face="bold", size=10.5)
    )
  return(plt)
}

################
### FIGURE 2 ###
################

plot_fig2 <- function(df_to_plot, label_full, label_active){
  plt <- ggplot(df_to_plot, aes(x=delta)) + 
    geom_histogram(aes(y = ..density..), bins = 15, 
                   fill = 'royalblue', color = 'grey50', alpha = 0.40) +
    geom_hline(aes(yintercept=0), color = 'grey70') +
    geom_segment(aes(x = low, xend = high, y = 0, yend = 0), size = 1, color = 'grey40') +
    geom_point(aes(x = x_bar, y = 0), shape = 21, fill = 'grey40', 
               color = 'grey40', size = 3) +
    geom_text(aes(x=x_bar, y=0, group=NULL), data = label_full, label = paste(round(label_full$x_bar, digits = 3)), vjust = 2.5, size = 3) +
    geom_text(aes(x=x_bar, y=0, group=NULL), data = label_active, label = paste(round(label_active$x_bar, digits = 3)), vjust = 2.5, size = 3) +
    facet_wrap(~label, nrow = 2, ncol = 1) +
    geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
    ylim(-.50,5.24) +
    ylab("Density\n") +
    xlab("\nFirst difference in the predicted probablity (Treatment - Control)") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(face="bold", size = 10)
    )
  return(plt)
}

################
### FIGURE 3 ###
################

plot_fig3_upper <- function(df_to_plot){
  # Set up labels
  lab1 <- df_to_plot[1,]
  
  plt <- ggplot(df_to_plot, aes(x=delta)) + 
    geom_hline(aes(yintercept=0), color = 'grey70') +
    geom_histogram(aes(y = ..density..), bins = 20, 
                   fill = 'royalblue', color = 'grey50', alpha = 0.40) +
    geom_segment(aes(x = low, xend = high, y = 0, yend = 0), size = 1, color = 'grey40') +
    geom_point(aes(x = x_bar, y = 0), shape = 21, fill = 'grey40', 
               color = 'grey40', size = 3) +
    geom_text(aes(x=x_bar, y=0, group=NULL), data=lab1, label = paste("$",round(lab1$x_bar, digits = 2), sep = ""), vjust = 2.5, size = 3) +
    facet_wrap(~label, nrow = 3, ncol = 2) +
    geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
    ylim(-.0015,.015) +
    ylab("Density") +
    xlab("") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(face="bold", size = 10)
    )
  return(plt)
}

plot_fig3_lower <- function(df_to_plot, post_medians){
  # Set up labels
  # $500 censored
  lab1 <- as.data.frame(cbind(post_medians[1,2], 0))
  names(lab1) <- c("x", "y")
  lab1$label <- post_medians[1,1]
  # $500 trimmed
  lab2 <- as.data.frame(cbind(post_medians[2,2], 0))
  names(lab2) <- c("x", "y")
  lab2$label <- post_medians[2,1]
  # $400 censored
  lab3 <- as.data.frame(cbind(post_medians[3,2], 0))
  names(lab3) <- c("x", "y")
  lab3$label <- post_medians[3,1]
  # $400 trimmed
  lab4 <- as.data.frame(cbind(post_medians[4,2], 0))
  names(lab4) <- c("x", "y")
  lab4$label <- post_medians[4,1]
  
  # Plot the censored and trimmed estimates
  plt <- ggplot(df_to_plot, aes(x=delta)) + 
    geom_hline(aes(yintercept=0), color = 'grey70') +
    geom_histogram(aes(y = ..density..), bins = 20, 
                   fill = 'royalblue', color = 'grey50', alpha = 0.40) +
    geom_segment(aes(x = low, xend = high, y = 0, yend = 0), size = 1, color = 'grey40') +
    geom_point(aes(x = x_bar, y = 0), shape = 21, fill = 'grey40', 
               color = 'grey40', size = 3) +
    geom_text(aes(x=x, y=y, group=NULL), data=lab1, label = paste("$",round(lab1$x, digits = 2), sep = ""), vjust = 2.5, size = 3) +
    geom_text(aes(x=x, y=y, group=NULL), data=lab2, label = paste("$", round(lab2$x, digits = 2), sep = ""), vjust = 2.5, size = 3) +
    geom_text(aes(x=x, y=y, group=NULL), data=lab3, label = paste("$", round(lab3$x, digits = 2), sep = ""), vjust = 2.5, size = 3) +
    geom_text(aes(x=x, y=y, group=NULL), data=lab4, label = paste("$", round(lab4$x, digits = 2), sep = ""), vjust = 2.5, size = 3) +
    facet_wrap(~label) +
    geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
    ylim(-.003,.023) +
    ylab("Density") +
    xlab("\nDifference in the Median WTA (Treatment - Control)") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      strip.text = element_text(face="bold", size = 10)
    )
  return(plt)
}

