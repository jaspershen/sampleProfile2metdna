# path <- "V:/workreport/Shen Xiaotao/demo/metDNA/fly20180712/POS"
#' @title sampleProfile2metdna
#' @description sampleProfile2metdna
#' @author Xiaotao Shen
#' \email{shenxt1990@@163.com}
#' @param path Absolute directory
#' @param width width, in
#' @param height height, in
#' @return A png figure
#' @export

sampleProfile2metdna <- function(path = ".",
                                 width = 10,
                                 height = 6){
  file.name <- dir(path)

  if(any(file.name == "data.csv")){
    data <- readr::read_csv(file.path(path, "data.csv"))
    int <- log(apply(data[,-c(1:3)], 1, mean), 10)
    data <- data.frame(data, int, stringsAsFactors = FALSE)
    data$mz <- as.numeric(data$mz)
    data$rt <- as.numeric(data$rt)

    plot <-  ggplot2::ggplot(data, ggplot2::aes(x = rt, y = mz, colour = int)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_color_gradient(low = "royalblue", high = "firebrick1") +
      ggplot2::labs(x = "Retention time (RT, second)",
                    y = "Mass to charge ratio (m/z)",
                    colour = "log10(intensity)")+
      # ggplot2::ggtitle(paste(length(temp.mz), "spectra in total"))+
      my.theme

    ggplot2::ggsave(filename = "plot.png", plot = plot, path = path,
                    width = width, height = height,
                    units = "in")
  }else{
   plot <- ggplot2::ggplot()+
     ggplot2::labs(x = "Retention time (RT, second)",
                   y = "Mass to charge ratio (m/z)",
                   colour = "log10(intensity)")+
     ggplot2::ggtitle("NO data are provided!")+
     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 20))+
     my.theme
   ggplot2::ggsave(filename = "plot.png", plot = plot, path = path,
                   width = width, height = height,
                   units = "in")
  }

}






my.theme <- ggplot2::theme_bw()+
  ggplot2::theme(axis.title.x = ggplot2::element_text(size = 18),
                 axis.title.y = ggplot2::element_text(size = 18)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(size = 15),
                 axis.text.y = ggplot2::element_text(size = 15)) +
  ggplot2::theme(legend.title = ggplot2::element_text(size = 12)) +
  ggplot2::theme(legend.text = ggplot2::element_text(size = 10))
