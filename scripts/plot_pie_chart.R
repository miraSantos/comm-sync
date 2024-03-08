data <- data.frame(
  group=c("Full Annual","Partial Annual","Occasional"),
  value=c(40,42,29)
)
data$perc <- data$value/(40 + 42 + 29)

head(data)

labels= paste0(as.character(substr(round(data$perc,digits=2),3,4)),"%")


ggplot(data, aes(x = "", y = perc, fill = group)) +
  geom_col() +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+# remove background, grid, numeric labels
  theme_void()
  
ggsave(file="/home/mira/MIT-WHOI/github_repos/comm-sync/figures/sorting_species/pie_chart_split.png",
       width=800,height=600,units="px",dpi=130)
