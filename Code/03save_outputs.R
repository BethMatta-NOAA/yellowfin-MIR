###YFS MARGINAL INCREMENT ANALYSIS


source("./Code/02analyze_data.R")


# SAVE PLOTS --------------------------------------------------------------

#Monthly boxplots

ggsave(plot=DMIR.boxplot, filename="./Output/DMIR_boxplot.png",
       dpi=300, width = 6, height = 5)

ggsave(plot=VMIR.boxplot, filename="./Output/VMIR_boxplot.png",
       dpi=300, width = 6, height = 5)

ggsave(plot=DMIR.stage.boxplot, filename="./Output/DMIR_stage_boxplot.png",
       dpi=300, width = 6, height = 5)

ggsave(plot=VMIR.stage.boxplot, filename="./Output/VMIR_stage_boxplot.png",
       dpi=300, width = 6, height = 5)


# Model fits

ggsave(plot=fitted.gam, filename="./Output/Model fits.png",
       dpi=300, width = 8, height = 5)



# SAVE PREDICTED VALUES ---------------------------------------------------


write.csv(mypredict, file = "./Output/fitted_DMIR_values.csv", row.names = FALSE)




