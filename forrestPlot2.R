library(grid)
library(forestploter)
library(tidyverse)

###### ONE columns NO groups
fp_dt <- readxl::read_excel(here::here('OR_fam_est_CIs_forestplotter_v2.xlsx'), sheet = 'Sheet4')
fp_dt$` ` <- paste(rep(" ", 5), collapse = " ")
fp_dt$`   ` <- paste(rep(" ", 20), collapse = " ")
fp_dt$`  ` <- paste(rep(" ", 5), collapse = " ")
fp_dt$`Team and Timeframe` <- if_else(stringr::str_detect(fp_dt$`Team and Timeframe`,"team|Extended"),
                                      fp_dt$`Team and Timeframe`,
                                      paste0("   ",fp_dt$`Team and Timeframe`))
# fp_dt$`Estimate [CI]` <- ifelse(is.na(fp_dt$est), "",
#                          sprintf("%.2f [%.2f to %.2f]",
#                                  fp_dt$est, fp_dt$ll, dt$ul))

fp_dt <- fp_dt %>%
  mutate(
    `Estimate [95% CI]` = if_else(
      is.na(est),
      "",
      paste0(est," [",ll," to ",ul,"]"))
  )


p <- forest(fp_dt[,c(1,5,6,7,8)],
            est = fp_dt$est,
            lower = fp_dt$ll,
            upper = fp_dt$ul,
            ci_column = 3,
            ref_line = 1,
            xlim = c(.9,1.4),
            ticks_at = c(.9,1,1.1,1.2,1.3,1.4),
            arrow_lab = c("Favours team familiarity","Does not favour team familiarity"),
            # vert_line = c(0.5, 2),
            nudge_y = 0.2#,
            # theme = tm
            )
# Add underline at the bottom of the header
# p <- forestploter::add_underline(p, part = "header")
p <- forestploter::add_border(p, part = "header")
p <- edit_plot(p,
               row = c(1, 10),
               gp = gpar(fontface = "bold"))
p <- edit_plot(p,
               row = c(2, 6, 11, 15),
               gp = gpar(fontface = "italic"))
p <- insert_text(p,
                 text = "Figure 2. Summary of Estimates",
                 col = 1:5,
                 part = "header",
                 gp = gpar(fontface = "bold"))
plot(p)






###### two columns NO groups

fp_dt <- readxl::read_excel(here('OR_fam_est_CIs_forestplotter.xlsx'), sheet = 'Sheet3')

# Add two blank column for CI
fp_dt$`Extended Room Time` <- paste(rep(" ", 20), collapse = " ")
fp_dt$`Extended Length of Stay` <- paste(rep(" ", 20), collapse = " ")

fp_dt$`Team and Timeframe` <- if_else(stringr::str_detect(fp_dt$`Team and Timeframe`,"team"),
                                      fp_dt$`Team and Timeframe`,
                                      paste0("   ",fp_dt$`Team and Timeframe`))

# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   footnote_col = "blue",
                   legend_name = "Team Members",
                   legend_value = c("Core", "All"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa")
                   )

p <- forest(fp_dt[,c(1:5)],
            est = list(fp_dt$est_RT,
                       fp_dt$est_LOS),
            lower = list(fp_dt$ll_RT,
                         fp_dt$ll_LOS), 
            upper = list(fp_dt$ul_RT,
                         fp_dt$ul_LOS),
            ci_column = c(2,4),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)
# Add underline at the bottom of the header
p <- add_underline(p, part = "header")
p <- edit_plot(p,
               row = c(1, 5),
               gp = gpar(fontface = "bold"))
plot(p)





### two columns and two groups
fp_dt <- readxl::read_excel(here('OR_fam_est_CIs_forestplotter.xlsx'), sheet = 'Sheet2')

# Add two blank column for CI
fp_dt$`Extended Room Time` <- paste(rep(" ", 20), collapse = " ")
fp_dt$`Extended Length of Stay` <- paste(rep(" ", 20), collapse = " ")



# Set-up theme
tm <- forest_theme(base_size = 10,
                   refline_lty = "solid",
                   ci_pch = c(15, 18),
                   ci_col = c("#377eb8", "#4daf4a"),
                   footnote_col = "blue",
                   legend_name = "Team Members",
                   legend_value = c("Core", "All"),
                   vertline_lty = c("dashed", "dotted"),
                   vertline_col = c("#d6604d", "#bababa"))

p <- forest(fp_dt[,c(1, 14,15)],
            est = list(fp_dt$est_RT_core,
                       fp_dt$est_LOS_core,
                       fp_dt$est_RT_all,
                       fp_dt$est_LOS_all),
            lower = list(fp_dt$ll_RT_core,
                         fp_dt$ll_LOS_core,
                         fp_dt$ll_RT_all,
                         fp_dt$ll_LOS_all), 
            upper = list(fp_dt$ul_RT_core,
                         fp_dt$ul_LOS_core,
                         fp_dt$ul_RT_all,
                         fp_dt$ul_LOS_all),
            ci_column = c(2,3),
            ref_line = 1,
            vert_line = c(0.5, 2),
            nudge_y = 0.2,
            theme = tm)

plot(p)