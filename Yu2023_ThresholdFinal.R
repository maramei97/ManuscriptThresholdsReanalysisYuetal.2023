##################Contact angle#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "contact_angle", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "contact_angle", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "contact_angle", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "contact_angle", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "contact_angle", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PP field-weathered (% w/w)",
    y      = "Contact angle (°)",
    title  = "Contact angle vs PP field-weathered",
    caption= "Breakpoint ψ̂ = 1.22 % w/w"
  )

print(p)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "contact_angle", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "contact_angle", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "contact_angle", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "Contact angle (°)",
    title  = "Contact angle vs PES pristine",
    caption= "Breakpoint φ̂ = 0.12 % w/w"
  )

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "contact_angle", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "contact_angle", pred = "conc_w")
summary_s2(res)

##################Bulk density#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "rho_b", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x = "PP field-weathered (% w/w)",
    y = "Bulk density (g\u00A0cm\u207B\u00B3)",
    caption = NULL)

## remove or comment the earlier print(p)
# print(p)

## --- replace the old extra styling & annotation with this ---
psi <- res$psi_best

## y-position at the breakpoint using the selected best model
thr_var <- paste0("conc_w","_thr")
nd <- data.frame(conc_w = psi)
if (!is.null(res$best_type) && res$best_type %in% c("segmented","stegmented","M11"))
  nd[[thr_var]] <- psi

y_psi <- tryCatch(as.numeric(predict(res$best, newdata = nd, type = "response")),
                  error = function(e) NA_real_)
if (!is.finite(y_psi)) {
  ## fallback to GAM if needed
  y_psi <- as.numeric(predict(res$gam, newdata = data.frame(conc_w = psi)))
}

## nudge the label a bit to the right of the red line
## --- lowercase φ with centered hat, drawn above the panel (no warnings) ---
## --- lowercase φ with centered hat, drawn above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # place at top
           label = lab_chr,              # character string
           parse = TRUE,                 # plotmath centers the hat
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "rho_b", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "Bulk density (g cm⁻³)",
    caption = expression(hat(psi) == 0.24 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,
           label = lab_chr,
           parse = TRUE,
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "rho_b", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "Bulk density (g cm⁻³)",
    title  = "Bulk density vs PES field-weathered",
    caption = expression(hat(psi) == 0.06 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(title = NULL, caption = NULL) +   # override the title & caption in your labs()
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,
           label = lab_chr,
           parse = TRUE,
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

##################Saturated hydraulic conductivity#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "Ks", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "Ks", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PP pristine (% w/w)",
    y      = "Ks (m d⁻¹)",
    caption= "Breakpoint ψ̂ = 0.61 % w/w"
  )

print(p)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "Ks", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "Ks", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "Ks", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "Ks (m d⁻¹)",
    caption = expression(hat(psi) == 0.24 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove old caption
  coord_cartesian(clip = "off") +        # allow label above panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # put at top of panel
           label = lab_chr,
           parse = TRUE,                 # plotmath: hat(varphi)
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "Ks", pred = "conc_w")
summary_s2(res)

##################WHC Volumetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
summary_s2(res)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "WHC_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "quadratic"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "WHC volumetric (cm³ cm⁻³)",
    caption = expression(hat(psi) == 0.06 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove caption
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,
           label = lab_chr,
           parse = TRUE,
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

##################WHC Gravimetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
summary_s2(res)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "WHC_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "WHC_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "WHC Gravimetric (g g⁻¹)",
    caption = expression(hat(psi) == 0.06 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove caption
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,
           label = lab_chr,
           parse = TRUE,
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

##################Field Capacity Volumetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "FC_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "FC Volumetric (cm³ cm⁻³)",
    caption = expression(hat(psi) == 0.24 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "FC_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "FC Volumetric (cm³ cm⁻³)",
    caption= "Breakpoint ψ̂ = 0.24 % w/w"
  )

print(p)

##################Field Capacity Gravimetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "FC_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "FC Gravimetric (g g⁻¹)",
    caption= "Breakpoint ψ̂ = 0.24 % w/w"
  )

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "FC_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "FC_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "FC Gravimetric (g g⁻¹)",
    caption= "Breakpoint ψ̂ = 0.24 % w/w"
  )

print(p)

##################Permanent Wilting Point Volumetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PP pristine (% w/w)",
    y      = "PWP Volumetric (cm³ cm⁻³)",
    caption= "Breakpoint ψ̂ = 1.22 % w/w"
  )

print(p)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "PWP Volumetric (cm³ cm⁻³)",
    caption = expression(hat(psi) == 0.12 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Volumetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "PWP Volumetric (cm³ cm⁻³)",
    caption = expression(hat(psi) == 0.06 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###Revisar PWP V. for PES field-weathered

##################Permanent Wilting Point Gravimetric#####################

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PP pristine (% w/w)",
    y      = "PWP Gravimetric (g g⁻¹)",
    caption= "Breakpoint ψ̂ = 1.22 % w/w"
  )

print(p)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES pristine (% w/w)",
    y      = "PWP Gravimetric (g g⁻¹)",
    caption = expression(hat(psi) == 0.12 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "PWP_Gravimetric", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "PWP_Gravimetric", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "PWP Gravimetric (g g⁻¹)",
    caption = expression(hat(psi) == 0.06 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)

##################Mean weight diameter#####################

d <- d %>% mutate(
  mean_weight_diameter =
    as.numeric(gsub(",", ".", mean_weight_diameter))   # swaps commas, then numeric
)

# optionally drop rows still missing after coercion
d <- d %>% filter(!is.na(mean_weight_diameter))

###PP Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
summary_s2(res)

###PP Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PP Granule", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
summary_s2(res)

###PES Pristine###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Pristine")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "rho_b", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
summary_s2(res)

###PES Field-weathered###

# source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctions.R")
source("C:/Users/Tamara/Desktop/PhD/Manuscripts/Yu2023_ThresholdFunctionsv2.R")
df_fw_seg <- d %>% filter(Type == "PES Fiber", Modification == "Field-weathered")
# res <- find_threshold_models(df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
# res <- find_threshold_models(df_fw_seg, resp = "Ks", pred = "conc_w")
res <- find_threshold_models(df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
print(res)
# autoplot(res, df_fw_seg, resp = "rho_b", pred = "conc_w")
# autoplot(res, df_fw_seg, resp = "PWP_Volumetric", pred = "conc_w")
p = autoplot(res, df_fw_seg, resp = "mean_weight_diameter", pred = "conc_w")
summary_s2(res)

### Improve visuals ###
## capture the plot
p <- autoplot(res, df_fw_seg,
              resp = "mean_weight_diameter", pred = "conc_w",
              show = FALSE)

## customised colours but KEEP the original labels
p <- p +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.line  = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")) +
  scale_colour_manual(
    values = c(best = "#0072B2",          # blue
               gam  = "#999999"),         # grey
    breaks = c("best", "gam"),            # <-- keep these
    labels = c(res$best_type, "GAM smooth"),   # "stegmented", …
    name   = "fit"                        # legend title
  ) +
  labs(
    x      = "PES field-weathered (% w/w)",
    y      = "MWD (mm)",
    caption = expression(hat(psi) == 0.24 ~ "% w/w")
  )

## --- add-on: lowercase φ̂ label above the panel (no warnings) ---
x_nudge <- diff(range(df_fw_seg$conc_w, na.rm = TRUE)) * 0.03
lab_chr <- paste0("hat(psi) == ", sprintf("%.2f", res$psi_best), " * ' % w/w'")

p <- p +
  labs(caption = NULL) +                 # remove the old caption
  coord_cartesian(clip = "off") +        # allow drawing above the panel
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  annotate("text",
           x = res$psi_best + x_nudge,
           y = Inf,                      # top of panel
           label = lab_chr,              # lowercase φ with hat
           parse = TRUE,                 # plotmath for proper hat centering
           colour = "red",
           fontface = "bold",
           hjust = 0, vjust = 1.2)

print(p)