#!/usr/bin/Rscript --no-site-file --no-init-file

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

mytheme <- theme_minimal(base_size = 8) +
  theme(aspect.ratio = 0.75) +
  theme(legend.title = element_blank(), legend.position = "top") +
  theme(plot.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(panel.grid = element_line(colour = "black", linetype = "solid")) +
  theme(axis.line = element_line(color = "black")) +
  theme(axis.ticks.x = element_line(), axis.ticks.y = element_line()) +
  theme(legend.margin=margin(b = -0.2, l = -0.2, r = 0, unit = "cm"))

cbPalette <- c("#D55E00", "#E69F00", "#009E73", "#0072B2", "#56B4E9", "#D55E00", "#F0E442", "#CC79A7", "#999999")
output_dir <- "."

read_data <- function(file) read.delim(file = file, header = TRUE, skip = 5, sep = "", stringsAsFactors = FALSE)

interpol_year <- function(x, y, min_x, max_x) approx(
  x,
  y,
  xout = seq(min_x, max_x, by = 1),
  method = "constant",
  rule = 1:2
)

interpol2_year <- function(x, y, xout) approx(
  x,
  y,
  xout = xout,
  method = "constant",
  rule = 1:2
)

data_amd <- read_data("data-amd.txt")
data_nvidia_sp <- read_data("data-sp-nvidia.txt")
data_nvidia_dp <- read_data("data-dp-nvidia.txt")
data_intel <- read_data("data-intel.txt")
data_pcie <- read_data("data-pcie.txt")
data_nvlink <- read_data("data-nvlink.txt")
#data_intel_phi <- read_data("data-intel-phi.txt")

device_data <- bind_rows(
  AMD = select(data_amd, X.Year, BW = Mem.BW, Name, Bus),
  Nvidia = select(data_nvidia_dp, X.Year, BW = Mem.BW, Name, Bus),
  Intel = select(data_intel, X.Year, BW = Mem.BW.Socket, Name),
  .id = "Vendor"
)

bus_data <- bind_rows(
  "PCI-e" = data_pcie,
  NVLink = data_nvlink,
  .id = "Bus"
)

device_with_bus <- left_join(device_data, bus_data, by = c("Bus" = "Name")) %>%
  filter(is.na(Bus) == FALSE)

device_with_best_bus <- inner_join(
  device_with_bus,
  group_by(device_with_bus, Vendor, Name) %>%
  summarise(Bus.BW = max(Bus.BW)),
  by = c("Vendor", "Name", "Bus.BW"))

bandwidth_data <- bind_rows(
  device_data,
  mutate(bus_data, Vendor = Bus, Bus = NULL, BW = Bus.BW, Bus.BW = NULL),
  .id = NULL
) %>%
  filter(min(device_data$X.Year) <= X.Year, X.Year <= max(device_data$X.Year))

interpol_pcie <- data.frame(interpol2_year(data_pcie$X.Year, data_pcie$Bus.BW, bandwidth_data$X.Year))
interpol_nvlink <- data.frame(interpol2_year(data_nvlink$X.Year, data_nvlink$Bus.BW, bandwidth_data$X.Year))
interpol_bus <- data.frame(x = interpol_pcie$x, y = pmax(interpol_pcie$y, interpol_nvlink$y, na.rm = TRUE))

bandwidth_data <- bandwidth_data %>% mutate(
  pcie_vs_BW = bandwidth_data$BW / interpol_pcie$y,
  bus_vs_BW = bandwidth_data$BW / interpol_bus$y
  )

bus_data <- bus_data %>% mutate(
  telsa_p100_vs_bus = filter(data_nvidia_dp, Name == "Tesla P100", Bus == "NVLink 1.0")$Mem.BW / Bus.BW
)

png(filename = paste0(output_dir, "/", "bandwidth_raw", ".png"), width = 1.5 * 3.32153, height = 1.5 * 2.1, pointsize = 11, units = "in", res = 300)
ggplot(bandwidth_data, aes(x = X.Year, y = BW, group = Vendor, colour = Vendor)) +
  geom_point() +
  labs(x = "Year", y = "Bandwidth Ratio (×)") +
  scale_colour_manual(values = cbPalette) +
  mytheme
dev.off()

png(filename = paste0(output_dir, "/", "bandwidth_ratio_latest_pcie_baseline", ".png"), width = 1.5 * 3.32153, height = 1.5 * 2.1, pointsize = 11, units = "in", res = 300)
ggplot(bandwidth_data, aes(x = factor(X.Year), y = pcie_vs_BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point() +
  labs(x = "Year", y = "Bandwidth Ratio (×)") +
  scale_colour_manual(values = cbPalette) +
  mytheme
dev.off()

png(filename = paste0(output_dir, "/", "bandwidth_ratio_best_bus_baseline", ".png"), width = 1.5 * 3.32153, height = 1.5 * 2.1, pointsize = 11, units = "in", res = 300)
ggplot(bandwidth_data, aes(x = factor(X.Year), y = bus_vs_BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point() +
  labs(x = "Year", y = "Bandwidth Ratio (×)") +
  scale_colour_manual(values = cbPalette) +
  mytheme
dev.off()

png(filename = paste0(output_dir, "/", "bandwidth_ratio_real_bus_baseline", ".png"), width = 1.5 * 3.32153, height = 1.5 * 2.1, pointsize = 11, units = "in", res = 300)
ggplot(device_with_best_bus, aes(x = factor(X.Year.x), y = BW / Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point() +
  labs(x = "Year", y = "Bandwidth Ratio (×)") +
  scale_colour_manual(values = cbPalette) +
  mytheme
dev.off()

ggplot(bandwidth_data, aes(x = X.Year, y = BW / filter(data_pcie, Name == "PCI-e 3.0")$Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point()

ggplot(bandwidth_data, aes(x = X.Year, y = BW / filter(data_pcie, Name == "PCI-e 4.0")$Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point()

ggplot(bandwidth_data, aes(x = X.Year, y = BW / filter(data_pcie, Name == "PCI-e 5.0")$Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point()

ggplot(bandwidth_data, aes(x = X.Year, y = BW / filter(data_nvlink, Name == "NVLink 1.0")$Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point()

ggplot(bandwidth_data, aes(x = X.Year, y = BW / filter(data_nvlink, Name == "NVLink 2.0")$Bus.BW, group = Vendor, colour = Vendor)) +
  geom_path() +
  geom_point()

ggplot(bus_data, aes(x = X.Year, y = telsa_p100_vs_bus), group = Bus, colour = Bus) +
  geom_point()
