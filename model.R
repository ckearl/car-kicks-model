install.packages("tidyverse")
install.packages("tidymodels")
install.packages("pins")
install.packages("vetiver")
install.packages("readr")
install.packages("lubridate")
library(tidyverse)
library(tidymodels)
library(pins)
library(vetiver)
library(readr)
library(lubridate)


path_to_model_export_directory <- '~/byU/wint_24/is555/group/is-555-g03-group-model-building-g_03_04/model_export/'

### 1. model_board, deployed_model, custom_metrics
model_board <- board_folder(path_to_model_export_directory)
pin_list(model_board)
model_name <- "kick_predictor"
deployed_model <- vetiver_pin_read(model_board, name = model_name)
custom_metrics <- metric_set(recall, specificity)

### 2. test_set_results_ordered, metrics_from_training_window, training_performance_plot
revised_test_set_results <- read_csv('https://www.dropbox.com/scl/fi/knscoha3v97d7ig4qspy5/g_03_04-test_set_results-with_dates.csv?rlkey=dcfh4jzhntkmfl7wfsbovozju&dl=1')
test_set_results_ordered <- revised_test_set_results %>%
  arrange(date_col) %>%
  mutate(
    .pred_class = as_factor(.pred_class),
    is_bad_buy = as_factor(is_bad_buy)
  )

metrics_from_training_window <- test_set_results_ordered %>%
  filter(date_col > as.Date('2021-04-01')) %>%
  vetiver_compute_metrics(
  date_col,
  period = "month",
  truth = is_bad_buy,
  estimate = .pred_class,
  metric_set = custom_metrics
)

training_performance_plot <- metrics_from_training_window %>% vetiver_plot_metrics()
training_performance_plot
training_performance_plot %>% ggsave('training_performance_plot.png', plot = .,
                                     device = 'png', width = 14, height = 9)

pin_write(model_board, metrics_from_training_window, name = "performance_metrics")

### 3. future_data_clean
future_data_raw <- read_csv('https://www.dropbox.com/scl/fi/addf1j1ufdt9mmodv4c0y/g_03_04-holdout_data.csv?rlkey=0rmpdenwrhhcfbmnfdhcbha7e&dl=1')
future_data_raw %>% select(date_col)
future_data_clean <- future_data_raw %>%
  mutate(
  id = row_number(),
  mmr_acq_auction_avg_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_average_price)), mmr_acquisition_auction_average_price, as.numeric((mmr_acquisition_auction_average_price))),
  mmr_acq_auction_clean_price = ifelse(is.na(as.numeric(mmr_acquisition_auction_clean_price)), mmr_acquisition_auction_clean_price, as.numeric((mmr_acquisition_auction_clean_price))),
  mmr_acq_retail_avg_price = ifelse(is.na(as.numeric(mmr_acquisition_retail_average_price)), mmr_acquisition_retail_average_price, as.numeric((mmr_acquisition_retail_average_price))),
  mmr_acq_retail_clean_price = ifelse(is.na(as.numeric(mmr_acquisiton_retail_clean_price)), mmr_acquisiton_retail_clean_price, as.numeric((mmr_acquisiton_retail_clean_price))),
  mmr_curr_auction_avg_price = ifelse(is.na(as.numeric(mmr_current_auction_average_price)), mmr_current_auction_average_price, as.numeric((mmr_current_auction_average_price))),
  mmr_curr_auction_clean_price = ifelse(is.na(as.numeric(mmr_current_auction_clean_price)), mmr_current_auction_clean_price, as.numeric((mmr_current_auction_clean_price))),
  mmr_curr_retail_avg_price = ifelse(is.na(as.numeric(mmr_current_retail_average_price)), mmr_current_retail_average_price, as.numeric((mmr_current_retail_average_price))),
  mmr_curr_retail_clean_price = ifelse(is.na(as.numeric(mmr_current_retail_clean_price)), mmr_current_retail_clean_price, as.numeric((mmr_current_retail_clean_price))),
  purchase_date = as_datetime(purch_date),
  auction_name = auction,
  transmission = ifelse(transmission == "Manual", "MANUAL", transmission),
  year = as.numeric(veh_year),
  age = as.numeric(vehicle_age),
  odometer = as.numeric(veh_odo),
  buyer_number = as.numeric(byrno),
  vendor_zip = as.numeric(vnzip1),
  auction_final_cost = as.numeric(veh_b_cost),
  vendor_state = vnst,
  prime_unit = primeunit,
  region = case_when(
    make %in% c("ACURA", "HONDA", "HYUNDAI", "INFINITI", "ISUZU", "LEXUS", "MITSUBISHI", "NISSAN", "TOYOTA", "TOYOTA SCION", "SCION", "SUBARU", "SUZUKI") ~ "Japan",
    make %in% c("VOLKSWAGEN") ~ "Germany",
    make %in% c("BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "GMC", "HUMMER", "JEEP", "LINCOLN", "OLDSMOBILE", "PONTIAC", "SATURN", "MERCURY", "PLYMOUTH") ~ "USA",
    make %in% c("MINI") ~ "UK",
    make %in% c("VOLVO") ~ "Sweden",
    make %in% c("KIA", "HYUNDAI") ~ "South Korea",
    TRUE ~ "Other"
  ),
  parent_company = case_when(
    make %in% c("BUICK", "CADILLAC", "CHEVROLET", "GMC", "HUMMER", "OLDSMOBILE", "PONTIAC", "SATURN") ~ "GM",
    make %in% c("FORD", "LINCOLN", "MERCURY") ~ "Ford",
    make %in% c("CHRYSLER", "DODGE", "JEEP", "PLYMOUTH") ~ "Fiat-Chrysler",
    make %in% c("ACURA", "HONDA") ~ "Honda",
    make %in% c("HYUNDAI", "KIA") ~ "Hyundai",
    make %in% c("MAZDA") ~ "Mazda",
    make %in% c("MITSUBISHI") ~ "Mitsubishi",
    make %in% c("NISSAN", "INFINITI") ~ "Nissan",
    make %in% c("SUBARU") ~ "Subaru",
    make %in% c("SUZUKI") ~ "Suzuki",
    make %in% c("TOYOTA", "LEXUS", "SCION", "TOYOTA SCION") ~ "Toyota",
    make %in% c("VOLKSWAGEN") ~ "Volkswagen",
    make %in% c("VOLVO") ~ "Volvo",
    TRUE ~ "Other"
  )) %>%
  select(-c(purch_date, veh_year, vehicle_age, veh_odo, wheel_type_id, byrno, vnzip1, vnst, auction, primeunit, mmr_acquisition_auction_average_price, mmr_acquisition_auction_clean_price, mmr_acquisition_retail_average_price, mmr_acquisiton_retail_clean_price, mmr_current_auction_average_price, mmr_current_auction_clean_price, mmr_current_retail_average_price, mmr_current_retail_clean_price)) %>%
  select(id, is_bad_buy, date_col, make, top_three_american_name, parent_company, region, nationality, model, trim, sub_model, size, color, transmission, year, age, odometer, prime_unit, wheel_type, mmr_acq_auction_avg_price, mmr_acq_auction_clean_price, mmr_acq_retail_avg_price, mmr_acq_retail_clean_price, mmr_curr_auction_avg_price, mmr_curr_auction_clean_price, mmr_curr_retail_avg_price, mmr_curr_retail_clean_price, auction_name, vendor_zip, vendor_state, is_online_sale, warranty_cost, buyer_number, auction_final_cost) %>%
  rename(retail_avg_price = mmr_curr_retail_avg_price,
         auction_avg_price = mmr_curr_auction_avg_price,
  ) %>%
  mutate(is_bad_buy = as.factor(is_bad_buy)) %>%
  select(-prime_unit,
         - color,
         - top_three_american_name,
         - trim,
         - id,
         - nationality,
         - mmr_acq_auction_avg_price,
         - mmr_acq_auction_clean_price,
         - mmr_acq_retail_avg_price,
         - mmr_acq_retail_clean_price,
         - mmr_curr_auction_clean_price,
         - mmr_curr_retail_clean_price,
         - buyer_number
  ) %>%
  mutate(year = as.character(year),
         vendor_zip = as.character(vendor_zip),
         vendor_zip = ifelse(vendor_zip %in% c('2764', '3106', '8505'), paste0("0", vendor_zip), vendor_zip),
         profit_margin = retail_avg_price - auction_avg_price
  ) %>%
  arrange(date_col)

future_data_clean %>% count(make, model) %>%
  arrange(desc(n)) %>%
  print(n = 10)

# select a random car where make = "CHEVROLET" and model = "IMPALA"
randomfuture_data_clean %>%
  filter(make == "CHEVROLET" & model == "IMPALA") %>%
  select(date_col, make, model, sub_model, size, transmission, year, age, odometer, wheel_type, auction_avg_price, retail_avg_price, auction_name, vendor_zip, vendor_state, is_online_sale, warranty_cost, auction_final_cost, profit_margin) %>%
  sample_n(1)

### 4. future_data_predictions, metrics_from_future_data, all_time_performance_plot
future_data_predictions <- future_data_clean %>%
  mutate(.pred_class = predict(deployed_model, new_data = future_data_clean)$.pred_class) %>%
  select(date_col, .pred_class, is_bad_buy, make, parent_company, region, model, sub_model, size, transmission, year, age, odometer, wheel_type, auction_avg_price, retail_avg_price, auction_name, vendor_zip, vendor_state, is_online_sale, warranty_cost, auction_final_cost, profit_margin)

random_car <- future_data_predictions %>%
  filter(make == "CHEVROLET",
         model == "IMPALA",
         .pred_class == 1,
         is_bad_buy == 1) %>%
         sample_n(1)

for (col_name in colnames(random_car)) {
  cat(col_name, ": ", random_car[[col_name]], "\n")
}

metrics_from_future_data <- future_data_predictions %>%
  filter(date_col > as.Date('2021-04-01')) %>%
  vetiver_compute_metrics(
    date_col,
    period = "month",
    truth = is_bad_buy,
    estimate = .pred_class,
    metric_set = custom_metrics
  )

model_board %>% vetiver_pin_metrics(metrics_from_future_data, metrics_pin_name = "performance_metrics", overwrite = F)

all_time_performance_plot <- model_board %>% pin_read("performance_metrics") %>% vetiver_plot_metrics()
all_time_performance_plot %>% ggsave('all_time_performance_plot.png', plot = .,
                                     device = 'png', width = 14, height = 9)
all_time_performance_plot
