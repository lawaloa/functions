# Converting pounds to kilograms ----
pounds_to_kg <- function(pounds){
  kg <- pounds * 0.4536
  return(kg)
}

# Transforms age in years to age in months.----
years_to_months <- function(years){
  month <- years * 12
  return(month)
}

# Converting Fahrenheit to Celsius.----
fahrenheit_to_celsius <- function(fahrenheit){
  celsius <- ((5/9) * (fahrenheit - 32))
  return(celsius)
}

# Converting Celsius to Fahrenheit.----
celsius_to_fahrenheit <- function(celsius){
  fahrenheit <- celsius*1.8+32
  return(fahrenheit)
}

# Calculate calories from macronutrients.----
calculate_calories <- function(carb_grams = 0, protein_grams=0, fat_grams = 0) {
  result <- (carb_grams * 4) + (protein_grams * 4) + (fat_grams * 9)
  return(result)
}


# Calculate bmi.----
calc_bmi <- function(weight=0, height=0){
  bmi <- weight/((height/100)^2)
  return(bmi)
}

# Converting celsius scale to other common sacle.----
celsius_convert <- function(celsius, convert_to){
  
  # Checking validity
  if (!convert_to %in% c("fahrenheit", "kelvin", "centigrade")){
    stop("convert to must be one of 'fahrenheit; or 'kelvin'")
  }
  
  # Converting value
  if (convert_to == "fahrenheit"){
    out <- (celsius * 9/5) + 32
  } else if (convert_to == "kelvin"){
    out <- celsius + 273.15
  } else if (convert_to == "centigrade"){
    out <- celsius
  }
  return(out)
}

# Calories calculation from macronutrient.----
calculate_calories2 <- function(carb_grams = 0, protein_grams = 0, fat_grams = 0) {
  
  # your code here
  if (!is.numeric(c(carb_grams, protein_grams, fat_grams))){
    stop("All arguments must be numeric")
  } 
  result <- (carb_grams * 4) + (protein_grams * 4) + (fat_grams * 9)
  return(result)
}

# Classify body temperatue of humans.----
classify_temp <- function(temp){
  out <-  ifelse(temp < 35, "hypothermia",
                 ifelse(temp >= 35 & temp <= 37, "normal", 
                        ifelse(temp > 37, "fever", "NA")))
  return(out)
}

# or

classify_temp2 <- function(temp) {
  dplyr::case_when(
    temp < 35 ~ "hypothermia",
    temp >= 35 & temp <= 37 ~ "normal",
    temp > 37 ~ "fever",
    TRUE ~ NA_character_
  )
}

# Calculate isoniazide dosage.----
calculate_isoniazid_dosage2 <- function(weight) {
  if (any(weight < 30)) stop("Weights must all be at least 30 kg.")
  
  # Your code here  
  {
    out <- ifelse(weight <= 35, 150,
                  ifelse(weight <= 45, 200,
                         ifelse(weight <= 55, 300,
                                ifelse(weight <= 70, 300, 300))))
    return(out)
    
  }
  return(out)
}

# or

calculate_isoniazid_dosage2 <- function(weight) {
  if (any(weight < 30)) stop("Weights must all be at least 30 kg.")
  
  dosage <- case_when(
    weight <= 35 ~ 150,
    weight <= 45 ~ 200,
    weight <= 55 ~ 300,
    weight <= 70 ~ 300,
    TRUE ~ 300
  )
  return(dosage)
}