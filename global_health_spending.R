library(tidyverse)
library(gtsummary)
library(dplyr)
library(readr)
health_spending <- read_csv("~/RSPH Classes/EPI590R RStudio/final-project/final-project/repository/health_spending.csv")
View(health_spending)


#How has the balance between government, private, and external aid changed over time in Guyana and Haiti?

#Create a {gtsummary} table of descriptive statistics about your data
health_spending_filtered <- health_spending %>%
	filter(country_name %in% c("Guyana", "Haiti")) #needed to filter to get government, private and external
#aid from the indicator code variable

health_spending_wide <- health_spending_filtered %>%
	pivot_wider( #takes values from one column and spreads them out into new columns, using another column to supply the new column names
		id_cols = c(country_name, year), #
		names_from = indicator_code,
		values_from = value
	) #Guyana and Haiti and fill them with whatever in value

#convert year to categorical (went back to numeric)
health_spending_wide$year <- as.numeric(health_spending_wide$year)

tbl_summary(
	health_spending_wide,
	by = country_name, include = c(ext_che, gghed_che, pvtd_che),
	label = list( ext_che ~ "External aid %",
								gghed_che ~ "Government aid %",
								pvtd_che ~ "Private aid %"),
	missing_text = "Missing") |>
	add_p(test = list(
		all_continuous() ~ "t.test",
		all_categorical() ~ "chisq.test")) |>
			bold_labels() |>
			remove_footnote_header()|>
			modify_header(
				label = "**Type of Aid**",
				p.value = "**P-value**") |>
			modify_caption("Aid Percentage Comparsion")

o#Fit a regression and present well-formatted results from the regression

linear_model <- (lm(ext_che ~ year * country_name, data = health_spending_wide))
linear_model

#is.numeric(health_spending_wide$year)

tbl_regression(linear_model, intercept = TRUE,
							 label = list(year ~ "Year (trend)",
							 						 country_name ~ "Country",
							 						 `year:country_name` ~ "Trend difference (Haiti vs Guyana"))
library(ggplot2)
ggplot(health_spending_wide, aes(x = year, y = ext_che, color = country_name)) +
	geom_point(alpha = 0.4) + #draws the actual data as dots, positioning the x & y. alpha controls the transparency
	geom_smooth(method = "lm", se = TRUE) + #visusallizes your regression. method = fits the straight line linear regression. se = is standard error
	labs(
		title = "External Aid Share of Health Spending Over Time",
		x = "Year", y = "External Share of CHE (5)",
		color = "Country"
	)


#Write and use a function that does something with the data
new_mean <- function(x) {
	n <- length(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

haiti_row <- health_spending_wide |>
	filter(country_name %in% c("Haiti")
	) #created dataset with just haiti rows

guyana_row <- health_spending_wide |>
	filter(country_name %in% c("Guyana")
	) ##created dataset with just guyana rows

ext_haiti <- c(haiti_row$ext_che)
ext_guyana <- c(guyana_row$ext_che)

new_mean( x = c(ext_haiti))
new_mean( x = c(ext_guyana))


