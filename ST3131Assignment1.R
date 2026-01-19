# ==========================================================
# Part I: Exploratory Data Analysis (EDA)
# ==========================================================

# 1. Load dataset -------------------------------------------------------------
setwd("C:/Users/Okaggar/OneDrive/DSA3131/Datasets")
hdb <- read.csv("hdb-resale-Jan-Jun2021.csv", stringsAsFactors = FALSE)

# 2. Feature Engineering ------------------------------------------------------
hdb$storey_range_num <- sapply(strsplit(gsub(" TO ", "-", hdb$storey_range), "-"),
                               function(x) mean(as.numeric(x)))
hdb$town <- trimws(as.character(hdb$town))

CCR <- c("BUKIT TIMAH", "CENTRAL AREA")
RCR <- c("BISHAN", "BUKIT MERAH", "KALLANG/WHAMPOA",
         "QUEENSTOWN", "TOA PAYOH", "GEYLANG", "CLEMENTI")
OCR <- c("ANG MO KIO", "BEDOK", "BUKIT BATOK", "BUKIT PANJANG",
         "CHOA CHU KANG", "HOUGANG", "JURONG EAST", "JURONG WEST",
         "PASIR RIS", "PUNGGOL", "SEMBAWANG", "SENGKANG",
         "SERANGOON", "TAMPINES", "WOODLANDS", "YISHUN")

hdb$region <- ifelse(hdb$town %in% CCR, "CCR",
                     ifelse(hdb$town %in% RCR, "RCR", "OCR"))

hdb[sapply(hdb, is.character)] <- lapply(hdb[sapply(hdb, is.character)], as.factor)
hdb <- subset(hdb, select = -c(town, storey_range, block, street_name))

# 3. Compute continuous lease variable ----------------------------------------
hdb$remaining_lease_years <- as.numeric(gsub("^(\\d+).*", "\\1", hdb$remaining_lease))
hdb$remaining_lease_months <- as.numeric(gsub(".*?(\\d+) month.*", "\\1", hdb$remaining_lease))
hdb$remaining_lease_months[is.na(hdb$remaining_lease_months)] <- 0
hdb$remaining_lease_total <- hdb$remaining_lease_years + hdb$remaining_lease_months / 12

hdb <- subset(hdb, select = -c(remaining_lease, remaining_lease_years, remaining_lease_months))

# 4. Data structure and missingness check -------------------------------------
str(hdb)
colSums(is.na(hdb))

# 5. Transform response variable (log) ---------------------------------------
hdb$log_price <- log(hdb$resale_price)

# 6. Visualize response distribution -----------------------------------------
par(mfrow = c(1, 2))
hist(hdb$resale_price, main = "Distribution of HDB Resale Prices",
     xlab = "Resale Price (SGD)", col = "lightgreen", border = "white", breaks = 40, freq = FALSE)
lines(density(hdb$resale_price, na.rm = TRUE), col = "darkgreen", lwd = 2)

hist(hdb$log_price, main = "Distribution of Log-Transformed Prices",
     xlab = "log(Resale Price)", col = "lightblue", border = "white", breaks = 40, freq = FALSE)
lines(density(hdb$log_price, na.rm = TRUE), col = "darkblue", lwd = 2)

# 7. Explore numerical vs response --------------------------------------------
par(mfrow = c(2, 2))
plot(hdb$floor_area_sqm, hdb$log_price,
     main = "log(Resale Price) vs floor_area_sqm",
     xlab = "floor_area_sqm", ylab = "log(Resale Price)",
     col = "darkorange", pch = 19, cex = 0.6)
abline(lm(hdb$log_price ~ hdb$floor_area_sqm), col = "red", lwd = 2)

plot(hdb$lease_commence_date, hdb$log_price,
     main = "log(Resale Price) vs lease_commence_date",
     xlab = "lease_commence_date", ylab = "log(Resale Price)",
     col = "steelblue", pch = 19, cex = 0.6)
abline(lm(hdb$log_price ~ hdb$lease_commence_date), col = "red", lwd = 2)

plot(hdb$storey_range_num, hdb$log_price,
     main = "log(Resale Price) vs storey_range_num",
     xlab = "storey_range_num", ylab = "log(Resale Price)",
     col = "seagreen", pch = 19, cex = 0.6)
abline(lm(hdb$log_price ~ hdb$storey_range_num), col = "red", lwd = 2)

plot(hdb$remaining_lease_total, hdb$log_price,
     main = "log(Resale Price) vs remaining_lease_total",
     xlab = "remaining_lease_total", ylab = "log(Resale Price)",
     col = "purple", pch = 19, cex = 0.6)
abline(lm(hdb$log_price ~ hdb$remaining_lease_total), col = "red", lwd = 2)

# 8. Explore categorical vs response ------------------------------------------
boxplot(hdb$log_price ~ hdb$month,
        main = "log(Resale Price) by month",
        xlab = "month", ylab = "log(Resale Price)",
        col = "skyblue", las = 2, cex.axis = 0.8)

boxplot(hdb$log_price ~ hdb$flat_type,
        main = "log(Resale Price) by flat_type",
        xlab = "flat_type", ylab = "log(Resale Price)",
        col = "lightpink", las = 2, cex.axis = 0.8)

boxplot(hdb$log_price ~ hdb$flat_model,
        main = "log(Resale Price) by flat_model",
        xlab = "flat_model", ylab = "log(Resale Price)",
        col = "lightgreen", las = 2, cex.axis = 0.8)

boxplot(hdb$log_price ~ hdb$region,
        main = "log(Resale Price) by region",
        xlab = "region", ylab = "log(Resale Price)",
        col = "khaki", las = 2, cex.axis = 0.8)


# ==========================================================
# Part II: Model Building, Adequacy Checking, and Refinement
# ==========================================================

# 1. Initial Model (M0) ------------------------------------------------------
M0 <- lm(log_price ~ month + flat_type + floor_area_sqm + flat_model +
           lease_commence_date + storey_range_num + region + remaining_lease_total,
         data = hdb)

summary(M0)    
anova(M0)        

# 2. Model Adequacy Checks ----------------------------------------------------
qqnorm(rstandard(M0), main = "QQ plot of SR (M0)")
qqline(rstandard(M0), col = "red")

plot(fitted(M0), rstandard(M0), main="SResiduals vs Fitted (Model 0)", xlab="Fitted", ylab="SR")
abline(h=0, col="red")

hist(rstandard(M0), breaks = 40, col = "lightblue",
     main = "Histogram of Standardized Residuals (M0)",
     xlab = "Standardized Residuals")
library(lmtest)
library(car)

B <- boxplot(rstandard(M0), main = "Standardized Residuals (M0)")
length(B$out)    # Number of possible outliers
cooksD <- cooks.distance(M0)
plot(cooksD, main="Cook's Distance", ylab="Influence")
abline(h = 4/(nrow(hdb)-length(coef(M0))), col="red")

# 3. Multicollinearity Check --------------------------------------------------
alias(M0)
hdb <- droplevels(subset(hdb, flat_model != "Multi Generation"))
M0 <- lm(log_price ~ month + flat_type + floor_area_sqm + flat_model +
           lease_commence_date + storey_range_num + region + remaining_lease_total,
         data = hdb)
vif_vals <- vif(M0)
vif_vals

# 4. Model Selection (redundant variable removal) -----------------------------
library(MASS)
M1<-step(M0, direction = c("backward"))
summary(M1)

AIC(M0, M1)
summary(M0)$adj.r.squared
summary(M1)$adj.r.squared


# 5. Transformation / Remedies if model is inadequate -------------------
library(car)
library(nlme)

# ----- (a) Box-Cox Transformation -------------------------------------------.
M1_1 <- lm(resale_price ~ month + flat_type + floor_area_sqm + flat_model +
           lease_commence_date + storey_range_num + region,
         data = hdb)
boxcox(M1_1, lambda = seq(-2, 2, 0.1))

# ----- (b) Predictor Transformations ----------------------------------------

hdb$log_floor_area <- log(hdb$floor_area_sqm)
hdb$log_lease <- log(hdb$lease_commence_date)

M2 <- lm(log_price ~ month + flat_type + log_floor_area + flat_model + storey_range_num + region + log_lease,
         data = hdb)

summary(M2)

wts <- 1 / fitted(M2)^2
M3 <- lm(log_price ~ month + flat_type + log_floor_area + flat_model + storey_range_num + region + log_lease,
         data = hdb, weights = wts)

summary(M3)

hdb$fitted_M2 <- fitted(M2)
M4 <- gls(log_price ~ month + flat_type + log_floor_area + flat_model +
            storey_range_num + region + log_lease,
          weights = varPower(form = ~ fitted_M2),
          data = hdb)

AIC(M2, M3, M4)

plot(fitted(M2), rstandard(M2), main="SResiduals vs Fitted (Model 2)", xlab="Fitted", ylab="SR")
abline(h=0, col="red")
plot(fitted(M3), rstandard(M3), main="SResiduals vs Fitted (Model 3)", xlab="Fitted", ylab="SR")
abline(h=0, col="red")

# 6. Ridge regression to check for severe multicollinearity -------------
library(MASS)
ridge_mod <- lm.ridge(M3,data = hdb, lambda = seq(0, 10, 0.1))
select(ridge_mod) 
plot(ridge_mod)

# 7. Final model summary ------------------------------------------------------
summary(M3)  
anova(M3)