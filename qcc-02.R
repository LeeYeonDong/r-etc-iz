# Generate example dataset
set.seed(123) # for reproducibility
treatment <- rep(c("A", "B"), each = 40)
gender <- rep(c("male", "female"), times = 40)
blood_pressure <- rnorm(80, mean = 120, sd = 10)
df <- data.frame(treatment, gender, blood_pressure)

# Perform two-way ANOVA
model <- aov(blood_pressure ~ treatment + gender, data = df) # 교호작용 "X"
summary(model) # view ANOVA table

model <- aov(blood_pressure ~ treatment + gender + treatment*gender, data = df) # 교호작용 "o"
summary(model) # view ANOVA table