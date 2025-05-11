#Hosting dashboard script 
#Create QR code
library(qrcode)
library(rsconnect)
library(paletteer)

rsconnect::deployApp("C:/Users/nali/Documents/PhD/PROJECTS/Chapter_1_Review_Intergenerational transmission of poverty/review_poverty/dashboard_app", forceUpdate = TRUE)


plot(qrcode::qr_code("https://brainandpoverty.shinyapps.io/dashboard_app/"))


#Save as png
png("qr_code_dashboard.png", width = 400, height = 400)

plot(qrcode::qr_code("https://brainandpoverty.shinyapps.io/dashboard_app/"))

dev.off()

rsconnect::showLogs(appName = "dashboard_app")



# Load the palette
palette_colors <- paletteer::paletteer_d("rcartocolor::Antique")

# View the color codes
print(palette_colors)
