#Author: Josh
#Date: 1/12/2020
#Ver: 1.0
#Subject: Intro R
#Student ID: 2355145
## Make up 4 different room types (eg: standard, deluxe, etc)
## Determine a fixed daily rate (in $) and operating cost (int $) for each room type
## The hotel should have between 60-80 rooms in total (set multiples of each type)
## Simulate the occupancy of each room throughout the year 2019
## Add a 30% premium to the room price on the weekends (Saturday, Sunday)
## 
## 1. Calculate the total revenue and profit of the companey
## 2. Calculate and visualize the PnL cumulative of the company throughout the year (line chart)
## 3. Calculate and visualize the PnL contribution of each room type (pie chart)


# Create the room data
room_data <- data.frame(
   type = c("Standard", "Deluxe", "Joint", "Apartment"),
   rate = c(80, 140, 60, 30),
   cost = c(10, 15, 5, 3)
)

#print(room_data)

# Create first room for hotel
hotel.data <- data.frame(
    room_num = c(1),
    room_type = room_data$type[1],
    room_rate = room_data$rate[1],
    room_cost = room_data$cost[1]
)

#print(hotel.data)

# Add data for hotel. Hotel have 70 room
i <- 2
while (i <= 70) {
    y <- as.integer(i %% 4) + 1
    #print(y)
    hotel.newroom <- data.frame(
        room_num = c(i),
        room_type = room_data$type[y],
        room_rate = room_data$rate[y],
        room_cost = room_data$cost[y]
    )
    hotel.data <- rbind(hotel.data, hotel.newroom)
    i = i + 1
}
#print(hotel.data)

## Simulate the occupancy of each room throughout the year 2019
## Add a 30% premium to the room price on the weekends (Saturday, Sunday)
# Calculate daily rate for room type. Add 30% at weekend
room.getPriceAtDate <- function(room_type, date) {
    i <- 1
    while(room_type != room_data$type[i]) {
        i = i + 1
    }
    
    library(chron)
    if (is.weekend(date)) {
        result <-  room_data$rate[i] + room_data$rate[i] * 30 / 100
    }
    else {
        result <- room_data$rate[i]
    }
    return(result)
}

## Calculate the total revenue and profit of the companey
hotel.getRevenueByYear <- function(year) {
    result <- 0
    start <- as.Date(paste("01-01-",year),format="%d-%m-%Y")
    end   <- as.Date(paste("31-12-",year),format="%d-%m-%y")

    theDate <- start
    while (theDate <= end)
    {
        #print(theDate)
        #print(hotel.data$room_num)
        for (num in hotel.data$room_num) {
            #print(num)
            #print(theDate)
            #print(hotel.data[num,])
            #print(hotel.data[num,]$room_rate)
            result = result + room.getPriceAtDate(hotel.data[num,]$room_type, theDate)
        }
        
        theDate <- theDate + 1 
    }
    
    return(result)
}

hotel.getCostByYear <- function(year) {
    result <- 0
    start <- as.Date(paste("01-01-",year),format="%d-%m-%Y")    
    end   <- as.Date(paste("31-12-",year),format="%d-%m-%y")
    #print(start)
    #print(end)

    theDate <- start
    while (theDate <= end)
    {
        #print(theDate)
        #print(hotel.data$room_num)
        for (num in hotel.data$room_num) {
            #print(num)
            #print(theDate)
            #print(hotel.data[num,])
            #print(hotel.data[num,]$room_rate)
            result = result + hotel.data[num,]$room_cost
        }
        
        theDate <- theDate + 1 
    }
    
    return(result)
}

hotel.getRevenueByMonth <- function(month) {
    result <- 0
    #print(month)
    start <- as.Date(paste("01-",month,"-2019", sep = ""),format="%d-%m-%Y")
    if (month < 12) {
        end   <- as.Date(paste("01-", month + 1, "-2019",sep = ""),format="%d-%m-%Y")
    }
    else
    {
        end   <- as.Date(paste("01-01-2020",sep = ""),format="%d-%m-%Y")
    }
    #print(start)
    #print(end)
    theDate <- start
    while (theDate < end)
    {
        for (num in hotel.data$room_num) {
            result = result + room.getPriceAtDate(hotel.data[num,]$room_type, theDate)
        }        
        theDate <- theDate + 1 
    }
    
    return(result)
}

hotel.getCostByMonth <- function(month) {
    result <- 0
    #print(month)
    start <- as.Date(paste("01-", month, "-2019",sep = ""),format="%d-%m-%Y")
    if (month < 12) {
        end   <- as.Date(paste("01-", month + 1, "-2019",sep = ""),format="%d-%m-%Y")
    }
    else
    {
        end   <- as.Date(paste("01-01-2020",sep = ""),format="%d-%m-%Y")
    }

    theDate <- start
    while (theDate < end)
    {
        for (num in hotel.data$room_num) {
            result = result + hotel.data[num,]$room_cost
        }
        
        theDate <- theDate + 1 
    }
    
    return(result)
}

hotel.getRevenueByRoomType <- function(roomType) {
    result <- 0
    #print(roomType)
    result <- 0
    start <- as.Date(paste("01-01-2019"),format="%d-%m-%Y")
    end   <- as.Date(paste("31-12-2019"),format="%d-%m-%y")
    #print(start)
    #print(end)
    theDate <- start
    while (theDate <= end)
    {
        for (num in hotel.data$room_num) {
            if(roomType == hotel.data[num,]$room_type)
                result = result + room.getPriceAtDate(hotel.data[num,]$room_type, theDate)
        }        
        theDate <- theDate + 1 
    }
    
    return(result)
}

hotel.getCostByRoomType <- function(roomType) {
    result <- 0
    #print(month)
    start <- as.Date(paste("01-01-2019"),format="%d-%m-%Y")
    end   <- as.Date(paste("31-12-2019"),format="%d-%m-%y")
    
    theDate <- start
    while (theDate <= end)
    {
        for (num in hotel.data$room_num) {
            if(roomType == hotel.data[num,]$room_type)
                result = result + hotel.data[num,]$room_cost
        }
        
        theDate <- theDate + 1 
    }
    
    return(result)
}


revenueYear <- hotel.getRevenueByYear(2019)
costYear <- hotel.getCostByYear(2019)

## Calculate the total revenue and profit of the companey
print(paste("The revenue in 2019: $", revenueYear))
print(paste("The profit in 2019: $", revenueYear - costYear))
# Create the matrix of the values.
Values <- matrix(c(revenueYear - costYear, costYear), nrow = 2, ncol = 1, byrow = TRUE)

colors = c("green","brown")
regions <- c("Profit","Cost")

# Create the bar chart
barplot(Values, main = paste("Total profit: $", revenueYear - costYear),xlab = "2019", ylab = "$", col = colors)
# Add the legend to the chart
legend("topleft", regions, cex = 1.3, fill = colors)

## 2. Calculate and visualize the PnL cumulative of the company throughout the year (line chart)
# Give the chart file a name.
png(file = "line_chart_PnLCumulativeThroughoutYear.jpg")
# Add data to grap
PnLCumData <- c()
for( i in 1:12) {
    #print(i)
    PnLCumData[i] <- hotel.getRevenueByMonth(i) - hotel.getCostByMonth(i)
}
dev.off()
plot(PnLCumData,type = "o", col = "red", xlab = "Month", ylab = "Profit",
   main = "the PnL cumulative of the company")


## 3. Calculate and visualize the PnL contribution of each room type (pie chart)
labels <- c("Standard", "Deluxe", "Joint", "Apartment")
PnLRoomData <- c()
count <- 1
for (room in labels) {
    #print(room)
    PnLRoomData[count] <- hotel.getRevenueByRoomType(room) - hotel.getCostByRoomType(room)
    count = count + 1
}
# Plot the chart with title and rainbow color pallet.
pie(PnLRoomData, labels, main = "the PnL contribution of each room type", col = rainbow(length(PnLRoomData)))

