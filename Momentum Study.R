# Set up the plot
plot(GraphData$dates, Graphportfolios$Loser, type = "l", col = "red", ylim = c(0.4, 10), xlab = "Date", ylab = "Dollar Value of Investment", main = "Cumulative Gains from Investment")
lines(GraphData$dates, Graphportfolios$Middle, type = "l", col = "orange")
lines(GraphData$dates, Graphportfolios$Winner, type = "l", col = "lime green")
lines(GraphData$dates, Graphportfolios$WML, type = "l", col = "dodgerblue3")
lines(GraphData$dates, Graphportfolios$COMBO, type = "l", col = "magenta")
lines(GraphData$dates, Graphportfolios$Market, type = "l", col = "black")
legend("topleft", c("Loser", "Middle", "Winner", "WML", "COMBO", "Market"), lty = 1, col = c("red", "orange", "lime green", "dodgerblue3", "magenta", "black"))

# Customize the axes
axis(2, at = seq(1, 10, by = 1), ylim = c(0.4, 10))

# Add horizontal grid lines starting at y = 1
grid(lty = "dotted")
abline(h = 1, col = "gray")