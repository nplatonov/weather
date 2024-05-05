n <- 100
gr1 <- sample(c("August","September"),n,replace=TRUE) |> factor(ordered=TRUE)
gr2 <- sample(c("Low","Medium","High"),n,replace=TRUE) |> factor(ordered=TRUE)
gr3 <- sample(c("Popov","Sterlegov"),n,replace=TRUE) |> factor(ordered=TRUE)
da <- data.frame(gr1=gr1,gr2=gr2,gr3=gr3)

# Convert gr3 to a numeric variable based on its factor levels
da$gr3_numeric <- as.numeric(as.character(da$gr3))

# Load ggplot2
require(ggplot2)

# Create the plot with horizontal jitter by gr3
p <- ggplot(da, aes(gr1, gr2, color = gr3)) +
  geom_jitter(width = 0.5, height = 0, size = 2) + # Adjust width for horizontal jitter
  labs(title = "Jittered Points by gr3 Horizontally", x = "gr1", y = "gr2")

print(p)
