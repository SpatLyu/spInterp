data(TempBrazil)
colnames(TempBrazil) <- c("lon", "lat", "temp")
df = TempBrazil

# range <- c(70, 140, 15, 55)
range <- c(-78, -34, -36, 5)
p = anusplin_params(df, "TempBrazil", "dem.txt", range, alt = NULL)
str(p)

