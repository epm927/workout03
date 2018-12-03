# The `Roller` Package

** Please Note:** I had some serious issues with submission... I ended up initializing mutiple git directories inside each other and so I had to resort to the following:

* This file is the `workout03` Tutorial file
* All files other than this `README` belong to the foler `roller`

### Tutorial:

---
title: "Workout3"
author: "Ethan M."
date: "December 2, 2018"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(roller)
```

### General Information

This package is designed to simulate rolling a specificed `device()` a specified number of times. 

\ 

### Device:

Users are able to specify any type of device, so long as the number of sides is greater than one and the probabilities obey the basic axioms of probability (i.e. are nonnegative, between 0 and 1, and sum to 1). The default is a fair "coin" with two sides ("1" and "2") of equal probability:

```{r}
fair_coin <- device()
fair_coin
```

You can also specify nonstandard devices, of any length:

```{r}
tri_die <- device(c("1", "2", "3"), prob = c(1/3, 1/3, 1/3))
tri_die
```

\ 

### Rolls:

Users are also able to simulate rolling a device using the `roll()` function. The function takes an object of class "device" and rolls it a specified number of times (`times` must be a positive integer).

```{r}
roll(fair_coin, 2)
roll(tri_die, 100)
```

\ 

### Plotting:

This package comes with a default `plot()` function, which produces a frequency barchart of the rolls of a device. 

```{r}
die <- device(sides = 1:6, prob = rep(1/6, 6))
rolls100 <- roll(die, 100)
plot(rolls100)
```
