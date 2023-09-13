# ============================= #
# ========   METRICS   ======== #
# ============================= #

word_neighbors = function(df, n_neighbors, df_row, count_stopwords = T) {
  # This function returns a data frame with several rows. Given a data frame df,
  # of the type obtained through get_music_df(), and a certain number of neighbors
  # n_neighbors, it returns the rows which are less than (or equal to) n_neighbors
  # apart from df_row, in terms of number of words.
  # A new column with the distance to the central note (which is df_row) is added.
  # count_stopwords tells whether we count n_neighbors words (whatever they may be),
  # or if we count n_neighbors which are not stopwords
  n = nrow(df)
  
  if ((df_row > n) | (df_row < 1)) {
    stop("df_row index out of range")
  }
  
  word = df$stem[df_row]
  word_before = word
  word_after = word
  count_before = 0
  count_after = 0
  
  i = df_row - 1
  while ((count_before <= n_neighbors) & (i >= 1)) {
    if (df$stem[i] == word_before) {
      i = i - 1
      next
    }
    else if (df$stem[i] == "") {
      i = i - 1
      next
    }
    else {
      word_before = df$stem[i]
      i = i - 1
      if (count_stopwords) {
        count_before = count_before + 1
        next
      }
      else {
        if (word_before %in% stopwords) {
          next
        }
        count_before = count_before + 1
      }
    }
  }
  
  i = 1 * (i == 0) + (i + 2) * (i > 0)
  
  j = df_row + 1
  while ((count_after <= n_neighbors) & (j <= n)) {
    if (df$stem[j] == word_after) {
      j = j + 1
      next
    }
    else if (df$stem[j] == "") {
      j = j + 1
      next
    }
    else {
      word_after = df$stem[j]
      j = j + 1
      if (count_stopwords) {
        count_after = count_after + 1
        next
      }
      else {
        if (word_after %in% stopwords) {
          next
        }
        count_after = count_after + 1
      }
    }
  }
  
  j = n * (j == n + 1) + (j - 2) * (j < n + 1)
  
  df_new = na.omit(df[i:j, ])
  df_new$distance = numeric(nrow(df_new))
  which_row = which(rownames(df_new) == as.character(df_row))[1]
  for (r in 1:nrow(df_new)) {
    df_new$distance[r] = r - which_row
  }
  df_new
}

word_asymmetric_neighbors = function(df,
                                     n_before,
                                     n_after,
                                     df_row,
                                     count_stopwords = T) {
  # This function returns a data frame with several rows. Given a data frame df,
  # of the type obtained through get_music_df(), it returns the rows which are
  # less than (or equal to) n_before words before, and less than (or equal to)
  # n_after words after apart from df_row, in terms of number of words.
  # A new column with the distance to the center note is added.
  # word_asymmetric_neighbors with n_before = n_after is the same as word_neighbors
  n = nrow(df)
  
  if ((df_row > n) | (df_row < 1)) {
    stop("df_row index out of range")
  }
  
  word = df$stem[df_row]
  word_before = word
  word_after = word
  count_before = 0
  count_after = 0
  
  i = df_row - 1
  while ((count_before <= n_before) & (i >= 1)) {
    if (df$stem[i] == word_before) {
      i = i - 1
      next
    }
    else if (df$stem[i] == "") {
      i = i - 1
      next
    }
    else {
      word_before = df$stem[i]
      i = i - 1
      if (count_stopwords) {
        count_before = count_before + 1
        next
      }
      else {
        if (word_before %in% stopwords) {
          next
        }
        count_before = count_before + 1
      }
    }
  }
  
  i = 1 * (i == 0) + (i + 2) * (i > 0)
  
  j = df_row + 1
  while ((count_after <= n_after) & (j <= n)) {
    if (df$stem[j] == word_after) {
      j = j + 1
      next
    }
    else if (df$stem[j] == "") {
      j = j + 1
      next
    }
    else {
      word_after = df$stem[j]
      j = j + 1
      if (count_stopwords) {
        count_after = count_after + 1
        next
      }
      else {
        if (word_after %in% stopwords) {
          next
        }
        count_after = count_after + 1
      }
    }
  }
  
  j = n * (j == n + 1) + (j - 2) * (j < n + 1)
  
  df_new = na.omit(df[i:j, ])
  df_new$distance = numeric(nrow(df_new))
  which_row = which(rownames(df_new) == as.character(df_row))[1]
  for (r in 1:nrow(df_new)) {
    df_new$distance[r] = r - which_row
  }
  df_new
}

note_neighbors = function(df, n_neighbors, df_row) {
  # This function returns the n_neighbors notes before and after the note
  # situated in the df_row-th row of the data frame df.
  df_new = na.omit(df[max(c(1,
                            (df_row - n_neighbors))):min(c(nrow(df),
                                                           (df_row + n_neighbors))),])
  if (nrow(df_new) == 0) {
    stop("Empty neighborhood, or only rest notes")
  }
  df_new$distance = numeric(nrow(df_new))
  which_row = which(rownames(df_new) == as.character(df_row))[1]
  for (r in 1:nrow(df_new)) {
    df_new$distance[r] = r - which_row
  }
  df_new
}

note_asymmetric_neighbors = function(df, n_before, n_after, df_row) {
  # This function returns n_before rows and n_after rows, respect to the
  # row df_row.
  df_new = df[max(c(1, (df_row - n_before))):min(c(nrow(df), (df_row + n_after))),]
  
  if (nrow(df_new) == 0) {
    stop("Empty neighborhood, or only rest notes")
  }
  df_new$distance = numeric(nrow(df_new))
  which_row = which(rownames(df_new) == as.character(df_row))[1]
  for (r in 1:nrow(df_new)) {
    df_new$distance[r] = r - which_row
  }
  df_new
}

first_note = function(df, df_row) {
  # This function returns the index of the row corresponding to the first
  # syllabus of the word corresponding to df_row.
  word = df$stem[df_row]
  if (word == "")
    stop("df_row doesn't correspond to a word")
  n = nrow(df)
  i = max(c(df_row - 1, 1))
  while (df$stem[i] == word) {
    if (i == 1) {
      return(i)
    }
    i = i - 1
  }
  i + 1
}

interv_mean = function(df,
                       n_neighbors,
                       df_row,
                       word_neighbors_bool = T,
                       count_stopwords = T) {
  # This function computes the intervallic mean in the neighborhood around the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  k = length(neighborhood$numbers) - 1
  if (k < 1)
    return(0)
  
  sum(abs(diff(neighborhood$numbers))) / k
}

interv_mean_before = function(df,
                              n_neighbors,
                              df_row,
                              word_neighbors_bool = T,
                              count_stopwords = T) {
  
  # This function computes the intervallic mean in the neighborhood before the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  k = length(neighborhood$numbers) - 1
  if (k < 1)
    return(0)
  
  sum(abs(diff(neighborhood$numbers))) / k
}

interv_median = function(df,
                         n_neighbors,
                         df_row,
                         word_neighbors_bool = T,
                         count_stopwords = T) {
  # This function computes the intervallic median in the neighborhood around the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  if (length(neighborhood$numbers) < 2)
    return(0)
  
  median(abs(diff(neighborhood$numbers)))
}

interv_median_before = function(df,
                         n_neighbors,
                         df_row,
                         word_neighbors_bool = T,
                         count_stopwords = T) {
  # This function computes the intervallic median in the neighborhood before the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  
  if (length(neighborhood$numbers) < 2)
    return(0)
  
  median(abs(diff(neighborhood$numbers)))
}

interv_range = function(df,
                        n_neighbors,
                        df_row,
                        word_neighbors_bool = T,
                        count_stopwords = T) {
  # This function computes the intervallic range in the neighborhood around the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  neigh_notes = neighborhood$numbers
  max(neigh_notes) - min(neigh_notes)
}

interv_range_before = function(df,
                        n_neighbors,
                        df_row,
                        word_neighbors_bool = T,
                        count_stopwords = T) {
  # This function computes the intervallic range in the neighborhood before the
  # word in data frame row df_row. Note of reference is the first one,
  # according to function first_note. If word_neighbors_bool = F, then neighbors
  # are just notes, according to function note_neighbors. If word_neighbors_bool
  # is set to TRUE, count_stopwords is the same as the argument passed to
  # word_neighbors
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  neigh_notes = neighborhood$numbers
  max(neigh_notes) - min(neigh_notes)
}

ordered_discrete_kernel = function(bw, x, X0 = 0) {
  # This is an implementation of the (unnormalized) Li and Racine kernel for
  # discrete, ordered variables (ordinal).
  # X0 is taken to be 0 since that is the reference note in our use.
  # bw is the bandwidth parameter
  
  if ((bw < 0) | (bw > 1)) {
    stop("bandwidth parameter must not be negative or greater than 1")
  }
  
  if (bw == 0) {
    return(as.integer(x == X0))
  }
  
  bw ^ (abs(x - X0))
}

kernel_regression_coefficients = function(x, y, deg, bw = 0.5) {
  # Calculates kernel regression E(y | x) = m(x), with a local polynomial fit
  # of degree deg, and returns the coefficients of the polynomial. Li and Racine
  # kernel for ordinal variables is used, maybe other kernels will be implemented.
  # bw is the bandwidth argument, should be between 0 and 1.
  
  ndata = length(x)
  if (ndata != length(y))
    stop("x and y lengths differ")
  
  X = matrix(rep(1, ndata), ncol = 1)
  
  for (i in 1:deg) {
    X = cbind(X, x ^ i)
  }
  
  W = diag(ordered_discrete_kernel(bw, x))
  betas = ((solve(t(X) %*% W %*% X)) %*% t(X) %*% W %*% y)[, 1]
  betas
}

local_slope = function(df,
                       n_neighbors,
                       df_row,
                       word_neighbors_bool = T,
                       count_stopwords = T,
                       bw = 0.5,
                       plot_neighborhood = F) {
  # This function returns the slope of a local linear fit around the note of
  # reference, either the first one of the corresponding word (if
  # word_neighbors_bool), or the one in df_row (if not word_neighbors_bool).
  # Local linear fit is calculated with kernel non-parametric regression.
  # bw is the bandwidth parameter
  # plot_neighborhood: whether we should plot the notes and regression line or not
  
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  X = neighborhood$distance
  y = neighborhood$numbers
  
  if (length(X) <= 1) {
    return(NA)
  }
  
  coeff = kernel_regression_coefficients(X, y, 1, bw)
  
  if (plot_neighborhood) {
    plot(
      X,
      y,
      pch = 21,
      bg = "red",
      col = "red",
      xlab = "notes",
      ylab = "numbers",
      main = "Local linear fit"
    )
    curve(
      coeff[1] + coeff[2] * x,
      lwd = 2,
      col = "slateblue",
      add = T
    )
    points(
      0,
      y[which(X == 0)],
      pch = 21,
      bg = "deeppink",
      col = "black",
      cex = 1.5
    )
  }
  coeff[2]
}

local_slope_before = function(df,
                       n_neighbors,
                       df_row,
                       word_neighbors_bool = T,
                       count_stopwords = T,
                       bw = 0.5,
                       plot_neighborhood = F) {
  # This function returns the slope of a local linear fit before the note of
  # reference, either the first one of the corresponding word (if
  # word_neighbors_bool), or the one in df_row (if not word_neighbors_bool).
  # Local linear fit is calculated with kernel non-parametric regression.
  # bw is the bandwidth parameter
  # plot_neighborhood: whether we should plot the notes and regression line or not
  
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  
  X = neighborhood$distance
  y = neighborhood$numbers
  
  if (length(X) <= 1) {
    return(NA)
  }
  
  coeff = kernel_regression_coefficients(X, y, 1, bw)
  
  if (plot_neighborhood) {
    plot(
      X,
      y,
      pch = 21,
      bg = "red",
      col = "red",
      xlab = "notes",
      ylab = "numbers",
      main = "Local linear fit"
    )
    curve(
      coeff[1] + coeff[2] * x,
      lwd = 2,
      col = "slateblue",
      add = T
    )
    points(
      0,
      y[which(X == 0)],
      pch = 21,
      bg = "deeppink",
      col = "black",
      cex = 1.5
    )
  }
  coeff[2]
}

local_squared_term = function(df,
                              n_neighbors,
                              df_row,
                              word_neighbors_bool = T,
                              count_stopwords = T,
                              bw = 0.5,
                              plot_neighborhood = F) {
  # This function returns the quadratic term of a local linear fit around the note
  # of reference, either the first one of the corresponding word (if
  # word_neighbors_bool), or the one in df_row (if not word_neighbors_bool).
  # Local quadratic fit is calculated with kernel non-parametric regression.
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  X = neighborhood$distance
  y = neighborhood$numbers
  
  if (length(X) <= 2) {
    return(NA)
  }
  
  coeff = kernel_regression_coefficients(X, y, 2, bw)
  
  if (plot_neighborhood) {
    plot(
      X,
      y,
      pch = 21,
      bg = "red",
      col = "red",
      xlab = "notes",
      ylab = "numbers",
      main = "Local linear fit"
    )
    curve(
      coeff[1] + coeff[2] * x + coeff[3] * x ^ 2,
      lwd = 2,
      col = "slateblue",
      add = T
    )
    points(
      0,
      y[which(X == 0)],
      pch = 21,
      bg = "deeppink",
      col = "black",
      cex = 1.5
    )
  }
  coeff[3]
}

local_squared_term_before = function(df,
                              n_neighbors,
                              df_row,
                              word_neighbors_bool = T,
                              count_stopwords = T,
                              bw = 0.5,
                              plot_neighborhood = F) {
  # This function returns the quadratic term of a local linear fit before the note
  # of reference, either the first one of the corresponding word (if
  # word_neighbors_bool), or the one in df_row (if not word_neighbors_bool).
  # Local quadratic fit is calculated with kernel non-parametric regression.
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  
  X = neighborhood$distance
  y = neighborhood$numbers
  
  if (length(X) <= 2) {
    return(NA)
  }
  
  coeff = kernel_regression_coefficients(X, y, 2, bw)
  
  if (plot_neighborhood) {
    plot(
      X,
      y,
      pch = 21,
      bg = "red",
      col = "red",
      xlab = "notes",
      ylab = "numbers",
      main = "Local linear fit"
    )
    curve(
      coeff[1] + coeff[2] * x + coeff[3] * x ^ 2,
      lwd = 2,
      col = "slateblue",
      add = T
    )
    points(
      0,
      y[which(X == 0)],
      pch = 21,
      bg = "deeppink",
      col = "black",
      cex = 1.5
    )
  }
  coeff[3]
}

variability = function(df,
                       n_neighbors,
                       df_row,
                       word_neighbors_bool = T,
                       count_stopwords = T,
                       deg = 1,
                       bw = 0.5) {
  # This function computes a measure of the variability around the mean linear
  # or quadratic trend of the neighborhood determined by n_neighbors, df_row and
  # word_neighbors_bool. This linear trend is calculated with non-param regression
  
  if (not(deg %in% c(1, 2))) {
    stop("deg must be equal to 1 or 2")
  }
  
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  x = neighborhood$distance
  y = neighborhood$numbers
  
  if ((length(x) <= 2) & (deg == 2)) {
    return(NA)
  }
  
  if ((length(x) <= 1) & (deg == 1)) {
    return(0)
  }
  
  coef = kernel_regression_coefficients(x, y, deg, bw)
  
  fitted_values = coef[1] + coef[2] * neighborhood$distance +
    ifelse(deg == 2, coef[3] * neighborhood$distance ^ 2, 0)
  
  variab = (sum((y - fitted_values) ^ 2)) / length(y)
  
  variab
}

variability_before = function(df,
                       n_neighbors,
                       df_row,
                       word_neighbors_bool = T,
                       count_stopwords = T,
                       deg = 1,
                       bw = 0.5) {
  # This function computes a measure of the variability around the mean linear
  # or quadratic trend of the asymmetric (before) neighborhood determined by 
  # n_neighbors, df_row and word_neighbors_bool. This linear trend is calculated 
  # with non-param regression
  
  if (not(deg %in% c(1, 2))) {
    stop("deg must be equal to 1 or 2")
  }
  
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  
  x = neighborhood$distance
  y = neighborhood$numbers
  
  if ((length(x) <= 2) & (deg == 2)) {
    return(NA)
  }
  
  if ((length(x) <= 1) & (deg == 1)) {
    return(0)
  }
  
  coef = kernel_regression_coefficients(x, y, deg, bw)
  
  fitted_values = coef[1] + coef[2] * neighborhood$distance +
    ifelse(deg == 2, coef[3] * neighborhood$distance ^ 2, 0)
  
  variab = (sum((y - fitted_values) ^ 2)) / length(y)
  
  variab
}

variability_alternative = function(df,
                                   n_neighbors,
                                   df_row,
                                   word_neighbors_bool = T,
                                   count_stopwords = T,
                                   deg = 1,
                                   bw = 0.5) {
  # This is an alternative version of the variability function. It implements
  # the R^2 statistic (in general form) to quantify the goodness of fit
  # around the fitted kernel regression. A smaller R^2 indicates higher variability
  # and an R^2 close to 1 indicates very small variability around the trend.
  # Problem is, we always get really small values (?)
  if (not(deg %in% c(1, 2))) {
    stop("deg must be equal to 1 or 2")
  }
  
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  x = neighborhood$distance
  y = neighborhood$numbers
  
  coef = kernel_regression_coefficients(x, y, deg, bw)
  
  fitted_values = coef[1] + coef[2] * neighborhood$distance +
    ifelse(deg == 2, coef[3] * neighborhood$distance ^ 2, 0)
  
  rsq = (sum((y - mean(y)) * (fitted_values - mean(y)))) ^ 2
  rsq = rsq / sum((y - mean(y)) ^ 2)
  rsq = rsq / sum((fitted_values - mean(y)) ^ 2)
  rsq
}

monotonic_rho = function(df,
                         n_neighbors,
                         df_row,
                         word_neighbors_bool = T,
                         count_stopwords = T) {
  # This function computes a Spearman's rho statistic, which summarises the
  # increasing/decreasing behavior of the neighborhood defined by the arguments.
  # A monotonic_rho closer to 1 indicates an increasing trend, while a
  # monotonic_rho closer to -1 indicates a decreasing trend.
  
  if (word_neighbors_bool) {
    neighborhood = word_neighbors(df,
                                  n_neighbors,
                                  first_note(df, df_row),
                                  count_stopwords)
  }
  else {
    neighborhood = note_neighbors(df, n_neighbors, df_row)
  }
  
  x = neighborhood$distance
  y = neighborhood$numbers
  
  z = suppressWarnings(cor(x = y, y = x, method = "spearman"))
  if (is.na(z)) {
    return(0)
  }
  z
}

monotonic_rho_before = function(df,
                         n_neighbors,
                         df_row,
                         word_neighbors_bool = T,
                         count_stopwords = T) {
  # This function computes a Spearman's rho statistic in the asymmetric 
  # neighborhood (before), which summarises the increasing/decreasing behavior 
  # of this asymmetric neighborhood defined by the arguments.
  # A monotonic_rho closer to 1 indicates an increasing trend, while a
  # monotonic_rho closer to -1 indicates a decreasing trend.
  
  if (word_neighbors_bool) {
    neighborhood = word_asymmetric_neighbors(df,
                                             n_neighbors, 
                                             0,
                                             first_note(df, df_row),
                                             count_stopwords)
  }
  else {
    neighborhood = note_asymmetric_neighbors(df, n_neighbors, 0, df_row)
  }
  
  x = neighborhood$distance
  y = neighborhood$numbers
  
  z = suppressWarnings(cor(x = y, y = x, method = "spearman"))
  if (is.na(z)) {
    return(0)
  }
  z
}

global_metrics = function(df,
                          word_stem,
                          bw = 0.5,
                          word_neighbors_bool = T,
                          count_stopwords = T) {
  # This function gives all of the global metrics associated to word_stem, for
  # each appearance of word_stem on data frame df. The metrics are calculated
  # according to neighborhoods defined by the other arguments, for n_neighbors
  # equal to 0, 1, 2 and 3 neighbors, as well as their asymmetric (before) respectives.
  # The output type is a data frame.
  
  starting_points = which((df$stem == word_stem) &
                            (not(
                              df$syllabic %in% c("middle", "end", "none")
                            ))) # first row of each word
  
  if (length(starting_points) == 0) {
    return ("stem doesn't occur")
  }
  
  n_row = nrow(df)
  
  last_row = last(starting_points)
  
  # Something that could happen is that there is a repetition and the stem appears
  # at the end, but the word doesn't end. For example, an aria could end with
  # prez-, and then repeat all the way to the beginning where there is -zo. We
  # don't want to count the prez- as another occurrence of prezzo, that's why
  # we remove it.
  
  remove_last_row = df$syllabic[last_row] == "begin"
  
  if (remove_last_row) {
    for (i in ((last_row + 1):n_row)) {
      if (df$stem[i] == word_stem) {
        if (df$syllabic[i] == "end") {
          remove_last_row = FALSE
          break
        }
      }
    }
  }
  
  if (remove_last_row) {
    starting_points = starting_points[1:(length(starting_points) - 1)]
  }
  
  number_rows = length(starting_points)
  
  out_df = data.frame(
    "main_note_number" = numeric(number_rows),
    "main_note" = as.character(numeric(number_rows)),
    "interv_mean_0" = numeric(number_rows),
    "interv_median_0" = numeric(number_rows),
    "interv_range_0" = numeric(number_rows),
    "local_slope_0" = numeric(number_rows),
    "local_quadratic_0" = numeric(number_rows),
    "variability_0" = numeric(number_rows),
    "monotonic_rho_0" = numeric(number_rows),
    "interv_mean_1" = numeric(number_rows),
    "interv_median_1" = numeric(number_rows),
    "interv_range_1" = numeric(number_rows),
    "local_slope_1" = numeric(number_rows),
    "local_quadratic_1" = numeric(number_rows),
    "variability_1" = numeric(number_rows),
    "monotonic_rho_1" = numeric(number_rows),
    "interv_mean_bef_1" = numeric(number_rows), 
    "interv_median_bef_1" = numeric(number_rows), 
    "interv_range_bef_1" = numeric(number_rows), 
    "local_slope_bef_1" = numeric(number_rows), 
    "local_quadratic_bef_1" = numeric(number_rows), 
    "variability_bef_1" = numeric(number_rows), 
    "monotonic_rho_bef_1" = numeric(number_rows),
    "interv_mean_2" = numeric(number_rows),
    "interv_median_2" = numeric(number_rows),
    "interv_range_2" = numeric(number_rows),
    "local_slope_2" = numeric(number_rows),
    "local_quadratic_2" = numeric(number_rows),
    "variability_2" = numeric(number_rows),
    "monotonic_rho_2" = numeric(number_rows),
    "interv_mean_bef_2" = numeric(number_rows), 
    "interv_median_bef_2" = numeric(number_rows), 
    "interv_range_bef_2" = numeric(number_rows), 
    "local_slope_bef_2" = numeric(number_rows), 
    "local_quadratic_bef_2" = numeric(number_rows), 
    "variability_bef_2" = numeric(number_rows), 
    "monotonic_rho_bef_2" = numeric(number_rows),
    "interv_mean_3" = numeric(number_rows),
    "interv_median_3" = numeric(number_rows),
    "interv_range_3" = numeric(number_rows),
    "local_slope_3" = numeric(number_rows),
    "local_quadratic_3" = numeric(number_rows),
    "variability_3" = numeric(number_rows),
    "monotonic_rho_3" = numeric(number_rows),
    "interv_mean_bef_3" = numeric(number_rows), 
    "interv_median_bef_3" = numeric(number_rows), 
    row.names = paste0("Line_", starting_points)
  )
  
  for (i in 1:number_rows) {
    out_df[i, 1] = df$numbers[starting_points[i]]
    out_df[i, 2] = df$step[starting_points[i]]
    out_df[i, 3] = interv_mean(df,
                               0,
                               starting_points[i],
                               word_neighbors_bool,
                               count_stopwords)
    out_df[i, 4] = interv_median(df,
                                 0,
                                 starting_points[i],
                                 word_neighbors_bool,
                                 count_stopwords)
    out_df[i, 5] = interv_range(df,
                                0,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords)
    out_df[i, 6] = local_slope(df,
                               0,
                               starting_points[i],
                               word_neighbors_bool,
                               count_stopwords,
                               bw)
    out_df[i, 7] = local_squared_term(df,
                                      0,
                                      starting_points[i],
                                      word_neighbors_bool,
                                      count_stopwords,
                                      bw)
    out_df[i, 8] = variability(df,
                               0,
                               starting_points[i],
                               word_neighbors_bool,
                               count_stopwords,
                               deg = 1,
                               bw)
    out_df[i, 9] = monotonic_rho(df,
                                 0,
                                 starting_points[i],
                                 word_neighbors_bool,
                                 count_stopwords)
    out_df[i, 10] = interv_mean(df,
                                1,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords)
    out_df[i, 11] = interv_median(df,
                                  1,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 12] = interv_range(df,
                                 1,
                                 starting_points[i],
                                 word_neighbors_bool,
                                 count_stopwords)
    out_df[i, 13] = local_slope(df,
                                1,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                bw)
    out_df[i, 14] = local_squared_term(df,
                                       1,
                                       starting_points[i],
                                       word_neighbors_bool,
                                       count_stopwords,
                                       bw)
    out_df[i, 15] = variability(df,
                                1,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                deg = 1,
                                bw)
    out_df[i, 16] = monotonic_rho(df,
                                  1,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 17] = interv_mean_before(df, 
                                       1, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords)
    out_df[i, 18] = interv_median_before(df, 
                                       1, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords)
    out_df[i, 19] = interv_range_before(df, 
                                        1, 
                                        starting_points[i], 
                                        word_neighbors_bool, 
                                        count_stopwords)
    out_df[i, 20] = local_slope_before(df, 
                                       1, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       bw)
    out_df[i, 21] = local_squared_term_before(df, 
                                              1, 
                                              starting_points[i], 
                                              word_neighbors_bool, 
                                              count_stopwords, 
                                              bw)
    out_df[i, 22] = variability_before(df, 
                                       1, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       deg = 1, 
                                       bw)
    out_df[i, 23] = monotonic_rho_before(df, 
                                         1, 
                                         starting_points[i], 
                                         word_neighbors_bool, 
                                         count_stopwords)
    out_df[i, 24] = interv_mean(df,
                                2,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords)
    out_df[i, 25] = interv_median(df,
                                  2,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 26] = interv_range(df,
                                 2,
                                 starting_points[i],
                                 word_neighbors_bool,
                                 count_stopwords)
    out_df[i, 27] = local_slope(df,
                                2,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                bw)
    out_df[i, 28] = local_squared_term(df,
                                       2,
                                       starting_points[i],
                                       word_neighbors_bool,
                                       count_stopwords,
                                       bw)
    out_df[i, 29] = variability(df,
                                2,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                deg = 1,
                                bw)
    out_df[i, 30] = monotonic_rho(df,
                                  2,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 31] = interv_mean_before(df, 
                                       2, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords)
    out_df[i, 32] = interv_median_before(df, 
                                         2, 
                                         starting_points[i], 
                                         word_neighbors_bool, 
                                         count_stopwords)
    out_df[i, 33] = interv_range_before(df, 
                                        2, 
                                        starting_points[i], 
                                        word_neighbors_bool, 
                                        count_stopwords)
    out_df[i, 34] = local_slope_before(df, 
                                       2, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       bw)
    out_df[i, 35] = local_squared_term_before(df, 
                                              2, 
                                              starting_points[i], 
                                              word_neighbors_bool, 
                                              count_stopwords, 
                                              bw)
    out_df[i, 36] = variability_before(df, 
                                       2, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       deg = 1, 
                                       bw)
    out_df[i, 37] = monotonic_rho_before(df, 
                                         2, 
                                         starting_points[i], 
                                         word_neighbors_bool, 
                                         count_stopwords)
    out_df[i, 38] = interv_mean(df,
                                3,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords)
    out_df[i, 39] = interv_median(df,
                                  3,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 40] = interv_range(df,
                                 3,
                                 starting_points[i],
                                 word_neighbors_bool,
                                 count_stopwords)
    out_df[i, 41] = local_slope(df,
                                3,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                bw)
    out_df[i, 42] = local_squared_term(df,
                                       3,
                                       starting_points[i],
                                       word_neighbors_bool,
                                       count_stopwords,
                                       bw)
    out_df[i, 43] = variability(df,
                                3,
                                starting_points[i],
                                word_neighbors_bool,
                                count_stopwords,
                                deg = 1,
                                bw)
    out_df[i, 44] = monotonic_rho(df,
                                  3,
                                  starting_points[i],
                                  word_neighbors_bool,
                                  count_stopwords)
    out_df[i, 45] = interv_mean_before(df, 
                                       3, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords)
    out_df[i, 46] = interv_median_before(df, 
                                         3, 
                                         starting_points[i], 
                                         word_neighbors_bool, 
                                         count_stopwords)
    out_df[i, 47] = interv_range_before(df, 
                                        3, 
                                        starting_points[i], 
                                        word_neighbors_bool, 
                                        count_stopwords)
    out_df[i, 48] = local_slope_before(df, 
                                       3, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       bw)
    out_df[i, 49] = local_squared_term_before(df, 
                                              3, 
                                              starting_points[i], 
                                              word_neighbors_bool, 
                                              count_stopwords, 
                                              bw)
    out_df[i, 50] = variability_before(df, 
                                       3, 
                                       starting_points[i], 
                                       word_neighbors_bool, 
                                       count_stopwords, 
                                       deg = 1, 
                                       bw)
    out_df[i, 51] = monotonic_rho_before(df, 
                                         3, 
                                         starting_points[i], 
                                         word_neighbors_bool, 
                                         count_stopwords)
  }
  
  out_df
}

metrics_tidy_csv = function(rdatas,
                            stems,
                            bw = 0.5,
                            append = F) {
  # This function exports a .csv that contains a data frame. stems is a character
  # vector containing the stems we are interested in (use pruning), one way
  # to get stems is like words_stems_df$word[1:581], which are the stems with freq
  # >= 20. rdatas is a vector with the paths for the RDatas. bw is bandwidth for
  # non-parametric regression. append determines whether we should overwrite an
  # already existing file, or continue adding rows to it. The data frame
  # exported has identification columns like aria_id, didone_id, name, composer,
  # year, etc..., as well as many metrics extracted from neighborhoods around
  # each note.
  
  if (not(append)) {
    write_file(paste0(paste0(
      c(
        "aria_id",
        "aria_name",
        "composer",
        "year",
        "didone_id",
        "voice_type",
        "stem",
        "note_number",
        "main_note",
        "df_line",
        "interv_mean_0",
        "interv_median_0",
        "interv_range_0",
        "local_slope_0",
        "local_quadratic_0",
        "variability_0",
        "monotonic_rho_0",
        "interv_mean_1",
        "interv_median_1",
        "interv_range_1",
        "local_slope_1",
        "local_quadratic_1",
        "variability_1",
        "monotonic_rho_1",
        "interv_mean_bef_1",
        "interv_median_bef_1",
        "interv_range_bef_1",
        "local_slope_bef_1",
        "local_quadratic_bef_1",
        "variability_bef_1",
        "monotonic_rho_bef_1",
        "interv_mean_2",
        "interv_median_2",
        "interv_range_2",
        "local_slope_2",
        "local_quadratic_2",
        "variability_2",
        "monotonic_rho_2",
        "interv_mean_bef_2",
        "interv_median_bef_2",
        "interv_range_bef_2",
        "local_slope_bef_2",
        "local_quadratic_bef_2",
        "variability_bef_2",
        "monotonic_rho_bef_2",
        "interv_mean_3",
        "interv_median_3",
        "interv_range_3",
        "local_slope_3",
        "local_quadratic_3",
        "variability_3",
        "monotonic_rho_3", 
        "interv_mean_bef_3",
        "interv_median_bef_3",
        "interv_range_bef_3",
        "local_slope_bef_3",
        "local_quadratic_bef_3",
        "variability_bef_3",
        "monotonic_rho_bef_3"
      ),
      collapse = ","
    ), "\n"),
    file = "metric_table.csv")
  }
  
  for (i in 1:length(rdatas)) {
    load(rdatas[i])
    current_aria_stems = unique(aria_df$stem)
    
    for (current_stem in current_aria_stems) {
      if (not(current_stem %in% stems)) {
        next
      }
      metric_df = global_metrics(
        aria_df,
        current_stem,
        bw = bw,
        word_neighbors_bool = T,
        count_stopwords = T
      )
      
      n_appearances = nrow(metric_df)
      
      metric_df$aria_id = rep(identifier, n_appearances)
      metric_df$aria_name = rep(curr_aria_name, n_appearances)
      metric_df$composer = rep(composer, n_appearances)
      metric_df$year = rep(ifelse(curr_aria_year == "Unknown",
                                  NA,
                                  curr_aria_year),
                           n_appearances)
      metric_df$didone_id = rep(didone_id, n_appearances)
      metric_df$voice_type = rep(voice_type, n_appearances)
      metric_df$stem = rep(current_stem, n_appearances)
      metric_df$df_line = row.names(metric_df)
      
      metric_df = metric_df[, c(52:58, 1:2, 59, 3:51)]
      
      write_csv(metric_df,
                file = "metric_table.csv",
                append = T)
      rm(metric_df)
      gc()
    }
    print(paste(as.character(i), identifier))
    rm(aria_df, instruments, identifier)
  }
}

xmlmetrics_tidy_csv = function(xmls, stems, bw = 0.5) {
  # DEPRECATED
  
  # This function is similar to metrics_tidy_csv, but a source of xml files is
  # used instead. It isn't recommended because of a memory leak bug happening
  # in XML::xmlParse, but aside from that it works and is still supported.
  # In the future everything will be ported to xml2 package, which doesn't have
  # memory leaking problems, and this function will be perfectly usable with 
  # large corpora.
  
  # This function exports a .csv that contains a data frame. stems is a character
  # vector containing the stems we are interested in (use pruning), one way
  # to get stems is like words_stems_df$word[1:581], which are the stems with freq
  # >= 20. xmls is a vector containing the source .xml files. bw is bandwidth for
  # non-parametric regression. The data frame exported has columns aria_id, which
  # is a (unique) identifier for each aria and voice, stem (referring
  # to the stem being studied, said stem belongs to stems), and then columns
  # for different metrics like interv_mean, interv_range, local_slope... for
  # 0, 1, 2 and 3 neighbors.
  
  
  write_file(paste0(paste0(
    c(
      "aria_id",
      "aria_name",
      "composer",
      "year",
      "didone_id",
      "voice_type",
      "stem",
      "note_number",
      "main_note",
      "df_line",
      "interv_mean_0",
      "interv_median_0",
      "interv_range_0",
      "local_slope_0",
      "local_quadratic_0",
      "variability_0",
      "monotonic_rho_0",
      "interv_mean_1",
      "interv_median_1",
      "interv_range_1",
      "local_slope_1",
      "local_quadratic_1",
      "variability_1",
      "monotonic_rho_1",
      "interv_mean_bef_1",
      "interv_median_bef_1",
      "interv_range_bef_1",
      "local_slope_bef_1",
      "local_quadratic_bef_1",
      "variability_bef_1",
      "monotonic_rho_bef_1",
      "interv_mean_2",
      "interv_median_2",
      "interv_range_2",
      "local_slope_2",
      "local_quadratic_2",
      "variability_2",
      "monotonic_rho_2",
      "interv_mean_bef_2",
      "interv_median_bef_2",
      "interv_range_bef_2",
      "local_slope_bef_2",
      "local_quadratic_bef_2",
      "variability_bef_2",
      "monotonic_rho_bef_2",
      "interv_mean_3",
      "interv_median_3",
      "interv_range_3",
      "local_slope_3",
      "local_quadratic_3",
      "variability_3",
      "monotonic_rho_3", 
      "interv_mean_bef_3",
      "interv_median_bef_3",
      "interv_range_bef_3",
      "local_slope_bef_3",
      "local_quadratic_bef_3",
      "variability_bef_3",
      "monotonic_rho_bef_3"
    ),
    collapse = ","
  ), "\n"),
  file = "metric_table.csv")
  
  for (i in 1:length(xmls)) {
    aria = xmlParse(xmls[i])
    root_node = xmlRoot(aria)
    curr_aria_name = aria_name(root_node)
    parts = Filter(is_part, xmlChildren(root_node))
    
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    
    identification_node = xmlElementsByTagName(root_node, "identification")[[1]]
    composer = aria_composer(identification_node)
    curr_aria_year = get_year(root_node)
    didone_id = get_didone_code(xmls[i])
    
    instruments = lapply(score_part_subnodes, get_instrument)
    
    free(aria)
    rm(aria,
       part_list_subnode,
       score_part_subnodes)
    gc()
    
    for (l in which_voice(instruments)) {
      voice_part = parts[[l]]
      
      notes = xmlElementsByTagName(voice_part, "note", recursive = TRUE)
      
      attributes = xmlElementsByTagName(voice_part, "attributes", recursive = T)[[1]]
      
      time1 <<- get_time(attributes)
      
      rm(voice_part, attributes)
      
      aria_df = get_music_df(notes)
      rm(notes)
      gc()
      
      voice_type = voice_name(instruments, l)
      
      identifier = aria_identifier(root_node, voice_type, l)
      
      current_aria_stems = unique(aria_df$stem)
      
      for (current_stem in current_aria_stems) {
        if (not(current_stem %in% stems)) {
          next
        }
        metric_df = global_metrics(
          aria_df,
          current_stem,
          bw = bw,
          word_neighbors_bool = T,
          count_stopwords = T
        )
        
        n_appearances = nrow(metric_df)
        
        metric_df$aria_id = rep(identifier, n_appearances)
        metric_df$aria_name = rep(curr_aria_name, n_appearances)
        metric_df$composer = rep(composer, n_appearances)
        metric_df$year = rep(ifelse(curr_aria_year == "Unknown",
                                    NA,
                                    curr_aria_year),
                             n_appearances)
        metric_df$didone_id = rep(didone_id, n_appearances)
        metric_df$voice_type = rep(voice_type, n_appearances)
        metric_df$stem = rep(current_stem, n_appearances)
        metric_df$df_line = row.names(metric_df)
        
        metric_df = metric_df[, c(52:58, 1:2, 59, 3:51)]
        
        write_csv(metric_df,
                  file = "metric_table.csv",
                  append = T)
        rm(metric_df)
        gc()
      }
      rm(identifier, current_aria_stems)
    }
    print(paste(
      as.character(i),
      aria_name(root_node),
      "-",
      aria_composer(xmlElementsByTagName(root_node, "identification")[[1]]),
      "-",
      get_year(root_node)
    ))
    rm(root_node, instruments)
  }
}

xmlmetrics_tidy_csv_continue = function(xmls, stems, bw = 0.5) {
  # DEPRECATED
  
  # This is a continuation to the function xmlmetrics_tidy_csv. The difference is
  # that this one does not produce a new .csv file, but instead appends to an
  # already existing one. The advantage is the ability to create the complete
  # .csv files at chunks, because doing so with all the xml's may be both
  # time and computationally expensive.
  # Maybe some time in the future this function will disappear and another
  # argument will be added to xmlmetrics_tidy_csv to continue with an already
  # existing file.
  
  for (i in 1:length(xmls)) {
    aria = xmlParse(xmls[i])
    root_node = xmlRoot(aria)
    curr_aria_name = aria_name(root_node)
    parts = Filter(is_part, xmlChildren(root_node))
    
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    
    identification_node = xmlElementsByTagName(root_node, "identification")[[1]]
    composer = aria_composer(identification_node)
    curr_aria_year = get_year(root_node)
    didone_id = get_didone_code(xmls[i])
    
    instruments = lapply(score_part_subnodes, get_instrument)
    
    free(aria)
    rm(aria,
       part_list_subnode,
       score_part_subnodes)
    gc()
    
    for (l in which_voice(instruments)) {
      voice_part = parts[[l]]
      
      notes = xmlElementsByTagName(voice_part, "note", recursive = TRUE)
      
      attributes = xmlElementsByTagName(voice_part, "attributes", recursive = T)[[1]]
      
      time1 <<- get_time(attributes)
      
      rm(voice_part, attributes)
      
      aria_df = get_music_df(notes)
      rm(notes)
      gc()
      
      voice_type = voice_name(instruments, l)
      
      identifier = aria_identifier(root_node, voice_type, l)
      
      current_aria_stems = unique(aria_df$stem)
      
      for (current_stem in current_aria_stems) {
        if (not(current_stem %in% stems)) {
          next
        }
        metric_df = global_metrics(
          aria_df,
          current_stem,
          bw = bw,
          word_neighbors_bool = T,
          count_stopwords = T
        )
        
        n_appearances = nrow(metric_df)
        
        metric_df$aria_id = rep(identifier, n_appearances)
        metric_df$aria_name = rep(curr_aria_name, n_appearances)
        metric_df$composer = rep(composer, n_appearances)
        metric_df$year = rep(ifelse(curr_aria_year == "Unknown",
                                    NA,
                                    curr_aria_year),
                             n_appearances)
        metric_df$didone_id = rep(didone_id, n_appearances)
        metric_df$voice_type = rep(voice_type, n_appearances)
        metric_df$stem = rep(current_stem, n_appearances)
        metric_df$df_line = row.names(metric_df)
        
        metric_df = metric_df[, c(52:58, 1:2, 59, 3:51)]
        
        write_csv(metric_df,
                  file = "metric_table.csv",
                  append = T)
        rm(metric_df)
      }
      rm(identifier, current_aria_stems)
    }
    print(paste(
      as.character(i),
      aria_name(root_node),
      "-",
      aria_composer(xmlElementsByTagName(root_node, "identification")[[1]]),
      "-",
      get_year(root_node)
    ))
    rm(root_node, instruments)
  }
}