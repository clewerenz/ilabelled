test_that(
  "Helper functions", {

    # .i_find_in - helper function for missings to na function (faster %in%)
    expect_equal(.i_find_in(as.numeric(1:5), as.numeric(4:5)), c(F,F,F,T,T))
    expect_equal(.i_find_in(as.character(LETTERS[1:5]), as.character(c("A", "B"))), c(T,T,F,F,F))
    expect_equal(.i_find_in(c(T,F,F,F,T), c(T)), c(T,F,F,F,T))
    expect_equal(.i_find_in(c(T,F,F,F,T), c(F)), c(F,T,T,T,F))
    expect_equal(.i_find_in(as.character(LETTERS[1:5]), as.numeric(4:5)), c(F,F,F,F,F))

  }
)
