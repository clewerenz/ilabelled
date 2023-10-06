test_that(
  "labelled data to factor", {

    # Default behavior

    ## factor to i_labelled and back to factor: will levels stay equal
    old_lvl <- levels(iris$Species)
    new_lvl <- levels(i_as_factor(i_labelled(iris$Species)))
    expect_equal(old_lvl, new_lvl)

    ## no value labels applied: values become labels (class i_labelled)
    x <- i_as_factor(i_labelled(c(1:3,NA)))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("1","2","3"))
    expect_equal(as.numeric(x), c(1:3,NA))

    x <- i_as_factor(i_labelled(c(124,256,1024,NA,124)))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("124","256","1024"))
    expect_equal(as.numeric(x), c(1,2,3,NA,1))

    x <- i_as_factor(i_labelled(c(124,256,1024,NA,-9,256)))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("-9","124","256","1024"))
    expect_equal(as.numeric(x), c(2,3,4,NA,1,3))

    x <- i_as_factor(i_labelled(c("A","B","C",NA)))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("A","B","C"))
    expect_equal(as.numeric(x), c(1:3,NA))

    x <- i_as_factor(i_labelled(c(T,F,F,T,NA)))
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("FALSE", "TRUE"))
    expect_equal(as.numeric(x), c(2,1,1,2,NA))

    ## no value labels applied: values become labels (base classes)
    x <- c(1:3,NA)
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("1","2","3"))
    expect_equal(as.numeric(x), c(1,2,3,NA))

    x <- c(NA,LETTERS[1:3])
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("A","B","C"))
    expect_equal(as.numeric(x), c(NA,1,2,3))

    x <- c(T,F,F,NA)
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("FALSE","TRUE"))
    expect_equal(as.numeric(x), c(2,1,1,NA))

    ## all value labels applied
    x <- i_labelled(c(1:3,-9,NA), labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("X","A","B","C"))
    expect_equal(as.numeric(x), c(2:4,1,NA))

    x <- i_labelled(c(LETTERS[1:3],"X",NA), labels = c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "X"))
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("Eins","Zwei","Drei","Null"))
    expect_equal(as.numeric(x), c(1:4,NA))

    x <- c(1:3,-9,NA)
    attr(x, "labels") <- c(A = 1, B = 2, C = 3, X = -9)
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("X","A","B","C"))
    expect_equal(as.numeric(x), c(2:4,1,NA))

    x <- c(LETTERS[1:3],"X",NA)
    attr(x, "labels") <- c("Eins" = "A", "Zwei" = "B", "Drei" = "C", "Null" = "X")
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("Eins","Zwei","Drei","Null"))
    expect_equal(as.numeric(x), c(1:4,NA))

    ### as factor when not all values are labelled: values become labels for missing labels; applied labels become levels, too
    x <- c(NA,1:3,-9)
    attr(x, "labels") <- c(A = 123, B = 345, C = 567, X = 789)
    x <- i_as_factor(x)
    expect_equal(class(x), "factor")
    expect_equal(levels(x), c("-9","1","2","3","A","B","C","X"))
    expect_equal(as.numeric(x), c(NA,2:4,1))

    x <- i_labelled(c(LETTERS[1:3],NA,"X"), labels = c("YYY" = "K", "ZZZ" = "L"))
    x <- i_as_factor(x)
    expect_true(is.factor(x))
    expect_equal(levels(x), c("A","B","C","YYY","ZZZ","X"))
    expect_equal(as.numeric(x), c(1,2,3,NA,6))

    ##################################################

    # Error handling

    ## duplicate labels in value labels: no duplicate labels allowed
    x <- c(LETTERS[1:3], "X")
    attr(x, "labels") <- c(A = 123, B = 345, C = 567, X = 789)
    expect_error(i_as_factor(x))

    ## duplicate values in value labels: no duplicate values allowed
    x <- i_labelled(c(1:3))
    attr(x, "labels") <- c(A = 1, B = 2, C = 3, X = 1)
    expect_error(i_as_factor(x))

    ## NA in value labels no NAs in value labels allowed
    x <- i_labelled(c(1:3))
    attr(x, "labels") <- c(A = 1, B = NA, C = 3, X = 1)
    expect_error(i_as_factor(x))


    ##################################################

    # Testing parameters

    ## 'require_all_labels' - break when labels are missing

    x <- i_labelled(1:3)
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(LETTERS[1:3])
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(1:3, labels = c(A = 1, B = 2))
    expect_error(i_as_factor(require_all_labels = T))

    x <- i_labelled(LETTERS[1:3], labels = c("Eins" = "A", "Drei" = "C"))
    expect_error(i_as_factor(require_all_labels = T))

    ## 'missing_to_na' - as missing descaled values will be set NA

    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = T)
    expect_true(is.na(x[[4]]))
    expect_equal(levels(x), c("X", "A", "B", "C"))

    ## 'remove_missing_labels' - remove label for as missing declared values - will not become factor levels

    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = T, remove_missing_labels = T)
    expect_true(is.na(x[[4]]))
    expect_equal(levels(x), c("A", "B", "C"))

    ### remove_missing_labels = T when missing_to_na = F
    x <- i_labelled(c(1,2,3,-9), na_values = -9, labels = c(A = 1, B = 2, C = 3, X = -9))
    x <- i_as_factor(x, missing_to_na = F, remove_missing_labels = T)
    expect_false(is.na(x[[4]]))
    expect_equal(levels(x), c("-9","A", "B", "C"))

    ## 'only_labelled' - convert only to factor when valid labels
    x <- c(1:3,-9)
    attr(x, "labels") <- c(A = 1, B = 2, C = 3, X = -9)
    x <- i_as_factor(x, only_labelled = T)
    expect_true(is.factor(x))
    expect_false(is.numeric(x))
    ## tbc

    ## keep/remove attributes
    x <- i_labelled(c(1:3,-9), labels = c(A = 1, B = 2, C = 3, X = -9), label = "Test1")
    attr(x, "Test2") <- "Test2"
    ### remove attributes
    expect_no_error(i_as_factor(x, keep_attributes = F))
    y <- i_as_factor(x, keep_attributes = F)
    expect_true(length(attributes(y)) == 2)
    expect_true(all(c("class","levels") %in% names(attributes(y))))
    expect_null(attr(y, "Test2"))
    ### keep attributes
    expect_no_error(i_as_factor(x, keep_attributes = T))
    y <- i_as_factor(x, keep_attributes = T)
    expect_true(length(attributes(y)) == 5)
    expect_true(all(c("class","levels","labels","label","Test2") %in% names(attributes(y))))


  }
)

