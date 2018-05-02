context("test as.weight()")

test_that("check_font is working", {
    
    txt <- "ABC abc AAA aaa BBB bbb CC cc"
    toks <- tokens(txt)
    mt <- dfm(txt, tolower = FALSE)
    
    expect_identical(as.weights(c("a*" = 1, "ab*" = 2), mt),
                     c(ABC = 2, abc = 2, AAA = 2, aaa = 2))
    expect_identical(as.weights(c("a*" = 1, "ab*" = 2), mt, case_insensitive = FALSE),
                     c(abc = 2, aaa = 2))
    
    expect_identical(as.weights(c("a*" = 1, "ab*" = 2), mt, nomatch_weight = 0),
                     c(ABC = 2, abc = 2, AAA = 2, aaa = 2, BBB = 0, bbb = 0, CC = 0, cc = 0))
    expect_identical(as.weights(c("a*" = 1, "ab*" = 2), mt, case_insensitive = FALSE, 
                                nomatch_weight = 0),
                     c(ABC = 0, abc = 2, AAA = 0, aaa = 2, BBB = 0, bbb = 0, CC = 0, cc = 0))
    
    expect_error(as.weights(c("a"), mt), "x must be a named vector")
    expect_error(as.weights(c("a*" = 2), toks), "y must be a dfm")
    
    
    as.weights(c("a*" = 1, "ab*" = 2), mt)
    as.weights(c("ab*" = 2, "a*" = 1), mt)
    
})

