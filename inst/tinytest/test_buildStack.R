
# test basic structure
stack <- .vsc.buildStack()
expect_identical(class(stack), "list")
expect_identical(names(stack), c("frames", "varLists"))
