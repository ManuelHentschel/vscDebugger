# Validates and normalizes the supported completion-context AST shape.

# Parse a partial child into its unescaped name and quote character.
.completion_parse_partial_child <- function(partial_child){
    # We don't handle completion of multi-line strings
    if(grepl("[\r\n]", partial_child)){
        return(NULL)
    }

    # Return unquoted partial children unchanged.
    quote <- substr(partial_child, 1L, 1L)
    if(!quote %in% c("'", "\"", "`")){
        return(list(
            name = partial_child,
            quote = NULL
        ))
    }

    # Add the closing quote before parsing escapes.
    parsed <- tryCatch(
        parse(text = paste0(partial_child, quote), keep.source = FALSE),
        error = function(error) NULL
    )
    if(is.null(parsed) || length(parsed) != 1L){
        return(NULL)
    }

    list(
        name = as.character(parsed[[1L]]),
        quote = quote
    )
}

.completion_ast_name_to_string <- function(node){
    if(is.name(node)){
        node <- as.character(node)
    }
    if(is.character(node) && length(node) == 1L && !is.na(node)){
        return(node)
    }
    stop("Expected a name or string as argument")
}

.completion_normalize_ast <- function(node){
    # Early returns
    if(is.null(node) || is.atomic(node) || is.name(node)){
        return(node)
    }

    # Check that node is a call with a named operator
    if(!is.call(node) || !is.name(node[[1L]])){
        stop("The context contains an unsupported expression")
    }

    operator <- as.character(node[[1L]])

    # Check recursively, normalize ambiguous operands to strings
    if(operator %in% c("$", "@")){
        return(as.call(list(
            as.name(operator),
            .completion_normalize_ast(node[[2L]]),
            .completion_ast_name_to_string(node[[3L]])
        )))
    }

    if(operator %in% c("::", ":::")){
        return(as.call(list(
            as.name(operator),
            .completion_ast_name_to_string(node[[2L]]),
            .completion_ast_name_to_string(node[[3L]])
        )))
    }

    if(operator %in% c("[", "[[")){
        # Normalize all operands (`[` and `[[` remain unchanged)
        return(as.call(lapply(as.list(node), .completion_normalize_ast)))
    }

    stop("Function or operator not allowed in context: ", operator)
}

.completion_parsing_result <- function(status, ast = NULL, reason = NULL){
    result <- list(status = status)
    if(status == "success"){
        result["ast"] <- list(ast)
    }
    if(!is.null(reason)){
        result$reason <- reason
    }
    result
}

parse_completion_context <- function(context){
    # Parse the context expression into an AST
    ast <- try(parse(text = context, keep.source = FALSE), silent = TRUE)

    if(inherits(ast, "try-error")){
        return(.completion_parsing_result(
            "infeasible",
            reason = "Parsing the context expression failed."
        ))
    }

    # Assert that the AST contains exactly one expression
    if(length(ast) != 1L){
        return(.completion_parsing_result(
            "infeasible",
            reason = "The context must contain exactly one expression"
        ))
    }

    # Normalize AST and check for unsupported operators
    normalized_ast <- try(.completion_normalize_ast(ast[[1L]]), silent = TRUE)

    if(inherits(normalized_ast, "try-error")){
        return(.completion_parsing_result(
            "infeasible",
            reason = conditionMessage(attr(normalized_ast, "condition"))
        ))
    }

    return(.completion_parsing_result(
        "success",
        ast = normalized_ast
    ))
}
