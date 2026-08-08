# Generates completion candidates from an already resolved context.
completion_candidates <- function(context, accessor, partial_name) {
    if (is.null(accessor)) {
        children <- unlist(lapply(
            context,
            ls,
            all.names = TRUE,
            sorted = FALSE
        ), use.names = FALSE)
    } else if (accessor == "::") {
        children <- getNamespaceExports(context)
    } else if (accessor == ":::") {
        children <- ls(context, all.names = TRUE, sorted = FALSE)
    } else if (accessor == "@") {
        children <- methods::slotNames(context)
    } else if (is.environment(context)) {
        children <- if (accessor == "[" && !is.object(context)) {
            NULL
        } else {
            ls(context, all.names = TRUE, sorted = FALSE)
        }
    } else if (
        accessor == "$" &&
        !is.recursive(context) &&
        !is.object(context)
    ) {
        children <- NULL
    } else if (accessor %in% c("$", "[", "[[")) {
        children <- attr(context, "names", exact = TRUE)
        if (is.null(children) && accessor %in% c("[", "[[")) {
            children <- unlist(
                attr(context, "dimnames", exact = TRUE),
                use.names = FALSE
            )
        }
    } else {
        stop("Unsupported completion accessor: ", accessor)
    }

    if (is.null(children)) {
        return(character())
    }
    children <- unique(children[!is.na(children) & nzchar(children)])
    children[startsWith(children, partial_name)]
}
