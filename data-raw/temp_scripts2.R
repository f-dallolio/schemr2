call_modify <- function (.call, ..., .homonyms = c("keep", "first", 
    "last", "error"), .standardise = NULL, .env = caller_env()) 
{
    rlang:::call_modify(.call = .call, .homonyms = .homonyms, 
        .standardise = .standardise, .env = .env)
}

