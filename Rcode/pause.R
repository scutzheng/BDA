pause = function(message = "Press <Enter> to continue")
{
    if (interactive())
    {
        invisible(readline(prompt = message))
    }
    else
    {
        cat(message)
        invisible(readLines(file("stdin"), 1))
    }
}
