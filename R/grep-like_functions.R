
grepi = function(...) grep(..., ignore.case=TRUE)

grepv = function(...) grep(..., value=TRUE)

grepvi = function(...) grep(..., value=TRUE, ignore.case=TRUE)

`%like%` = function(x, y) grepl(y, x, ignore.case=TRUE)

`%Like%` = function(x, y) grepl(y, x)

`%unlike%` = function(x, y) !grepl(y, x, ignore.case=TRUE)

`%Unlike%` = function(x, y) !grepl(y, x)

