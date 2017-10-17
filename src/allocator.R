allocator <- function(units, preds) {
    zones <- c('zone1', 'zone2', 'zone3', 'zone4', 'zone5', 'zone6', 'zone7')
    total <- sum(preds)
    if (units <= 7) {  # if fewer units than zones
        preds <- sort(preds, decreasing = TRUE)
        alloc <- rep(0, 7)
        names(alloc) <- zones
        alloc[names(preds[1: units])] <- 1  # assign to zones with greatest demand
        return (alloc)
    }
    else {
        alloc <- rep(1, 7)  # give each zone a unit
        names(alloc) <- zones
        units <- units - 7
        additional <- floor((preds * units) / total)  # distribute units based upon need
        alloc <- alloc + additional
        units <- units - sum(additional)
        if (units > 0){  # if any units are unallocated
            sorted_alloc <- sort(alloc)  # assign remaining units to zones with fewest units
            alloc[names(sorted_alloc[1: units])] <- alloc[names(sorted_alloc[1: units])] + 1
        }
        return (alloc)
    }
}
