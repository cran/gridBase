
# Get the location/size of the current viewport in inches on the device
currentViewportLoc <- function() {
  # Get the current viewport
  cvp <- current.viewport()
  # Get the current viewport transformation
  # This transforms from inches in the current viewport to
  # inches on the device
  transform <- viewport.transform(cvp)
  # Convert the current viewport's location to inches ...
  width <- grid.convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
  height <- grid.convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE)
  # ... then to inches on the device
  bottomleft <- c(0, 0, 1) %*% transform
  left <- bottomleft[1]/bottomleft[3]
  bottom <- bottomleft[2]/bottomleft[3]
  topright <- c(width, height, 1) %*% transform
  right <- topright[1]/topright[3]
  top <- topright[2]/topright[3]
  list(left=left, bottom=bottom, right=right, top=top)
}

# Prolific use of zapsmall(..., digits=3) because lots of the time
# calculated values will be intended to be the same, but will
# differ by tiny amounts due to rounding error.  In particular,
# we are keen to avoid getting tiny negative values for par()
# settings.  digits=3 because with the values we are dealing with
# two decimal places is plenty of precision.

# Check that a viewport location is within base outer margin setting
badOMI <- function(cvp, omi, din) {
  zapsmall(cvp$left - omi[2], digits=3) < 0 ||
  zapsmall(cvp$bottom - omi[1], digits=3) < 0 ||
  zapsmall(cvp$right - (din[1] - omi[4]), digits=3) > 0 ||
  zapsmall(cvp$top - (din[2] - omi[3]), digits=3) > 0
}

# Check that a viewport location is within base figure region setting
badFIG <- function(cvp, fig, omi, din) {
  width <- din[1] - omi[2] - omi[4]
  height <- din[2] - omi[1] - omi[3]
  zapsmall(cvp$left - (omi[2] + fig[1]*width), digits=3) < 0 ||
  zapsmall(cvp$bottom - (omi[1] + fig[3]*height), digits=3) < 0||
  zapsmall(cvp$right - (omi[2] + fig[2]*width), digits=3) > 0 ||
  zapsmall(cvp$top - (omi[1] + fig[4]*height), digits=3) > 0
}

# Return par(omi) settings that correspond to the current
# grid viewport
gridOMI <- function() {
  # First get the current viewport locn/size
  cvp <- currentViewportLoc()
  # return outer margin values
  din <- par("din")
  # Do a zapsmall to avoid rounding error
  omi <- zapsmall(c(cvp$bottom, cvp$left,
                    din[2] - cvp$top, din[1] - cvp$right), digits=3)
  omi
}

# Return par(fig) settings that correspond to the current
# grid viewport
gridFIG <- function() {
  # First get the current viewport locn/size
  cvp <- currentViewportLoc()
  # Get the current base outer margins (in inches)
  din <- par("din")
  omi <- par("omi")
  # Throw an error if the curent viewport is outside the
  # current outer margins (implies fig values outside [0, 1] range)
  if (badOMI(cvp, omi, din))
    stop("Outer margins too large and/or viewport too large")
  # par(fig) is proportions within the inner region
  # par(fig) is c(x1, x2, y1, y2)
  width <- din[1] - omi[2] - omi[4]
  height <- din[2] - omi[1] - omi[3]
  # Do a zapsmall to avoid rounding error
  fig <- zapsmall(c((cvp$left - omi[1])/width,
                    (cvp$right - omi[1])/width,
                    (cvp$bottom - omi[2])/height,
                    (cvp$top - omi[2])/height), digits=3)
  fig
}

# Return par(plt) settings that correspond to the current
# grid viewport
gridPLT <- function() {
  # First get the current viewport locn/size
  cvp <- currentViewportLoc()
  # Get the current base outer margins (in inches)
  # and the current base figure region (as a proportion of the inner region)
  din <- par("din")
  omi <- par("omi")
  fig <- par("fig")
  # Throw an error if the curent viewport is outside the
  # current figure region (implies plt values outside [0, 1] range)
  if (badFIG(cvp, fig, omi, din))
    stop("Figure region too small and/or viewport too large")
  # par(plt) is proportions within the figure region
  # par(plt) is c(x1, x2, y1, y2)
  innerwidth <- din[1] - omi[2] - omi[4]
  innerheight <- din[2] - omi[1] - omi[3]
  width <- innerwidth*(fig[2] - fig[1])
  height <- innerheight*(fig[4] - fig[3])
  left <- omi[2] + innerwidth*fig[1]
  bottom <- omi[1] + innerheight*fig[3]
  # Do a zapsmall to avoid rounding error
  plt <- zapsmall(c((cvp$left - left)/width,
                    (cvp$right - left)/width,
                    (cvp$bottom - bottom)/height,
                    (cvp$top - bottom)/height), digits=3)
  plt
}

# Return some gpar settings that can be used to set par() graphical
# parameters
gridPAR <- function() {
  # FIXME:  This should be a simple get.gpar() call
  #         And will be once r-devel-grid is merged with r-devel
  # FIXME:  Need to add font specifications too
  gpars <- list(col=grid:::get.gpar("col"),
                lwd=grid:::get.gpar("lwd"),
                lty=grid:::get.gpar("lty"))
}
