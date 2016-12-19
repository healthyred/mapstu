# This script is for the actual drawing of the regions by taking
# the lat and long coords

Region <- setClass(
  # set the name for the class
  "Region",
  #Creates the slots for the class
  slots = (x="coordinates", y="numofstudents", z="%change")
)

