Polarize <- function(x,y)
{
  tmp <- x
  x = y
  y = tmp
  Polar <- c(0,0)
  Polar[1] = sqrt(x^2+y^2)
  if (x>0 && y>=0)
  {
    Polar[2] = atan(y/x)
  }
  if (x<0 && y>=0)
  {
    Polar[2] = atan(y/x)+180
  }
  if (x<0 && y<0)
  {
    Polar[2] = atan(y/x)+180
  }
  if (x>0 && y<0)
  {
    Polar[2] = atan(y/x)+360
  }
  if (x==0 && y<0)
  {
    Polar[2]=270
  }
  if (x==0 && y>0)
  {
    Polar[2]=90
  }
  if (x==0 && y==0)
  {
    Polar[2]=0
  }
  Polar
}