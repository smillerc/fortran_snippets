program hello
  implicit none

  sync all
  write (*,*) "hello from image", this_image()
  sync all
  
end program hello