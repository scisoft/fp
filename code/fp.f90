program fp
    implicit none
    integer :: i
    real, dimension(:,:), allocatable :: x

    x = qeq(life(5), 4.0, 2.0)
    print *, x
contains
   pure function life(a) result(b)
      integer, intent(in) :: a
      real, dimension(:,:), allocatable :: b

      allocate(b(a,a))
      b = 42.0
   end function
   elemental function qeq(a, b, c) result(y)
      real, intent(in) :: a, b, c
      real :: y

      y = -b + sqrt(b**2 + 4*a*c)/a/2.0
   end function
end program
