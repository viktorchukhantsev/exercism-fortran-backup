
module armstrong_numbers
   implicit none
contains

   logical function isArmstrongNumber(i)
      integer, intent(in) :: i
      integer :: r, j, s
      character(len=20) :: tmp
      integer :: digits

      write (tmp, '(I0)') abs(i)
      digits = len_trim(tmp)

      isArmstrongNumber = .false.

      j = i
      s = 0
      r = 1
      do while (r /= 0)
         r = mod(j, 10)
         j = j/10
         s = s + r**digits
      end do

      if (s == i) then
         isArmstrongNumber = .true.
      end if
   end function

end module
