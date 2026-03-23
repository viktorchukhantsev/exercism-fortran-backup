
module collatz_conjecture
   implicit none
contains

   integer function steps(i)
      integer :: i, j
      steps = -1

      if (i <= 0) return
      if (i == 1) then
         steps = 0
         return
      end if

      j = i
      steps = 0
      do while (j /= 1)

         if (mod(j, 2) == 0) then
            j = j/2
         else
            j = j*3 + 1
         end if

         steps = steps + 1
      end do
   end function

end module
