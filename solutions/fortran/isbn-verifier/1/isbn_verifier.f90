module isbn_verifier
   implicit none

contains

   function isValid(isbn) result(valid)
      character(*), intent(in) :: isbn
      logical :: valid, inv
      character :: c

      integer :: n(10), i, j, s

      valid = .false.
      j = 1
      inv = .false.

      do i = 1, LEN_TRIM(isbn)
         c = isbn(i:i)
         if ((j == 10) .and. (c == 'X')) then
           n(j) = 10
           j = j + 1
           cycle
         endif
         if ((IACHAR('0') <= iachar(c)) .and. (iachar('9') >= iachar(c))) then
            n(j) = IACHAR(c) - 48
            j = j + 1
         end if
         if ((c /= 'X') .and. ((IACHAR('A') <= iachar(c)) .and. (iachar('Z') >= iachar(c)))) then
            n(j) = IACHAR(c) - 48
            j = j + 1
         end if
      end do

      if ((j /= 11) .or. inv) then
         valid = .false.
         return
      end if

      s = 0
      do i = 1, 10
         s = s + i*n(11 - i)
      end do

      if (mod(s, 11) == 0) then
         valid = .true.
      else
         valid = .false.
      end if
   end function isValid

end module isbn_verifier
