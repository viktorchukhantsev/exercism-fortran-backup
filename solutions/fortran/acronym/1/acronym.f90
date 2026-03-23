
module acronym
   implicit none
contains

   function abbreviate(s)
      character(len=*), intent(in) :: s
      character(len=len_trim(s)) :: abbreviate
      character :: c
      integer :: i
      logical :: f, c1, c2

      f = .false.
      abbreviate = s(1:1)

      do i = 2, len_trim(s)
         c = s(i:i)
         c1 = ((iachar(c) >= iachar('a')) .and. (iachar(c) <= iachar('z')))
         c2 = ((iachar(c) >= iachar('A')) .and. (IACHAR(c) <= IACHAR('Z')))
         if (f .and. (c1 .or. c2)) then
            if ((iachar(c) >= iachar('a')) .and. (iachar(c) <= iachar('z'))) then
               c = char(iachar(c) - 32)
            end if
            abbreviate = trim(abbreviate)//c
            f = .false.
         end if
         if ((c == ' ') .or. (c == ',') .or. (c == '-')) then
            f = .true.
         end if
      end do
   end function

end module
