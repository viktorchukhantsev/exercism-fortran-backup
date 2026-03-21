module pangram
   implicit none
contains

   logical function is_pangram(sentence)
      character(*), intent(in) :: sentence
      integer :: i
      integer :: e(26), code
      character :: c

      e(:) = 0
      code = -1
      c = ""

      if (LEN_TRIM(sentence) == 0) then
         is_pangram = .false.
         return
      end if

      is_pangram = .true.

      do i = 1, len(sentence)
         c = sentence(i:i)
         if ((iachar(c) >= iachar('A')) .and. (iachar(c) <= iachar('Z'))) then
            c = char(iachar(c) + 32)
         end if
         if (((iachar(c) < iachar('a')) .or. (iachar(c) > iachar('z')))) cycle

         code = iachar(c) - 96

         e(code) = e(code) + 1
      end do

      do i = 1, 26
      if (e(i) == 0) then
         is_pangram = .false.
         return
      end if
      end do
   end function is_pangram

end module pangram
