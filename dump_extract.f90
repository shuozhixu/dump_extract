   program dump_extract

!  From a large LAMMPS dump file, dump.cac, this program extracts
!  some atoms whose ids are stored in group_atomap.id,
!  and then output these atoms to a smaller LAMMPS dump file, dump.group

!  dump.cac is a LAMMPS dump file;
!  group_atomap.id file contains id of atoms which are to be extracted;
!  dump.group is the output LAMMPS dump file.

!  Author: Shuozhi Xu (shuozhixu@gatech.edu)
!  Date: 04/22/2015
 
   implicit none
 
   integer :: i, ia, iosline, iospara, atomap_num, id_num, id_check, allostat

   character(len = 50), dimension(9) :: paraline

   double precision, dimension(3) :: box_h, box_l

   logical, dimension(3) :: need_atom

   double precision, allocatable :: r_atomap(:, :)

   logical, allocatable :: need(:, :)

   open(1, file = 'dump.cac', status = 'old', &
           action = 'read', position = 'rewind')

   open(2, file = 'group_atomap.id', status = 'old', &
           action = 'read', position = 'rewind')

   open(3, file = 'dump.group', status = 'replace', &
           action = 'write', position = 'rewind')

!  initialize

   paraline(:) = ''

   box_h(:) = 0.

   box_l(:) = 0.

   need_atom(:) = .false.

   atomap_num = 0

   id_num = 0

!  read dump.cac

!  read line 1-4

   do i = 1, 4

     read(1, '(a)', iostat = iosline) paraline(i)

     if(iosline > 0) then

       print *, 'Error: Wrong reading', i, ' line ', paraline(i), &
                ' because of', iosline
       stop

     end if

   end do

   read(paraline(4), *, iostat = iospara) atomap_num

   if(iospara > 0) then

     print *, 'Error: Wrong reading atomap_num', atomap_num, &
              ' because of', iospara
     stop

   end if

!  allocate and initialize r_atomap and need

   allocate( &
     r_atomap(3, atomap_num), &
     need(3, atomap_num), &
     stat = allostat &
   )

   if(allostat /= 0) then

     print *, 'Error: Fail to allocate r_atomap because of', allostat
     stop

   else

     r_atomap(:, :) = 0.

     need(:, :) = .false.

   end if

!  read line 5-9

   do i = 5, 9

     read(1, '(a)', iostat = iosline) paraline(i)

     if(iosline > 0) then

       print *, 'Error: Wrong reading', i, ' line ', paraline(i), &
                ' because of', iosline
       stop

     end if

   end do

!  read atomic positions in the original file

   do ia = 1, atomap_num

     read(1, '(3f15.8)', iostat = iospara) r_atomap(1:3, ia)

     if(iospara > 0) then

       print *, 'Error: Wrong reading r_atomap', r_atomap(1:3, ia), &
                ' of atomap', ia, ' because of', iospara
       stop

     end if

   end do

!  read group_atomap.id to update need

   id_num = 0
 
   do while(iospara == 0)

     read(2, '(i16)', iostat = iospara) ia

     if(iospara > 0) then

       print *, 'Error: Wrong reading ia', ia, ' because of', iospara
       stop

     else if(iospara == 0) then

       id_num = id_num+1

       need(:, ia) = .true.

     end if

   end do

!  write dump.group

!  write line 1-3

   do i = 1, 3

     write(3, '(a)', iostat = iosline) paraline(i)

     if(iosline > 0) then

       print *, 'Error: Wrong writing', i, ' line ', paraline(i), &
                ' because of', iosline
       stop

     end if

   end do

!  write line 4

   write(3, '(i16)', iostat = iospara) id_num

   if(iospara > 0) then

     print *, 'Error: Wrong writing id_num', id_num, &
              ' because of', iospara
     stop

   end if

!  write line 5

   write(3, '(a)', iostat = iosline) paraline(5)

   if(iosline > 0) then

     print *, 'Error: Wrong writing 5th line ', paraline(5), &
              ' because of', iosline
     stop

   end if

!  write line 6-8

!  update boundaries

   box_h(:) = maxval(r_atomap, 2, need)

   box_l(:) = minval(r_atomap, 2, need)

   do i = 1, 3

     write(3, '(2f15.8)', iostat = iospara) box_l(i), box_h(i)

   end do

!  write line 9

   write(3, '(a)', iostat = iosline) paraline(9)

   if(iosline > 0) then

     print *, 'Error: Wrong writing 9th line ', paraline(9), &
              ' because of', iosline
     stop

   end if

   id_check = 0

   do ia = 1, atomap_num

     need_atom(:) = need(:, ia)

     if(all(need_atom).eqv..true.) then

       id_check = id_check+1

       write(3, '(3f15.8)', iostat = iospara) r_atomap(1:3, ia)

       if(iospara > 0) then

         print *, 'Error: Wrong writing r_atomap', r_atomap(1:3, ia), &
                  ' of atomap', ia, ' because of', iospara
         stop

       end if

     end if

   end do

!  debug

   if(id_check /= id_num) then

     print *, 'Error: Wrong id_check', id_check, ' which should be', id_num
     stop

   end if

   stop
   end program dump_extract
