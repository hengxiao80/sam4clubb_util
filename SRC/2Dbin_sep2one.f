      program main
      implicit none

c---------------------------------------------------------------
c variables:

      character(120) filename
      character(80) long_name
      character(10) units
      character(8)  name
      character(1)  blank/" "/
      character(4) rankchar

      real(4), allocatable :: byte(:)
      real(4) dx,dy,z(500),p(500),time
      integer nsubs,nsubsx,nsubsy,nx,ny,nz,nfields,nstep
      integer i,k,n,nx_gl,ny_gl,ifields

c External functions:

      integer, external :: iargc
      real fldmin, fldmax

c---------------------------------------------------------------
c---------------------------------------------------------------
c
c Read the file-name from the comman line:
c
      n=COMMAND_ARGUMENT_COUNT()
      if(n.eq.0) then
        print*,'no input-file name is specified.'
        print*,'Format: 2Dbin_sep2one input.2Dbin (without trailing _*)'
        stop
      end if
      call getarg(1,filename)

c---------------------------------------------------------------
c Read files; merge data from different subdomains;
c save as a netcdf file.
c
      open(1,file=trim(filename)//"_0",status='old',form='unformatted')
      open(3,file=trim(filename),status='new',form='unformatted')

      read(1,end=3333,err=3333) nstep
      print*, nstep
      write(3) nstep
      read(1) nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
      print*, nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
      write(3) nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
      read(1) dx
      read(1) dy
      read(1) time
      write(3) dx
      write(3) dy
      write(3) time

      allocate(byte(nx*ny))

      do ifields=0,nfields-1

        do n=0,nsubs-1
          if(n.ne.0) then 
            if (ifields .eq. 0) then
              write(rankchar,'(i4)') n
              open(8+n,file=trim(filename)//"_"//trim(adjustl(rankchar)),status='old',form='unformatted')
              read(8+n) ! nstep
              read(8+n) ! nx,ny,nzm,nsubdomains, nsubdomains_x,nsubdomains_y,nfields
              read(8+n) ! dx
              read(8+n) ! dy
              read(8+n) ! nstep*dt/(3600.*24.)+day0
            endif
            read(8+n) (byte(k),k=1,nx*ny)
          else
            read(1) name,blank,long_name,blank,units
            write(3) name,blank,long_name,blank,units
            print*,ifields+1,trim(long_name),' ',trim(units)
            read(1) (byte(k),k=1,nx*ny)
          end if
          write(3) (byte(k),k=1,nx*ny)
        end do ! n

      end do ! ifields

 3333 continue

      end



