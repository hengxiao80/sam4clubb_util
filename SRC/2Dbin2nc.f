      ! (C) 2000 Marat Khairoutdinov

      program main

      implicit none
      include 'netcdf.inc'

      ! variables:

      character(80) filename,filename1,long_name
      character(10) units
      character(8)  name
      character(1)  blank

      real(4), allocatable :: byte(:)
      real(4), allocatable :: fld(:)
      real fmax,fmin,day0
      real dx,dy,x(100000),y(100000),time
      logical condition
      integer nsubs,nsubsx,nsubsy,nx,ny,nz,nfields,nstep
      integer i,j,k,k1,k2,n,i0,j0,nx_gl,ny_gl,length,ifields

      integer vdimids(3),start(3),count(3),ndimids,nfile
      integer ncid,err,yid,xid,timeid,ntime,varrr
      integer nrecords

      ! External functions:

      integer iargc,strlen1
      external iargc,strlen1

      real fldmin, fldmax

      nrecords = 10000! max number ot time steps in one file

      ! Read the file-name from the comman line:

      i=COMMAND_ARGUMENT_COUNT()
      if(i.eq.0) then
        print*,'no input-file name is specified.'
        print*,'Format: 2Dbin2nc input.2Dbin'
        stop
      end if
      call getarg(1,filename)

      ! Read files; merge data from different subdomains;
      ! save as a netcdf file.

      open(1,file=filename,status='old',form='unformatted')

      ntime=1
      nfile=0

      do while(.true.) ! infinit loop 

        ! The output filename:
        condition=mod(ntime-1,nrecords).eq.0
        if(condition) then
          ntime=1
          nfile=nfile+1
          filename1=filename
          do i=1,76
            if(filename1(i:i+5).eq.'.2Dbin') then
              if(nfile.lt.10) then
                write(filename1(i+6:i+12),'(a1,i1,a3)') '_',nfile,'.nc'
              else if(nfile.lt.100) then
                write(filename1(i+6:i+13),'(a1,i2,a3)') '_',nfile,'.nc'
              else if(nfile.lt.1000) then
                write(filename1(i+6:i+14),'(a1,i3,a3)') '_',nfile,'.nc'
              end if
              print*,filename1
              EXIT
            else if(i.eq.76) then
              print*,'wrong file name extension!'
              stop
            endif
          end do
          if(nfile.ne.1) err = NF_CLOSE(ncid)
        end if

        read(1,end=3333,err=3333) nstep
        read(1) nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
        read(1) dx
        read(1) dy
        read(1) time
  
        print*,'nx,ny,nz,nsubs,nsubsx,nsubsy,nfields:'
        print*,nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
        
        nx_gl=nx*nsubsx
        ny_gl=ny*nsubsy
        if(condition) then
          print*,'nx_gl=',nx_gl, '    dx=',dx
          print*,'ny_gl=',ny_gl, '    dy=',dy
        end if
  
        do i=1,nx_gl
          x(i) = dx*(i-1)
        end do
        do j=1,ny_gl
          y(j) = dy*(j-1)
        end do
  
        if(ntime.eq.1.and.nfile.eq.1) then
           day0=time
           allocate(byte(nx*ny))
           allocate(fld(nx_gl*ny_gl))
        end if

        ! Initialize netcdf stuff, define variables,etc.

        if(condition) then
  
          err = NF_CREATE(filename1, NF_CLOBBER, ncid)
          err = NF_REDEF(ncid)
  
          err = NF_DEF_DIM(ncid, 'x', nx_gl, xid)
          if(ny_gl.ne.1) err = NF_DEF_DIM(ncid, 'y', ny_gl, yid)
          err = NF_DEF_DIM(ncid, 'time', NF_UNLIMITED, timeid)
  
          err = NF_DEF_VAR(ncid, 'x', NF_FLOAT, 1, xid, varrr)
          err = NF_PUT_ATT_TEXT(ncid,varrr,'units',1,'m')
          if(ny_gl.ne.1) then
            err = NF_DEF_VAR(ncid, 'y', NF_FLOAT, 1, yid, varrr)
            err = NF_PUT_ATT_TEXT(ncid,varrr,'units',1,'m')
          endif
          err = NF_DEF_VAR(ncid, 'time', NF_FLOAT, 1, timeid, varrr)
          err = NF_PUT_ATT_TEXT(ncid,varrr,'units',3,'day')
          err = NF_PUT_ATT_TEXT(ncid,varrr,'long_name',4,'time')
  
          err = NF_ENDDEF(ncid)
  
          err = NF_INQ_VARID(ncid,'x',varrr)
          err = NF_PUT_VAR_REAL(ncid, varrr, x)
          if(ny_gl.ne.1) then
            err = NF_INQ_VARID(ncid,'y',varrr)
            err = NF_PUT_VAR_REAL(ncid, varrr, y)
          endif
  
        end if ! condition
  
        if(ny_gl.ne.1) then
          ndimids=3
          vdimids(1) = xid
          vdimids(2) = yid
          vdimids(3) = timeid
          start(1) = 1
          start(2) = 1
          start(3) = ntime 
          count(1) = nx_gl
          count(2) = ny_gl
          count(3) = 1 
        else
          ndimids=2
          vdimids(1) = xid
          vdimids(2) = timeid
          start(1) = 1
          start(2) = ntime 
          count(1) = nx_gl
          count(2) = 1 
        endif
  
        do ifields=1,nfields
        
          read(1) name,blank,long_name,blank,units
          print*,long_name
          do n=0,nsubs-1
  
            read(1) (byte(k),k=1,nx*ny)
  
            j0 = n/nsubsx 
            i0 = n - j0*nsubsx      
            i0 = i0 * (nx_gl/nsubsx) 
            j0 = j0 * (ny_gl/nsubsy)  
            length=0
            do j=1+j0,ny+j0
              do i=1+i0,nx+i0
                length=length+1
                fld(i+nx_gl*(j-1))=byte(length) 
              end do
            end do
  
          end do ! n
  
         if(condition) then
           err = NF_REDEF(ncid)
           err = NF_DEF_VAR(ncid,name,NF_FLOAT,ndimids,vdimids,varrr)
           err = NF_PUT_ATT_TEXT(ncid,varrr,'long_name',strlen1(long_name),long_name(1:strlen1(long_name)))
           err = NF_PUT_ATT_TEXT(ncid,varrr,'units',strlen1(units),units(1:strlen1(units)))
           err = NF_ENDDEF(ncid)
         end if
          
         err = NF_INQ_VARID(ncid,name,varrr)
         err = NF_PUT_VARA_REAL(ncid,varrr,start,count,fld)
  
        end do
  
        err = NF_INQ_VARID(ncid,'time',varrr)
        err = NF_PUT_VAR1_REAL(ncid,varrr,ntime,time)
  
        ntime=ntime+1

      end do ! while

 3333 continue

      err = NF_CLOSE(ncid)

      end
      
      integer function strlen1(str)
        character*(*) str
        strlen1=len(str)
        do i=len(str),1,-1
          if(str(i:i).ne.' ') then
            strlen1=i
            return
          endif 
        end do
        return
      end
