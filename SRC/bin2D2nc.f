 
c (C) 2000 Marat Khairoutdinov
c
        program bin2D2nc

        implicit none

        include 'netcdf.inc'

c  Modifications to use COARDS conventions.
c  Dave Schanen, UW-Milwaukee, Jul 2008.
c---------------------------------------------------------------
c variables:
        real, external :: m2deglon, m2deglat

        character(80) filename,long_name
        character(86) filename1
        character(10) units
        character(8)  name
        character(1)  blank

        real, allocatable :: byte(:)
        real, allocatable :: fld(:)
        real zmax,day0
        real dx,dy,z(500),pres(500),x(100000),y(100000),time(1)
        logical condition
        integer nsubs,nsubsx,nsubsy,nx,ny,nz,nfields,nz_max
        integer i,j,k,n,i0,j0,nx_gl,ny_gl,length,ifields

        ! Added for COARDS
        real(4), allocatable, dimension(:) :: x_in_deg, y_in_deg
        real(4) :: time_minutes

        integer vdimids(4),start(4),count(4),ndimids,nfile
        integer ncid,err,zid(1),yid(1),xid(1),timeid(1)
        integer varid(99),varrr(1),ntime
        integer nrecords
c External functions:

        integer iargc,strlen1
        external iargc,strlen1

        real fldmin, fldmax

        zmax = 30000.  ! maximum height
        nrecords=500 ! the maximum number of time records in one file:
        time_minutes = 0. ! Kluge for GrADS -dschanen

c---------------------------------------------------------------
c---------------------------------------------------------------
c
c Read the file-name from the comman line:
c
        i=iargc()
        if(i.eq.0) then
          print*,'no input-file name is specified.'
          print*,'Format: bin2Dnc input.bin2D'
          stop
        end if
        call getarg(1,filename)

c---------------------------------------------------------------
c Read files; merge data from different subdomains;
c save as a netcdf file.
c
        open(1,file=filename,status='old',form='unformatted')


        ntime=1
        nfile=1
      do while(.true.) ! infinit loop 

c
c The output filename:

        condition=mod(ntime-1,nrecords).eq.0
        if(condition) then
           filename1=filename
          print*,filename1
          do i=1,76
            if(filename1(i:i+5).eq.'.bin2D') then
              write(filename1(i+6:i+12),'(a1,i1,a3)') '_',nfile,'.nc'
              print*,filename1
              if(nfile.ne.1) err = NF_CLOSE(ncid)
              ntime=1
              nfile=nfile+1
              EXIT
            else if(i.eq.76) then
              print*,'wrong filename extension!'
              stop
            endif
          end do
        end if

        read(1,end=3333,err=3333) nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
        do k=1,nz
          read(1) z(k)
        end do
        do k=1,nz
          read(1) pres(k)
        end do
        read(1) dx
        read(1) dy
        read(1) time(1)
        time_minutes = time_minutes + 1.0


        print*,'nx,ny,nz,nsubs,nsubsx,nsubsy,nfields:'
        print*,nx,ny,nz,nsubs,nsubsx,nsubsy,nfields
        
        nx_gl=nx*nsubsx
        ny_gl=ny*nsubsy

        do i=1,nx_gl
         x(i) = dx*(i-1)
        end do
        do j=1,ny_gl
         y(j) = dy*(j-1)
        end do

        if(ntime.eq.1.and.nfile.eq.2) then
          print*,'allocate'
          do k=1,nz
            if(z(k).lt.zmax) nz_max=k
          end do
          day0=time(1)
          allocate(byte(nx*ny*nz))
          allocate(fld(nx_gl*ny_gl*nz))
        end if
        print*,'nx_gl=',nx_gl
        print*,'ny_gl=',ny_gl
        print*,'nz_max=',nz_max
c
c Initialize netcdf stuff, define variables,etc.
c
       if(condition) then



        err = NF_CREATE(filename1, NF_CLOBBER, ncid)
        err = NF_REDEF(ncid)

        err = NF_DEF_DIM(ncid, 'x', nx_gl, xid)
        err = NF_DEF_DIM(ncid, 'y', ny_gl, yid)
        err = NF_DEF_DIM(ncid, 'z', nz_max, zid)
        err = NF_DEF_DIM(ncid, 'time', NF_UNLIMITED, timeid)

        err = NF_DEF_VAR(ncid, 'x', NF_FLOAT, 1, xid, varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'X') ! COARDS
!       err = NF_PUT_ATT_TEXT(ncid,varid,'units',1,'m')
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',9,'degrees_E') ! COARDS
!       if(ny_gl.ne.1) then
        if( .true. ) then
         err = NF_DEF_VAR(ncid, 'y', NF_FLOAT, 1, yid, varid)
         err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'Y') ! COARDS
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',1,'m')
         err = NF_PUT_ATT_TEXT(ncid,varid,'units',9,'degrees_N') ! COARDS
        endif
        err = NF_DEF_VAR(ncid, 'z', NF_FLOAT, 1, zid, varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'Z') ! COARDS
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',1,'m')
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',6,'height')
        err = NF_DEF_VAR(ncid, 'time', NF_FLOAT, 1, timeid, varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',7,'minutes')
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',4,'time')
        err = NF_DEF_VAR(ncid, 'p', NF_FLOAT, 1, zid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',2,'mb')
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',8,'pressure')

        err = NF_ENDDEF(ncid)

        err = NF_INQ_VARID(ncid,'x',varid)
c---> COARDS
        if ( .not. allocated( x_in_deg ) ) then
          allocate( x_in_deg(nx_gl), stat=err )
        end if
        if ( err /= 0 ) stop "allocate failed"
        do k = 1, nx_gl
          x_in_deg(k) = m2deglat( x(k) )
        end do
        err = NF_PUT_VAR_REAL( ncid, varid, x_in_deg )
c---> end COARDS
c       err = NF_PUT_VAR_REAL(ncid, varid, x)
!       if(ny_gl.ne.1) then
        if( .true. ) then
          err = NF_INQ_VARID(ncid,'y',varid)
c---> COARDS
          if ( .not. allocated( y_in_deg ) ) then
            allocate( y_in_deg(ny_gl), stat=err )
          end if
          if ( err /= 0 ) stop "allocate failed"
          do k = 1, ny_gl
            y_in_deg(k) = m2deglat( y(k) )
          end do
          err = NF_PUT_VAR_REAL( ncid, varid, y_in_deg )

          err = NF_PUT_VAR_REAL(ncid, varid, y_in_deg)
c---> end COARDS
c         err = NF_PUT_VAR_REAL(ncid, varid, y)
        endif
        err = NF_INQ_VARID(ncid,'z',varid)
        err = NF_PUT_VAR_REAL(ncid, varid, z(1))
        err = NF_INQ_VARID(ncid,'time',varid)
!       err = NF_PUT_VAR_REAL(ncid, varid, time(1)) ! Kluge for GrADS
        err = NF_PUT_VAR_REAL(ncid, varid, time_minutes)
        err = NF_INQ_VARID(ncid,'p',varid)
        err = NF_PUT_VAR_REAL(ncid, varid, pres(1))

        end if ! condition

!       if(ny_gl.ne.1) then
        if( .true. ) then
         ndimids=4
         vdimids(1) = xid(1)
         vdimids(2) = yid(1)
         vdimids(3) = zid(1)
         vdimids(4) = timeid(1)
         start(1) = 1
         start(2) = 1
         start(3) = 1
         start(4) = ntime 
         count(1) = nx_gl
         count(2) = ny_gl
         count(3) = nz_max
         count(4) = 1 
        else
         ndimids=3
         vdimids(1) = xid(1)
         vdimids(2) = zid(1)
         vdimids(3) = timeid(1)
         start(1) = 1
         start(2) = 1
         start(3) = ntime 
         count(1) = nx_gl
         count(2) = nz_max
         count(3) = 1 
        endif

        
        ifields=0

        do while(ifields.lt.nfields)
        
          read(1) name,blank,long_name,blank,units

          do n=0,nsubs-1
                 read(1) (byte(k),k=1,nx*ny*nz)
            j0 = n/nsubsx 
            i0 = n - j0*nsubsx        
            i0 = i0 * (nx_gl/nsubsx) 
            j0 = j0 * (ny_gl/nsubsy)  
            length=0
            do k=1,nz
             do j=1+j0,ny+j0
              do i=1+i0,nx+i0
                length=length+1
                fld(i+nx_gl*(j-1)+nx_gl*ny_gl*(k-1))=byte(length)
              end do
             end do
            end do

          end do ! n

            fldmin=1.e20
            fldmax=-1.e20
            do k=1,nz
            do j=1,ny_gl
            do i=1,nx_gl
                fldmin=min(fldmin,fld(i+nx_gl*(j-1)+nx_gl*ny_gl*(k-1)))
                fldmax=max(fldmax,fld(i+nx_gl*(j-1)+nx_gl*ny_gl*(k-1)))
            end do
            end do
            end do
            print*,n,long_name
            print*,fldmax,fldmin
c     &            maxval(fld(1:nx_gl*ny_gl*nz))
c                   minval(fld(1:nx_gl*ny_gl*nz)),




          ifields=ifields+1

         if(condition) then
          err = NF_REDEF(ncid)
          err = NF_DEF_VAR(ncid,name,NF_FLOAT,
     &                           ndimids,vdimids,varid(ifields))
          err = NF_PUT_ATT_TEXT(ncid,varid(ifields),'long_name',
     &          strlen1(long_name),long_name(1:strlen1(long_name)))
          err = NF_PUT_ATT_TEXT(ncid,varid(ifields),'units',
     &                strlen1(units),units(1:strlen1(units)))
          err = NF_ENDDEF(ncid)
         end if

         err = NF_PUT_VARA_REAL(ncid,varid(ifields),start,count,fld)

        end do ! while

         err = NF_INQ_VARID(ncid,'time',varrr(1))
         err = NF_PUT_VAR1_REAL(ncid,varrr,ntime,time_minutes)


        ntime = ntime+1

      end do

     
 3333        continue

        err = NF_CLOSE(ncid)

        end
c-----------------------------------------------------------------------
c functions m2deglat, m2deglon
c
c Description: Assuming we're on Earth and at lat/long (0,0), takes the 
c number of x points and number of y points file and determine the
c approximate degrees N and degrees E of each point.

c We also assume the the grid is aligned N/S E/W and (1,1) is the
c southwest corner of the grid for simplicity.

c-----------------------------------------------------------------------
        function m2deglat( in_meters ) result ( deg_N )
        implicit none

        real, parameter :: circum = 40000800. ! Approx. circum N/S [m]

        real, intent(in) :: in_meters

        real :: deg_N

        deg_N = ( in_meters * 360.0 )/ circum

        return
        end function m2deglat
c-----------------------------------------------------------------------
        function m2deglon( in_meters ) result( deg_E )
        implicit none

        real, parameter :: circum = 40075160. ! approx. circum E/W [m]

        real, intent(in) :: in_meters

        real :: deg_E

        deg_E = ( in_meters * 360.0 )/ circum
        
        return
        end function m2deglon



        integer function strlen1(str)
        character*(*) str
        strlen1=len(str)
        do i=1,len(str)
          if(str(i:i).ne.' ') then
            strlen1=strlen1-i+1
            return
          endif 
        end do
        return
        end
