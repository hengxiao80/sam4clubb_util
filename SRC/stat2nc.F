c-------------------------------------------------------
c $Id: stat2nc.f 1479 2014-08-02 15:17:05Z dschanen@uwm.edu $
        program stat2nc

c       Author:
c       Marat Khairoutdinov? Someone at CSU?

c       Modifications:
c       Modified to use COARDS conventions.
c       Dave Schanen, UW-Milwaukee, Feb 2007.

c               ***** IMPORTANT *****
c     If you shift around a bunch of lines of code, then
c     the line numbers for the handle_err() calls are
c     probably wrong.  You will need to change them.
c               ***** IMPORTANT *****
c-------------------------------------------------------        
        implicit none
#ifdef UWM_STATS
#include "netcdf.inc"
#include "hbuf.inc"        
#else
        include 'netcdf.inc'
        include 'hbuf.inc'        
#endif   
        integer flag(HBUF_MAX_LENGTH)
c-------------------------------------------------------        
c---> COARDS addition
        character date*35
        integer year, month, day, hours, minutes
        integer offset
        real lat, lon, dt
c---> end COARDS addition

        character caseid*40, version*20, filename*148, filename2*148
        character name*80
        integer nzm, ntime,npar
#ifndef UWM_STATS
        real time(50000),z(5000),p(5000)
        real f(9000000), f1(9000000), parms(500000)
        real tmp(5000), tmp1(5000), tmp2(5000), tmp3(5000)
        real rho(5000)
#else        
        real time(5000000),z(5000),p(5000)
        real f(9000000), f1(9000000), parms(500000)
        real tmp(500000), tmp1(500000), tmp2(500000), tmp3(500000)
        real rho(500000)
#endif
        real cape,cin

        integer i,j,k,l,m,nparms
c       integer vdimids(2), ndimids
        integer ndimids
        parameter ( ndimids = 4 )
        integer vdimids(ndimids)
        integer ncid,err,zid,timeid,varid
c---> COARDS addition
        integer xid, yid
c---> end COARDS addition
#ifdef UWM_STATS
        character(80) long_name
        character(20) abbr_name
        character(12) units
#else
        character(80) long_name
        character(8) abbr_name
        character(12) units
#endif
        integer iargc
        external iargc

        external handle_err

        time=0.
        z=0.
        p=0.
        f=0.
c-------------------------------------------------------        
        m=iargc()
        if(m.eq.0.) then
         print*,'you forgot to specify the name of the file.'
         stop
        endif        
        call getarg(1,filename)
        print *,'open file: ',filename
        open(2,file=filename,status='old',form='unformatted')

        do i=1,144
         if(filename(i:i+4).eq.'.stat') then
          filename2=filename(1:i-1)//'.nc'
          print*,filename2
         endif
        end do


c-------------------------------------------------------        
c Count how many time points in a file:
        call HBUF_info(2,ntime,time,nzm,z,p,caseid,version)
        call HBUF_read(2,nzm,'RHO',1,1,rho,m)
        print*,'.......',ntime
        print*,(time(k),k=1,ntime/3)
        print*
        print*,(z(k),k=1,nzm)
        print*
        print*,(p(k),k=1,nzm)
        print*
        print*,(rho(k),k=1,nzm)

        ! Create the netCDF data set, using a 64 bit offset for files
        ! that are larger than 2 gigabytes
        err = NF_CREATE(filename2, OR(NF_CLOBBER,NF_64BIT_OFFSET), ncid)
        call handle_err( err, 112 )

        err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'SAM version',
     &                           len_trim(version),version)
        call handle_err( err, 116 )
        err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'caseid',
     &                                len_trim(caseid),caseid)
        call handle_err( err, 119 )

        err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'Conventions',
     &            6, "COARDS" )
        call handle_err( err, 123 )

        err = NF_PUT_ATT_TEXT(ncid,NF_GLOBAL,'caseid',
     &                                  len_trim(caseid),caseid)
        call handle_err( err, 127 )

        err = NF_DEF_DIM(ncid, 'x', 1, xid)
        call handle_err( err, 130 )
        err = NF_DEF_DIM(ncid, 'y', 1, yid)
        call handle_err( err, 132 )

        err = NF_DEF_VAR(ncid, 'x', NF_FLOAT, 1, xid, varid)
        call handle_err( err, 135 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'X')
        call handle_err( err, 137 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',9,'degrees_E')
        call handle_err( err, 139 )
        err = NF_PUT_ATT_INT(ncid,varid,'ipositive',NF_INT,1,1)
        call handle_err( err, 141 )
        err = NF_DEF_VAR(ncid, 'y', NF_FLOAT, 1, yid, varid)
        call handle_err( err, 143 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'Y')
        call handle_err( err, 145 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',9,'degrees_N')
        call handle_err( err, 147 )
        err = NF_PUT_ATT_INT(ncid,varid,'ipositive',NF_INT,1,1)
        call handle_err( err, 149 )

        err = NF_DEF_DIM(ncid, 'z', nzm, zid)
        call handle_err( err, 152 )
        err = NF_DEF_DIM(ncid, 'time', ntime, timeid)
        call handle_err( err, 154 )

        err = NF_DEF_VAR(ncid, 'z', NF_FLOAT, 1, zid, varid)
        call handle_err( err, 157 )
c---> COARDS addition
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'Z')
        call handle_err( err, 160 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'positive',2,'up')
        call handle_err( err, 162 )
        err = NF_PUT_ATT_INT(ncid,varid,'ipositive',NF_INT,1,1)
        call handle_err( err, 164 )
c<--- end COARDS addition
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',1,'m')
        call handle_err( err, 167 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',6,'height')
        call handle_err( err, 169 )

        err = NF_DEF_VAR(ncid, 'time', NF_FLOAT, 1, timeid, varid)
        call handle_err( err, 172 )
c---> COARDS addition
        err = NF_PUT_ATT_TEXT(ncid,varid,'cartesian_axis',1,'T')
        call handle_err( err, 175 )
#ifndef NO_READ
        write(*,'(a)',advance="no") "Year in YYYY = "
        read *, year 
        write(*,'(a)',advance="no") "Month in MM = "
        read *, month 
        write(*,'(a)',advance="no") "Day in DD = "
        read *, day
        write(*,'(a)',advance="no") "Hour in HH = "
        read *, hours
        write(*,'(a)',advance="no") "Minutes in MM = "
        read *, minutes
#else
        year = 1901
        month = 01
        day = 01
        hours = 0
        minutes = 0
#endif
        date = "minutes since YYYY-MM-DD 00:00:00.0"

        write(date(15:18),'(i4.4)') year
        write(date(20:21),'(i2.2)') month
        write(date(23:24),'(i2.2)') day
        write(date(26:27),'(i2.2)') hours
        write(date(29:30),'(i2.2)') minutes

        err = NF_PUT_ATT_TEXT( ncid, varid, 'units', len( date ), date )
        call handle_err( err, 203 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',4,'time')
        call handle_err( err, 205 )

        err = NF_DEF_VAR(ncid, 'p', NF_FLOAT, 1, zid, varid)
        call handle_err( err, 208 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',2,'mb')
        call handle_err( err, 210 )
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',8,'pressure')
        call handle_err( err, 212 )

        err = NF_ENDDEF(ncid)
        call handle_err( err, 215 )

        err = NF_INQ_VARID(ncid,'time',varid)
        call handle_err( err, 218 )

        ! Assume time starts at 0 for simplicity and viewing in GrADS
        ! with sdfopen -dschanen 20 Oct 2011
        dt = nint( ( time(2) - time(1) )*1440. )

        do i = 1, ntime
          time(i) = real( i ) * dt
        end do

        err = NF_PUT_VAR_REAL( ncid, varid, time ) 
        call handle_err( err, 229 )

        err = NF_INQ_VARID(ncid,'z',varid)
        call handle_err( err, 232 )

        write(*,'(a)',advance="no") "Distance of the bottom point from"
     &   // " the true surface (on the zm grid; for ASc cases) = "

#ifndef NO_READ
        read *, offset 
#else
        offset = 0
#endif
        z(:) = z(:) + offset

        err = NF_PUT_VAR_REAL(ncid, varid, z)
        call handle_err( err, 245 )

        err = NF_INQ_VARID(ncid,'p',varid)
        call handle_err( err, 248 )
        err = NF_PUT_VAR_REAL(ncid, varid, p)
        call handle_err( err, 250 )

c---> COARDS addition
        write(*,'(a)',advance="no") "Latitude in degrees N (+-) = "
#ifndef NO_READ
        read *, lat 
#else
        lat = 0.0
#endif
        write(*,'(a)',advance="no") "Longitude in degrees E (+-) = "
#ifndef NO_READ
        read *, lon 
#else
        lon = 0.0
#endif
        err = NF_INQ_VARID(ncid,'y',varid)
        call handle_err( err, 266 )
        err = NF_PUT_VAR_REAL(ncid, varid, lat ) 
        call handle_err( err, 268 )
        err = NF_INQ_VARID(ncid,'x',varid)
        call handle_err( err, 270 )
        err = NF_PUT_VAR_REAL(ncid, varid, lon )
        call handle_err( err, 272 )
c<--- end COARDS addition
! Commented out generation of 1-D fields as they are unusable
! by GrADS. In addition, by omitting these, stat2nc runs
! significantly faster
! 
! Joshua Fasching 15 Sept 2007
!
!  1-D fields now:
!
       nparms = 60
       call HBUF_parms(2,parms,nparms)
       call HBUF_read(2,nzm,'RHO',1,1,rho,m)        
c--------------------------------------------------------
       long_name = 'SST'
       abbr_name = 'SST'
       units = 'K'
       npar = 1
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Surface Pressure'
       abbr_name = 'Ps'
       units = ' '
       npar = 2
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Shaded Cloud Fraction'
       abbr_name = 'CLDSHD'
       units = ' '
       npar = 3
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Surface Precip. Fraction'
       abbr_name = 'AREAPREC'
       units = ' '
       npar = 4
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Cloud Fraction above 245K level'
       abbr_name = 'CLD245'
       units = ' '
       npar = 5
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
      err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Maximum Updraft Velocity'
       abbr_name = 'WMAX'
       units = 'm/s'
       npar = 6
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name

c--------------------------------------------------------
       long_name = 'Convective Precipitation Rate'
       abbr_name = 'PRECC'
       units = 'mm/day'
       npar = 7
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c--------------------------------------------------------
       long_name = 'Stratiform Precipitation Rate'
       abbr_name = 'PRECS'
       units = 'mm/day'
       npar = 8
       err = NF_REDEF(ncid)
       err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
       err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
       err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
       err = NF_ENDDEF(ncid)
       tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
       err = NF_PUT_VAR_REAL(ncid, varid, tmp)
       print*,long_name
c----------------------------------------------------------
        long_name = 'Surface Precipitation'
        abbr_name = 'PREC'
        units = 'mm/day'
        f = 0.
        call HBUF_read(2,nzm,'PRECIP',1,ntime,f,m)
        do i=1,ntime
         tmp(i)=f(1+(i-1)*nzm)
        end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,abbr_name, long_name
c----------------------------------------------------------
!        long_name = 'Latent Heat Flux'
!        abbr_name = 'LHF'
!        units = 'W/m2'
!        call HBUF_read(2,nzm,'QTFLUX',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=f(1+(i-1)*nzm)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'Sensible Heat Flux'
!        abbr_name = 'SHF'
!        units = 'W/m2'
!        call HBUF_read(2,nzm,'TLFLUX',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=f(1+(i-1)*nzm)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'Precipitable Water'
!        abbr_name = 'PW'
!        units = 'mm'
!        call HBUF_read(2,nzm,'QV',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=0.
!         do k=1,nzm-1
!          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
!         end do
!         tmp(i)=tmp(i)*1.e-3
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'Observed Precipitable Water'
!        abbr_name = 'PWOBS'
!        units = 'mm'
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=0.
!        do k=1,nzm-1
!          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
!         end do
!         tmp(i)=tmp(i)*1.e-3
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!
c----------------------------------------------------------
        long_name = 'Cloud Water Path'
        abbr_name = 'CWP'
        units = 'g/m2'
        f = 0.
        call HBUF_read(2,nzm,'QC',1,ntime,f,m)
        do i=1,ntime
         tmp(i)=0.
         do k=1,nzm-1
          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
         end do
        end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
         err = NF_ENDDEF(ncid)
         err = NF_PUT_VAR_REAL(ncid, varid, tmp)
         print*, abbr_name, long_name
c----------------------------------------------------------
        long_name = 'Ice Water Path'
        abbr_name = 'IWP'
        units = 'g/m2'
        f = 0.
        call HBUF_read(2,nzm,'QI',1,ntime,f,m)
        do i=1,ntime
         tmp(i)=0.
         do k=1,nzm-1
          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
         end do
        end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*, abbr_name, long_name
c----------------------------------------------------------
        long_name = 'Rain Water Path'
        abbr_name = 'RWP'
        units = 'g/m2'
        f = 0.
        call HBUF_read(2,nzm,'QR',1,ntime,f,m)
        do i=1,ntime
         tmp(i)=0.
         do k=1,nzm-1
          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
         end do
        end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                  len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*,abbr_name, long_name
c----------------------------------------------------------
        long_name = 'Snow Water Path'
        abbr_name = 'SWP'
        units = 'g/m2'
        f = 0.
        call HBUF_read(2,nzm,'QS',1,ntime,f,m)
        do i=1,ntime
         tmp(i)=0.
         do k=1,nzm-1
          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
         end do
        end do
        err = NF_REDEF(ncid)
        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
     &                                  1, timeid,varid)
        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                 len_trim(long_name),trim(long_name))
        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                       len_trim(units),trim(units))
        err = NF_ENDDEF(ncid)
        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
        print*, abbr_name, long_name
c----------------------------------------------------------
!        long_name = 'Grauple Water Path'
!        abbr_name = 'GWP'
!        units = 'g/m2'
!        call HBUF_read(2,nzm,'QG',1,ntime,f,m)
!        do i=1,ntime
!         tmp(i)=0.
!         do k=1,nzm-1
!          tmp(i)=tmp(i)+rho(k)*f(k+(i-1)*nzm)*(z(k+1)-z(k))
!         end do
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'CAPE'
!        abbr_name = 'CAPE'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QV',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp2(i) =  cape(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!         tmp3(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!          print*,'CAPE: CIN:',tmp2(i), tmp3(i)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
!
c----------------------------------------------------------
!        long_name = 'CAPEOBS'
!        abbr_name = 'CAPEOBS'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABSOBS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp2(i) =  cape(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!         tmp3(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!          print*,'CAPEOBS: CINOBS:',tmp2(i), tmp3(i)
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'CIN'
!        abbr_name = 'CIN'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QV',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm) 
!         tmp2(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
c----------------------------------------------------------
!        long_name = 'CINOBS'
!        abbr_name = 'CINOBS'
!        units = 'J/kg'
!        call HBUF_read(2,nzm,'TABSOBS',1,ntime,f,m)
!        call HBUF_read(2,nzm,'QVOBS',1,ntime,f1,m)
!        do i=1,ntime
!         tmp(1:nzm)=f(1+(i-1)*nzm:nzm+(i-1)*nzm)
!         tmp1(1:nzm)=f1(1+(i-1)*nzm:nzm+(i-1)*nzm) 
!         tmp2(i) =  cin(nzm,p(1:nzm),tmp(1:nzm),tmp1(1:nzm))
!        end do
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp2)
!        print*,long_name
!
!
!
!
c--------------------------------------------------------
!        if(nparms.gt.8) then
!
!        print*,'nparms=',nparms
!
!        long_name = 'Net LW flux at sfc'
!        abbr_name = 'LWNS'
!        units = 'W/m2'
!        npar = 9
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'Net LW flux at TOA'
!        abbr_name = 'LWNT'
!        units = 'W/m2'
!        npar = 10
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'Net LW flux at sfc (Clear Sky)'
!        abbr_name = 'LWNSC'
!        units = 'W/m2'
!        npar = 11
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Net LW flux at TOA (Clear Scy)'
!        abbr_name = 'LWNTC'
!        units = 'W/m2'
!        npar = 12
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Downward LW flux at sfc'
!        abbr_name = 'LWDS'
!        units = 'W/m2'
!        npar = 13
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Net SW flux at sfc'
!        abbr_name = 'SWNS'
!        units = 'W/m2'
!        npar = 14
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Net SW flux at TOA'
!        abbr_name = 'SWNT'
!        units = 'W/m2'
!        npar = 15
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name


c--------------------------------------------------------
!        long_name = 'Net SW flux at sfc (Clear Sky)'
!        abbr_name = 'SWNSC'
!        units = 'W/m2'
!        npar = 16
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Net SW flux at TOA (Clear Scy)'
!        abbr_name = 'SWNTC'
!        units = 'W/m2'
!        npar = 17
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Downward SW flux at sfc'
!        abbr_name = 'SWDS'
!        units = 'W/m2'
!        npar = 18
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Incoming SW flux at TOA'
!        abbr_name = 'SOLIN'
!        units = 'W/m2'
!        npar = 19
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Observed SST'
!        abbr_name = 'SSTOBS'
!        units = 'K'
!        npar = 20
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'Observed Latent Heat Flux'
!        abbr_name = 'LHFOBS'
!        units = 'W/m2'
!        npar = 21
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'Observed Sensible Heat Flux'
!        abbr_name = 'SHFOBS'
!        units = 'SHFOBS'
!        npar = 22
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name

c--------------------------------------------------------
!        long_name = 'Low Cloud Fraction'
!        abbr_name = 'CLDLOW'
!        units = ' '
!        npar = 23
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'Middle Cloud Fraction'
!        abbr_name = 'CLDMID'
!        units = ' '
!        npar = 24
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'High Cloud Fraction'
!        abbr_name = 'CLDHI'
!        units = ' '
!        npar = 25
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'ISCCP Total Cloud Fraction (tau > 0.3)'
!        abbr_name = 'ISCCPTOT'
!        units = ' '
!        npar = 26
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'ISCCP Low Cloud Fraction (tau > 0.3)'
!        abbr_name = 'ISCCPLOW'
!        units = ' '
!        npar = 27
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'ISCCP Middle Cloud Fraction (tau > 0.3)'
!        abbr_name = 'ISCCPMID'
!        units = ' '
!        npar = 28
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
c--------------------------------------------------------
!        long_name = 'ISCCP High Cloud Fraction (tau > 0.3)'
!        abbr_name = 'ISCCPHGH'
!        units = ' '
!        npar = 29
!        err = NF_REDEF(ncid)
!        err = NF_DEF_VAR(ncid,trim(abbr_name),NF_FLOAT,
!     &                                  1, timeid,varid)
!        err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
!     &                  len_trim(long_name),trim(long_name))
!        err = NF_PUT_ATT_TEXT(ncid,varid,'units',
!     &                       len_trim(units),trim(units))
!        err = NF_ENDDEF(ncid)
!        tmp(1:ntime) = parms(npar:npar+nparms*(ntime-1):nparms)
!        err = NF_PUT_VAR_REAL(ncid, varid, tmp)
!        print*,long_name
!
!
!       end if

c--------------------------------------------------------------
c       ndimids=4
c       Note: These are in the reverse of this order in the NetCDF file.
c       -dschanen 13 Feb 2007
        vdimids(1) = xid
        vdimids(2) = yid
        vdimids(3) = zid
        vdimids(4) = timeid

        do k=1,hbuf_length

           write(6,'(a80)') namelist(k)//" "//deflist(k)
           call HBUF_read(2,nzm,namelist(k),1,ntime,f,m)

           err = NF_REDEF(ncid)
           call handle_err( err, 1085 )
           name = namelist(k)
           l=len_trim(name)
           err = NF_DEF_VAR(ncid,name(1:l),NF_FLOAT,
     &                                  ndimids,vdimids,varid)
           call handle_err( err, 1090 )
           err = NF_PUT_ATT_TEXT(ncid,varid,'long_name',
     &                                len_trim(deflist(k)),deflist(k))
           call handle_err( err, 1093 )
           err = NF_PUT_ATT_TEXT(ncid,varid,'units',
     &                              len_trim(unitlist(k)),unitlist(k))
           call handle_err( err, 1096 )
           err = NF_ENDDEF(ncid)
           call handle_err( err, 1098 )
           err = NF_PUT_VAR_REAL(ncid, varid, f)
           call handle_err( err, 1100 )

        end do


        err = NF_CLOSE(ncid)
        call handle_err( err, 1106 )

        end program stat2nc 

c-----------------------------------------------------------------------
        subroutine handle_err( err, line ) 

        implicit none

        include 'netcdf.inc'

        ! Parameters
        integer, parameter :: fstderr = 0

        ! Input Variables
        integer, intent(in) :: err, line

        if ( err .ne. NF_NOERR ) then
          write(fstderr,*) 'error:', trim( nf_strerror( err ) )
          write(fstderr,*) "line number ", line
          stop
        end if

        end subroutine handle_err
