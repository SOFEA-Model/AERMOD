      module ncpost
C***********************************************************************
C     This module implements netCDF POSTFILE support.
C***********************************************************************
      private

C --- netCDF Compression Level
      integer, parameter :: DEFLATE_LEVEL = 1

C --- netCDF Dimension IDs (NGRP,NAVE)
      integer, allocatable :: rec_dimid(:,:)
      integer, allocatable :: arc_dimid(:,:)
      integer, allocatable :: grp_dimid(:,:)
      integer, allocatable :: ave_dimid(:,:)
      integer, allocatable :: time_dimid(:,:)
      integer, allocatable :: strlen_dimid(:,:)

C --- netCDF Variable IDs (NGRP,NAVE)
      integer, allocatable :: x_varid(:,:)
      integer, allocatable :: y_varid(:,:)
      integer, allocatable :: zelev_varid(:,:)
      integer, allocatable :: zhill_varid(:,:)
      integer, allocatable :: zflag_varid(:,:)
      integer, allocatable :: rec_varid(:,:)
      integer, allocatable :: arc_varid(:,:)
      integer, allocatable :: arcid_varid(:,:)
      integer, allocatable :: grp_varid(:,:)
      integer, allocatable :: ave_varid(:,:)
      integer, allocatable :: time_varid(:,:)
      integer, allocatable :: clmsg_varid(:,:)

C --- netCDF Data Variable IDs (NGRP,NAVE,NTYP)
      integer, allocatable :: data_varid(:,:,:)

C --- Variables for Time Calculation
      logical :: l_timeinit = .false.
      character(len=31) :: time_units_att
      integer :: time_value = 0
      integer :: prev_time_value = -1
      integer :: init_jday, init_iyr

C --- Public Procedures
      public :: ncalloc
      public :: ncsetup
      public :: ncreset
      public :: ncwrite

      contains

      subroutine ncalloc
C***********************************************************************
C                 NCALLOC
C
C        PURPOSE: Allocate netCDF Arrays
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    September 29, 2018
C
C        INPUTS:
C
C        OUTPUTS:
C
C        CALLED FROM:  ALLSETUP
C***********************************************************************
#ifdef ENABLE_NETCDF
      use main1, only: ngrp, nave, ntyp, iounit, alloc_err
      implicit none

      integer :: iastat

      allocate (rec_dimid(ngrp,nave),
     &          arc_dimid(ngrp,nave), 
     &          grp_dimid(ngrp,nave),
     &          ave_dimid(ngrp,nave),
     &          time_dimid(ngrp,nave),
     &          strlen_dimid(ngrp,nave),
     &          x_varid(ngrp,nave),
     &          y_varid(ngrp,nave),
     &          zelev_varid(ngrp,nave),
     &          zhill_varid(ngrp,nave),
     &          zflag_varid(ngrp,nave),
     &          rec_varid(ngrp,nave),
     &          arc_varid(ngrp,nave),
     &          arcid_varid(ngrp,nave),
     &          grp_varid(ngrp,nave),
     &          ave_varid(ngrp,nave),
     &          time_varid(ngrp,nave),
     &          clmsg_varid(ngrp,nave),
     &          data_varid(ngrp,nave,ntyp),
     &          stat=iastat)

      if (iastat .ne. 0) then
C        call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',
     &                   'netCDF Arrays!'
      end if
#endif

      end subroutine ncalloc


      subroutine ncsetup
C***********************************************************************
C                 NCSETUP
C
C        PURPOSE: Create or Open netCDF POSTFILE
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    September 29, 2018
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   OUPOST
C***********************************************************************
      use main1
#ifdef ENABLE_NETCDF
      use netcdf
#endif
      implicit none

#ifdef ENABLE_NETCDF
      integer :: i
      character(len=5) :: data_label
      integer(kind=1), dimension(2), parameter :: clmsg_flags = (/1,2/)
      integer, parameter :: title_len = ilen_fld * 2 + 1
      character(len=title_len) :: title_att
      character(len=15) :: versn_att

C     Check for restart option (RSTINP).
      if (rstinp) then
C        This is a restarted run; open file for writing.
         call nccheck(nf90_open(pstfil(indgrp,indave),nf90_write,
     &                          ipsunt(indgrp,indave)))

C        Get dimension IDs.
         call nccheck(nf90_inq_dimid(ipsunt(indgrp,indave), "rec",
     &                               rec_dimid(indgrp,indave)))
         call nccheck(nf90_inq_dimid(ipsunt(indgrp,indave), "arc",
     &                               arc_dimid(indgrp,indave)))
         call nccheck(nf90_inq_dimid(ipsunt(indgrp,indave), "grp",
     &                               grp_dimid(indgrp,indave)))
         call nccheck(nf90_inq_dimid(ipsunt(indgrp,indave), "ave", 
     &                               ave_dimid(indgrp,indave)))
         call nccheck(nf90_inq_dimid(ipsunt(indgrp,indave), "time",
     &                               time_dimid(indgrp,indave)))

C        Get variable IDs.
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "x",
     &                               x_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "y",
     &                               y_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "zelev", 
     &                               zelev_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "zhill", 
     &                               zhill_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "zflag",
     &                               zflag_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "rec",
     &                               rec_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "arc",
     &                               arc_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "arcid",
     &                               arc_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "grp",
     &                               grp_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "ave",
     &                               ave_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "time", 
     &                               time_varid(indgrp,indave)))
         call nccheck(nf90_inq_varid(ipsunt(indgrp,indave), "clmsg", 
     &                               clmsg_varid(indgrp,indave)))
         do i = 1, numtyp
            if (outtyp(i) .eq. 'CONC') then
               call nccheck(nf90_inq_varid(ipsunt(indgrp,indave),
     &            "conc", data_varid(indgrp,indave,i)))
            else if (outtyp(i) .eq. 'DEPOS') then
               call nccheck(nf90_inq_varid(ipsunt(indgrp,indave),
     &            "depos", data_varid(indgrp,indave,i)))
            else if (outtyp(i) .eq. 'DDEP') then
               call nccheck(nf90_inq_varid(ipsunt(indgrp,indave),
     &            "ddep", data_varid(indgrp,indave,i)))
            else if (outtyp(i) .eq. 'WDEP') then
               call nccheck(nf90_inq_varid(ipsunt(indgrp,indave),
     &            "wdep", data_varid(indgrp,indave,i)))
            end if
         end do
      else
C        This is not a restarted run; create new file.
         call nccheck(nf90_create(pstfil(indgrp,indave),
     &                            nf90_netcdf4,
     &                            ipsunt(indgrp,indave)))

C        Define dimensions.
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "rec", 
     &      numrec, rec_dimid(indgrp,indave)))
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "arc", 
     &      numarc, arc_dimid(indgrp,indave)))
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "grp", 
     &      numgrp, grp_dimid(indgrp,indave)))
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "ave", 
     &      numave, ave_dimid(indgrp,indave)))
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "time", 
     &      nf90_unlimited, time_dimid(indgrp,indave)))
         call nccheck(nf90_def_dim(ipsunt(indgrp,indave), "strlen",
     &      8, strlen_dimid(indgrp,indave)))

C        Define variables.
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "x", 
     &      nf90_double, rec_dimid(indgrp,indave),
     &      x_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "y", 
     &      nf90_double, rec_dimid(indgrp,indave),
     &      y_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "zelev", 
     &      nf90_double, rec_dimid(indgrp,indave),
     &      zelev_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "zhill", 
     &      nf90_double, rec_dimid(indgrp,indave),
     &      zhill_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "zflag", 
     &      nf90_double, rec_dimid(indgrp,indave),
     &      zflag_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "rec", 
     &      nf90_int, rec_dimid(indgrp,indave),
     &      rec_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "arc", 
     &      nf90_int, rec_dimid(indgrp,indave),
     &      arc_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "arcid", 
     &      nf90_char,
     &      (/ strlen_dimid(indgrp,indave), arc_dimid(indgrp,indave) /),
     &      arcid_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "grp", 
     &      nf90_char,
     &      (/ strlen_dimid(indgrp,indave), grp_dimid(indgrp,indave) /),
     &      grp_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "ave", 
     &      nf90_int, ave_dimid(indgrp,indave),
     &      ave_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "time", 
     &      nf90_int, time_dimid(indgrp,indave),
     &      time_varid(indgrp,indave)))
         call nccheck(nf90_def_var(ipsunt(indgrp,indave), "clmsg", 
     &      nf90_byte, time_dimid(indgrp,indave),
     &      clmsg_varid(indgrp,indave)))
     
         do i = 1, numtyp
            if (outtyp(i) .eq. 'CONC') then
               data_label = 'conc'
            else if (outtyp(i) .eq. 'DEPOS') then
               data_label = 'depos'
            else if (outtyp(i) .eq. 'DDEP') then
               data_label = 'ddep'
            else if (outtyp(i) .eq. 'WDEP') then
               data_label = 'wdep'
            else
               cycle
            end if

C           The data array is structured so the first dimension varies
C           fastest. Though it differs from the in-memory structure of
C           AVEVAL, this allows for efficient hyperslab access when 
C           reading. Compression is also applied to these variables.

            call nccheck(nf90_def_var(ipsunt(indgrp,indave),
     &         trim(data_label), nf90_double,
     &         (/ time_dimid(indgrp,indave),
     &            rec_dimid(indgrp,indave),
     &            grp_dimid(indgrp,indave),
     &            ave_dimid(indgrp,indave) /),
     &         data_varid(indgrp,indave,i),
     &         deflate_level=DEFLATE_LEVEL))
         end do

C        Define attributes per CF conventions for time series data,
C        orthogonal multidimensional array representation.
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      x_varid(indgrp,indave), "units", "m"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      y_varid(indgrp,indave), "units", "m"))
         if (soelev .eq. 'METERS') then
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         zelev_varid(indgrp,indave), "units", "m"))
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         zhill_varid(indgrp,indave), "units", "m"))
         else if (soelev .eq. 'FEET') then
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         zelev_varid(indgrp,indave), "units", "ft"))
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         zhill_varid(indgrp,indave), "units", "ft"))
         end if
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      zflag_varid(indgrp,indave), "units", "m"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      time_varid(indgrp,indave), "axis", "T"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      time_varid(indgrp,indave), "units", time_units_att))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      x_varid(indgrp,indave), "axis", "X"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      y_varid(indgrp,indave), "axis", "Y"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      zelev_varid(indgrp,indave), "axis", "Z"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      rec_varid(indgrp,indave), "standard_name",
     &      "receptor"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      rec_varid(indgrp,indave), "cf_role", "timeseries_id"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      arc_varid(indgrp,indave), "standard_name",
     &      "arc_index"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      arcid_varid(indgrp,indave), "standard_name",
     &      "arc_id"))
         call nccheck(nf90_def_var_fill(ipsunt(indgrp,indave),
     &      arc_varid(indgrp,indave), 0, 0))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      grp_varid(indgrp,indave), "standard_name",
     &      "source_group"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      ave_varid(indgrp,indave), "standard_name",
     &      "averaging_period"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      ave_varid(indgrp,indave), "units", "hr"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      clmsg_varid(indgrp,indave), "standard_name",
     &      "calm_or_missing_hour status_flag"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      clmsg_varid(indgrp,indave), "flag_values",
     &      clmsg_flags))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      clmsg_varid(indgrp,indave), "flag_meanings",
     &      "calm_hour missing_hour"))
         do i = 1, numtyp
            if (outtyp(i) .eq. 'CONC') then
               call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &            data_varid(indgrp,indave,i), "standard_name",
     &            "concentration"))
            else if (outtyp(i) .eq. 'DEPOS') then
               call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &            data_varid(indgrp,indave,i), "standard_name",
     &            "total_deposition_flux"))
            else if (outtyp(i) .eq. 'DDEP') then
               call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &            data_varid(indgrp,indave,i), "standard_name",
     &            "dry_deposition_flux"))
            else if (outtyp(i) .eq. 'WDEP') then
               call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &            data_varid(indgrp,indave,i), "standard_name",
     &            "wet_deposition_flux"))
            else
               cycle
            end if
C           Set units (CONCUNIT, DEPOUNIT)
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         data_varid(indgrp,indave,i), "units", outlbl(i)))
C           Set cell_methods to indicate data may be averaged.
            call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &         data_varid(indgrp,indave,i), "cell_methods",
     &         "time: mean"))
         end do

C        Define global attributes.
         title_att = trim(adjustl(title1)) // new_line('a') //
     &               trim(adjustl(title2))
         versn_att = 'AERMOD (' // trim(adjustl(versn)) // ')'

         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      NF90_GLOBAL, "title", title_att))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      NF90_GLOBAL, "source", versn_att))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      NF90_GLOBAL, "options", trim(adjustl(MODOPS_String))))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      NF90_GLOBAL, "featureType", "timeSeries"))
         call nccheck(nf90_put_att(ipsunt(indgrp,indave),
     &      NF90_GLOBAL, "Conventions", "CF-1.6"))

C        End definition mode.
         call nccheck(nf90_enddef(ipsunt(indgrp,indave)))
     
C        Write receptor data.
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      rec_varid(indgrp,indave), (/ (irec, irec=1,numrec) /)))
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      x_varid(indgrp,indave), axr))
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      y_varid(indgrp,indave), ayr))
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      zelev_varid(indgrp,indave), azelev))
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      zhill_varid(indgrp,indave), azhill))
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      zflag_varid(indgrp,indave), azflag))
     
C        Write arc index by receptor.
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &      arc_varid(indgrp,indave), ndxarc(1:numrec)))
     
C        Write arc identifiers.
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &     arcid_varid(indgrp,indave), arcid(1:numarc)))
     
C        Write source groups.
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &     grp_varid(indgrp,indave), grpid(1:numgrp)))
     
C        Write averaging periods.
         call nccheck(nf90_put_var(ipsunt(indgrp,indave),
     &     ave_varid(indgrp,indave), kave(1:numave)))
      end if
#else
      write(*,*) ' NETCDF POSTFILE Option Not Supported! Aborting.'
      stop
#endif

      end subroutine ncsetup


      subroutine ncreset
C***********************************************************************
C                 NCRESET
C
C        PURPOSE: Set Variables for Previously Found netCDF POSTFILE
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    September 29, 2018
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   OUPOST
C***********************************************************************
#ifdef ENABLE_NETCDF
      use main1
      implicit none

      integer :: i, j

C     Set netCDF IPSUNT, VARIDs and DIMIDs to previously found values.
C     This allows multiple POSTFILE cards to reference the same file.

      do j = 1, numave
         do i = 1, numgrp
            if (pstfil(indgrp,indave) .eq. pstfil(i,j) .and.
     &          ipsfrm(indgrp,indave) .eq. ipsfrm(i,j) .and.
     &          ipsfrm(indgrp,indave) .eq. 2) then
C              Copy NCID.
               ipsunt(indgrp,indave) = ipsunt(i,j)

C              Copy dimension IDs.
               rec_dimid(indgrp,indave) = rec_dimid(i,j)
               arc_dimid(indgrp,indave) = arc_dimid(i,j)
               grp_dimid(indgrp,indave) = grp_dimid(i,j)
               ave_dimid(indgrp,indave) = ave_dimid(i,j)
               time_dimid(indgrp,indave) = time_dimid(i,j)
               strlen_dimid(indgrp,indave) = strlen_dimid(i,j)

C              Copy variable IDs.
               x_varid(indgrp,indave) = x_varid(i,j)
               y_varid(indgrp,indave) = y_varid(i,j)
               zelev_varid(indgrp,indave) = zelev_varid(i,j)
               zhill_varid(indgrp,indave) = zhill_varid(i,j)
               zflag_varid(indgrp,indave) = zflag_varid(i,j)
               rec_varid(indgrp,indave) = rec_varid(i,j)
               arc_varid(indgrp,indave) = arc_varid(i,j)
               grp_varid(indgrp,indave) = grp_varid(i,j)
               ave_varid(indgrp,indave) = ave_varid(i,j)
               time_varid(indgrp,indave) = time_varid(i,j)
               clmsg_varid(indgrp,indave) = clmsg_varid(i,j)
               data_varid(indgrp,indave,:) = data_varid(i,j,:)
            end if
         end do
      end do
#endif

      end subroutine ncreset


      subroutine ncwrite
C***********************************************************************
C                 NCWRITE
C
C        PURPOSE: Write Concurrent Values to File for Postprocessing
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    September 29, 2018
C
C        INPUTS:  Postprocessing File Options
C                 Array of CONC or DEPOS Averages
C
C        OUTPUTS: Postprocessor Files
C
C        CALLED FROM:   POSTFL
C***********************************************************************
#ifdef ENABLE_NETCDF
      use main1
      use netcdf
      implicit none

      integer :: time_index, i
      integer, dimension(4) :: aveval_start, aveval_count

C     Set time units attribute on first call to this subroutine, after
C     the date variables have been initialized.
      if (l_timeinit .eq. .false.) THEN
C        Ensure that we are in definition mode.
         call nccheck(nf90_redef(ipsunt(igrp,iave)))

C        Write time units using UDUNITS convention.
C        This is required for CF compliance.
         write(time_units_att,
     &         '(a12,i4.4,"-",i2.2,"-",i2.2,x,i2.2,a6)')
     &         'hours since ', iyr, imonth, iday, ihour-1, ':00:00'
         call nccheck(nf90_put_att(ipsunt(igrp,iave),
     &      time_varid(igrp,iave), "units", time_units_att))

C        End definition mode.
         call nccheck(nf90_enddef(ipsunt(igrp,iave)))

C        Store initial time for offset calculations.
         init_jday = jday
         init_iyr = iyr
         l_timeinit = .true.
      end if

C     Calculate current hour index as offset from initial time.
      time_value = 24*(jday-init_jday) + ihour-1
      if (iyr .ne. init_iyr) then
         do i = init_iyr, iyr
            if ((mod(i,4) .ne. 0) .or.
     &          (mod(i,100) .eq. 0 .and.
     &           mod(i,400) .ne. 0)) then
C              Standard year
               time_value = time_value + 24*365*(i-init_iyr)
            else
C              Leap year
               time_value = time_value + 24*366*(i-init_iyr)
            end if
         end do
      end if

C     Determine the current time index.
      call nccheck(nf90_inquire_dimension(ipsunt(igrp,iave),
     &   time_dimid(igrp,iave), len=time_index))

C     If time changed, increment index and write current hour.
      if (time_value .gt. prev_time_value) then
         time_index = time_index + 1
         call nccheck(nf90_put_var(ipsunt(igrp,iave),
     &      time_varid(igrp,iave), time_value,
     &      start = (/ time_index /)))
      end if

C     Write AVEVAL array.
C     netCDF Indexes: [TIME, IREC, IGRP, IAVE]
C     AVEVAL Indexes: [IREC, IGRP, IAVE, ITYP]
      do ityp = 1, numtyp
         aveval_start = (/ time_index, 1, igrp, iave /)
         aveval_count = (/ 1, numrec, 1, 1 /)
         call nccheck(nf90_put_var(ipsunt(igrp,iave),
     &      data_varid(igrp,iave,ityp),
     &      aveval(1:numrec,igrp,iave,ityp),
     &      start = aveval_start, count = aveval_count))
      end do

C     Write calm and missing flags.
      if (clmhr) then
         call nccheck(nf90_put_var(ipsunt(igrp,iave),
     &      clmsg_varid(igrp,iave), 1,
     &      start = (/ time_index /)))
      else if (msghr) then
         call nccheck(nf90_put_var(ipsunt(igrp,iave),
     &      clmsg_varid(igrp,iave), 2,
     &      start = (/ time_index /)))
      else
         call nccheck(nf90_put_var(ipsunt(igrp,iave),
     &      clmsg_varid(igrp,iave), 0,
     &      start = (/ time_index /)))
      end if

C     Set previous time index to current.
      prev_time_value = time_value
#endif

      end subroutine ncwrite


      subroutine nccheck(rc)
C***********************************************************************
C                 NCCHECK Module
C
C        PURPOSE: Error Handling Procedure for netCDF
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    March 8, 2018
C
C        INPUTS:  Error Code
C
C        OUTPUTS: Error Message
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
#ifdef ENABLE_NETCDF
      use main1, only: iounit
      use netcdf
      implicit none

      integer, intent(in) :: rc

C     Print a string representation of the netCDF error code to terminal.
C     AERMOD ERRHDL subroutine could be used if moved to module MAIN1.

      if(rc /= NF90_NOERR) then
         write(*,*) trim(nf90_strerror(rc))
         stop
      end if
#endif

      end subroutine nccheck

      end module ncpost
