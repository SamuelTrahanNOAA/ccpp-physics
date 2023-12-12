module cires_tauamf_data

  use machine, only: kind_phys
!...........................................................................................
! tabulated GW-sources: GRACILE/Ern et al., 2018 and/or Resolved GWs from C384-Annual run
!...........................................................................................
implicit none

   integer           :: ntau_d1y, ntau_d2t  
   real(kind=kind_phys), pointer :: ugwp_taulat(:) => null()
   real(kind=kind_phys), pointer :: tau_limb(:,:) => null()
   real(kind=kind_phys), pointer :: days_limb(:) => null()
   logical           :: flag_alloctau = .false.          
   character(len=255):: ugwp_taufile =  'ugwp_limb_tau.nc' 

   public :: read_tau_amf, cires_indx_ugwp, tau_amf_interp 

contains
  
 
   logical function netcdf_check(status, errmsg, errflg, why)
     use netcdf
     implicit none
     character(len=*), intent(inout) :: errmsg
     integer, intent(out) :: errflg
     integer, intent(in) :: status
     character(len=*), intent(in) :: why

     netcdf_check = (status == NF90_NOERR)

     if(netcdf_check) then
       errflg = 0
       errmsg = ' '
     else
       errflg = 1
       errmsg = trim(why) // ': ' // trim(nf90_strerror(status))
     endif
   end function netcdf_check

   subroutine read_tau_amf(me, master, errmsg, errflg)

     use netcdf
     implicit none
     integer, intent(in) ::  me, master   
     integer :: ncid,  iernc, vid, dimid, status         
     integer :: k

     real(kind_phys), allocatable :: local_ugwp_taulat(:), local_tau_limb(:,:), local_days_limb(:)

     character(len=*), intent(out) :: errmsg
     integer,          intent(out) :: errflg    
     !
     write(0,*) 'read_tau_amf'      

     ntau_d1y = 0
     ntau_d2t = 0
     ncid = -1
     DimID = -1
     vid=-1

     errflg = 0
     errmsg = ' '

     if(.not. netcdf_check(NF90_OPEN(trim(ugwp_taufile), nf90_nowrite, ncid), &
          errmsg, errflg, 'open "'//trim(ugwp_taufile)//'"')) then
       return
     endif

     if(.not.netcdf_check(nf90_inq_dimid(ncid, "lat", DimID), &
          errmsg, errflg, 'find id of lat dimension')) then
       return
     endif

     if(.not.netcdf_check(nf90_inquire_dimension(ncid, DimID,  len =ntau_d1y ), &
          errmsg, errflg, 'find length of lat dimension')) then
       return
     endif

     if(.not.netcdf_check(nf90_inq_dimid(ncid, "days", DimID), &
          errmsg, errflg, 'find id of days dimension')) then
       return
     endif
     if(.not.netcdf_check(nf90_inquire_dimension(ncid, DimID,  len =ntau_d2t ), &
          errmsg, errflg, 'find length of days dimension')) then
       return
     endif

     if (me == master)  print *, ntau_d1y, ntau_d2t, ' dimd of tau_ngw ugwp-v1 '
     if (ntau_d2t .le. 0 .or. ntau_d1y .le. 0) then 
       print *, 'ugwp-v1 tau-file=',    trim(ugwp_taufile)	   
       print *, '  ugwp-v1: ', 'ntau_d2t=',ntau_d2t, 'ntau_d2t=',ntau_d1y
       stop
     endif

     allocate(local_ugwp_taulat(ntau_d1y))
     allocate(local_days_limb(ntau_d2t))
     allocate(local_tau_limb(ntau_d1y, ntau_d2t))

     call free_globals

     allocate(ugwp_taulat(ntau_d1y))
     allocate(days_limb(ntau_d2t))
     allocate(tau_limb(ntau_d1y, ntau_d2t))

     if(.not.netcdf_check(nf90_inq_varid( ncid, 'DAYS', vid ), &
          errmsg, errflg, 'find id of DAYS variable')) then
       call cleanup
       return
     endif
     if(.not.netcdf_check(nf90_get_var( ncid, vid, local_days_limb, start=(/1/), count=(/ntau_d2t/)), &
          errmsg, errflg, 'read DAYS variable')) then
       call cleanup
       return
     endif
     days_limb = local_days_limb

     if(.not.netcdf_check(nf90_inq_varid( ncid, 'LATS', vid ), &
          errmsg, errflg, 'find LATS variable')) then
       call cleanup
       return
     endif
     if(.not.netcdf_check(nf90_get_var( ncid, vid, local_ugwp_taulat, start=(/1/), count=(/ntau_d1y/)), &
          errmsg, errflg, 'read LATS variable')) then
       call cleanup
       return
     endif
     ugwp_taulat = local_ugwp_taulat

     if(.not.netcdf_check(nf90_inq_varid( ncid, 'ABSMF', vid ), &
          errmsg, errflg, 'findi ABSMF variable')) then
       call cleanup
       return
     endif
     if(.not.netcdf_check(nf90_get_var( ncid, vid, local_tau_limb, start=(/1,1/), count=(/ntau_d1y, ntau_d2t/)), &
          errmsg, errflg, 'read ABSMF variable')) then
       call cleanup
       return
     endif
     tau_limb = local_tau_limb

     if(.not.netcdf_check(nf90_close(ncid), &
          errmsg, errflg, 'close tau amf file')) then
       call cleanup
       return
     endif

     call free_locals

   contains

     subroutine free_locals
       deallocate(local_ugwp_taulat)
       deallocate(local_days_limb)
       deallocate(local_tau_limb)
     end subroutine free_locals

     subroutine free_globals
       if(associated(ugwp_taulat)) then
         deallocate(ugwp_taulat)
         nullify(ugwp_taulat)
       endif

       if(associated(days_limb)) then
         deallocate(days_limb)
         nullify(days_limb)
       endif

       if(associated(tau_limb)) then
         deallocate(tau_limb)
         nullify(tau_limb)
       endif
     end subroutine free_globals

     subroutine cleanup
       call free_locals
       call free_globals
     end subroutine cleanup

   end subroutine read_tau_amf
  
    subroutine cires_indx_ugwp (npts, me, master, dlat,j1_tau,j2_tau, w1_j1tau, w2_j2tau, errmsg, errflg)
     
    use machine, only: kind_phys
    		 
    implicit none
    
      integer, intent(in)                                      ::   npts, me, master
      real(kind=kind_phys) ,   dimension(:), intent(in)     ::   dlat 
           
      integer, dimension(:), intent(inout)                  ::  j1_tau,   j2_tau
      real(kind=kind_phys) ,   dimension(:), intent(inout)  ::  w1_j1tau, w2_j2tau

      character(*), intent(out) :: errmsg
      integer, intent(out) :: errflg
      
!locals

      integer :: i,j, j1, j2     
!     

      errmsg = ' '
      errflg = 0

    if(ntau_d1y /= 73) then
      errmsg = 'corrupted ntau_d1y (lat) dimension'
      errflg = 1
      return
    endif

      do j=1,npts
        j2_tau(j) = ntau_d1y
        do i=1,ntau_d1y
          if (dlat(j) < ugwp_taulat(i)) then
            j2_tau(j) = i
            exit
          endif
        enddo
	
     
        j2_tau(j) = max(1,min(j2_tau(j),ntau_d1y))
        j1_tau(j) = max(1,min(j2_tau(j)-1,ntau_d1y))
	
        if (j1_tau(j) /= j2_tau(j) ) then
          w2_j2tau(j) = (dlat(j)  - ugwp_taulat(j1_tau(j))) &
                 / (ugwp_taulat(j2_tau(j))-ugwp_taulat(j1_tau(j)))       	 
        else
          w2_j2tau(j) = 1.0
        endif
          w1_j1tau(j) = 1.0 -	w2_j2tau(j)	
      enddo
      return
    end subroutine cires_indx_ugwp   
    
    subroutine tau_amf_interp(me, master, im, idate, fhour, j1_tau,j2_tau, ddy_j1, ddy_j2, tau_ddd, errmsg, errflg)
    use machine, only: kind_phys	           
    implicit none
    
!input    
    integer, intent(in)               :: me, master
    integer, intent(in)               :: im, idate(4)
    real(kind=kind_phys), intent(in)  :: fhour
      
    real(kind=kind_phys), intent(in), dimension(:) ::  ddy_j1, ddy_j2
    integer             , intent(in), dimension(:) ::  j1_tau,j2_tau        
!ouput    
    real(kind=kind_phys), intent(out), dimension(:)  ::  tau_ddd
    integer, intent(out) :: errflg
    character(*), intent(out) :: errmsg
!locals

    integer :: i, j1, j2, it1, it2 , iday
    integer :: ddd    
    real(kind=kind_phys)  :: tx1, tx2, w1, w2, fddd 
!
! define day of year ddd ..... from the old-fashioned "GFS-style"
! 
    ddd = 1e9
    fddd = 1e9

    errmsg = ' '
    errflg = 0

    if(ntau_d2t /= 14) then
      errmsg = 'corrupted ntau_d2t (days) dimension'
      errflg = 1
      return
    endif

         call gfs_idate_calendar(idate, fhour, ddd, fddd)  

         it2 = 1e9
         it1 = 2
         do iday=1, ntau_d2t
	    if (fddd .lt. days_limb(iday) ) then
	    it2 = iday
	    exit
	    endif
	 enddo
	 
	 if (it2 > ntau_d2t ) then
          errmsg = 'time index out of bounds in tau_amf_interp cires_tauamf_data.F90'
          errflg = 1
	  write(0,*) ' Error in time-interpolation for tau_amf_interp '	 
	  write(0,*) ' it1, it2, ntau_d2t ', it1, it2, ntau_d2t
	  write(0,*) ' Error in time-interpolation see cires_tauamf_data.F90 '	  
	  return
	 endif

	 it2 = min(it2,ntau_d2t)	 
	 it1 = max(it2-1,1)
	 
	 w2 = (fddd-days_limb(it1))/(days_limb(it2)-days_limb(it1))
	 w1 = 1.0-w2     
       
      do i=1, im	 
	 j1 = j1_tau(i)
	 j2 = j2_tau(i)
	 tx1 = tau_limb(j1, it1)*ddy_j1(i)+tau_limb(j2, it1)*ddy_j2(i)
	 tx2 = tau_limb(j1, it2)*ddy_j1(i)+tau_limb(j2, it2)*ddy_j2(i)	 
	 tau_ddd(i) =  tx1*w1 + w2*tx2
      enddo
             
    end subroutine tau_amf_interp  
    
    subroutine gfs_idate_calendar(idate, fhour, ddd, fddd) 
    
    use machine, only: kind_phys    		 
    implicit none  
! input     
    integer, intent(in)                 :: idate(4)
    real(kind=kind_phys), intent(in)   :: fhour
!out    
    integer, intent(out)                :: ddd    
    real(kind=kind_phys), intent(out)  :: fddd  
!
!locals
!
      real(kind=kind_phys) :: rjday
      integer              :: jdow, jdoy, jday
      real(8)              :: rinc(5)
      real(4)              :: rinc4(5)
      integer              :: w3kindreal, w3kindint
      
      integer ::  iw3jdn
      integer :: jd1, jddd
      
      integer  idat(8),jdat(8)  
          
       
      idat(1:8)    = 0
      idat(1) = idate(4)
      idat(2) = idate(2)
      idat(3) = idate(3)
      idat(5) = idate(1)
      rinc(1:5)    = 0.
      rinc(2) = fhour
!    
      call w3kind(w3kindreal,w3kindint)
      if(w3kindreal==4) then
        rinc4 = rinc
        call w3movdat(rinc4, idat,jdat)
      else
        call w3movdat(rinc,  idat,jdat)
      endif           
!     jdate(8)- date and time (yr, mo, day, [tz], hr, min, sec)
      jdow = 0
      jdoy = 0
      jday = 0
      call w3doxdat(jdat,jdow, ddd, jday)
      fddd = float(ddd) + jdat(5) / 24.        
    end  subroutine gfs_idate_calendar    
    
end  module cires_tauamf_data
