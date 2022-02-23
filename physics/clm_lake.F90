!> \file clm_lake.F90
!!  Contains code related to the CLM lake model

!> This module contains the clm lake model
MODULE clm_lake

  ! This lake scheme was taken from module_sf_lake in WRF 4.3.1. The original documentation said:
  !
  ! The lake scheme was retrieved from the Community Land Model version 4.5 
  ! (Oleson et al. 2013) with some modifications by Gu et al. (2013). It is a 
  ! one-dimensional mass and energy balance scheme with 20-25 model layers, 
  ! including up to 5 snow layers on the lake ice, 10 water layers, and 10 soil 
  ! layers on the lake bottom. The lake scheme is used with actual lake points and 
  ! lake depth derived from the WPS, and it also can be used with user defined 
  ! lake points and lake depth in WRF (lake_min_elev and lakedepth_default). 
  ! The lake scheme is independent of a land surface scheme and therefore 
  ! can be used with any land surface scheme embedded in WRF. The lake scheme 
  ! developments and evaluations were included in Subin et al. (2012) and Gu et al. (2013) 
  !
  !   Subin et al. 2012: Improved lake model for climate simulations, J. Adv. Model. 
  !
  !   Earth Syst., 4, M02001. DOI:10.1029/2011MS000072; 
  !
  !   Gu et al. 2013: Calibration and validation of lake surface temperature simulations 
  !
  !   with the coupled WRF-Lake model. Climatic Change, 1-13, 10.1007/s10584-013-0978-y. 
  
      use machine,               only: kind_phys

    implicit none 

    integer, parameter :: nlevsoil     =  10   ! number of soil layers
    integer, parameter :: nlevlake     =  10   ! number of lake layers
    integer, parameter :: nlevsnow     =   5   ! maximum number of snow layers

    integer,parameter  ::     lbp = 1                        ! pft-index bounds
    integer,parameter  ::     ubp = 1
    integer,parameter  ::     lbc = 1                        ! column-index bounds
    integer,parameter  ::     ubc = 1
    integer,parameter  ::     num_shlakec       = 1          ! number of columns in lake filter
    integer,parameter  ::     filter_shlakec(1) = 1          ! lake filter (columns)
    integer,parameter  ::     num_shlakep       = 1          ! number of pfts in lake filter
    integer,parameter  ::     filter_shlakep(1) = 1          ! lake filter (pfts)
    integer,parameter  ::     pcolumn(1)        = 1  
    integer,parameter  ::     pgridcell(1)      = 1  
    integer,parameter  ::     cgridcell(1)      = 1          ! gridcell index of column
    integer,parameter  ::     clandunit(1)      = 1          ! landunit index of column
  
    integer,parameter  ::     begg = 1
    integer,parameter  ::     endg = 1
    integer,parameter  ::     begl = 1
    integer,parameter  ::     endl = 1
    integer,parameter  ::     begc = 1
    integer,parameter  ::     endc = 1
    integer,parameter  ::     begp = 1
    integer,parameter  ::     endp = 1

    integer,parameter  ::     column    =1
    logical,parameter  ::     lakpoi(1) = .true.
   



    !Initialize physical constants:
    real(kind_phys), parameter :: vkc    = 0.4_kind_phys       !von Karman constant [-]
    real(kind_phys), parameter :: pie    = 3.141592653589793_kind_phys ! pi
    real(kind_phys), parameter :: grav   = 9.80616_kind_phys   !gravity constant [m/s2]
    real(kind_phys), parameter :: sb     = 5.67e-8_kind_phys   !stefan-boltzmann constant  [W/m2/K4]
    real(kind_phys), parameter :: tfrz   = 273.16_kind_phys    !freezing temperature [K]
    real(kind_phys), parameter :: denh2o = 1.000e3_kind_phys   !density of liquid water [kg/m3]
    real(kind_phys), parameter :: denice = 0.917e3_kind_phys   !density of ice [kg/m3]
    real(kind_phys), parameter :: cpice  = 2.11727e3_kind_phys !Specific heat of ice [J/kg-K]
    real(kind_phys), parameter :: cpliq  = 4.188e3_kind_phys   !Specific heat of water [J/kg-K]
    real(kind_phys), parameter :: hfus   = 3.337e5_kind_phys   !Latent heat of fusion for ice [J/kg]
    real(kind_phys), parameter :: hvap   = 2.501e6_kind_phys   !Latent heat of evap for water [J/kg]
    real(kind_phys), parameter :: hsub   = 2.501e6_kind_phys+3.337e5_kind_phys !Latent heat of sublimation    [J/kg]
    real(kind_phys), parameter :: rair   = 287.0423_kind_phys  !gas constant for dry air [J/kg/K]
    real(kind_phys), parameter :: cpair  = 1.00464e3_kind_phys !specific heat of dry air [J/kg/K]
    real(kind_phys), parameter :: tcrit  = 2.5          !critical temperature to determine rain or snow
    real(kind_phys), parameter :: tkwat  = 0.6          !thermal conductivity of water [W/m/k]
    real(kind_phys), parameter :: tkice  = 2.290        !thermal conductivity of ice   [W/m/k]
    real(kind_phys), parameter :: tkairc = 0.023        !thermal conductivity of air   [W/m/k]
    real(kind_phys), parameter :: bdsno = 250.            !bulk density snow (kg/m**3)
    
    real(kind_phys), public, parameter :: spval = 1.e36  !special value for missing data (ocean)

    real(kind_phys), parameter  ::     depth_c = 50.          ! below the level t_lake3d will be 277.0  !mchen

    
   ! These are tunable constants
    real(kind_phys), parameter :: wimp   = 0.05    !Water impremeable if porosity less than wimp
    real(kind_phys), parameter :: ssi    = 0.033   !Irreducible water saturation of snow
    real(kind_phys), parameter :: cnfac  = 0.5     !Crank Nicholson factor between 0 and 1


   ! Initialize water type constants
    integer,parameter :: istsoil = 1  !soil         "water" type
    integer, private  :: i  ! loop index 
    real(kind_phys) :: dtime                                    ! land model time step (sec)

    real(kind_phys) :: zlak(1:nlevlake)     !lake z  (layers)
    real(kind_phys) :: dzlak(1:nlevlake)    !lake dz (thickness)
    real(kind_phys) :: zsoi(1:nlevsoil)     !soil z  (layers)
    real(kind_phys) :: dzsoi(1:nlevsoil)    !soil dz (thickness)
    real(kind_phys) :: zisoi(0:nlevsoil)    !soil zi (interfaces)  


    real(kind_phys) :: sand(19)                           ! percent sand
    real(kind_phys) :: clay(19)                           ! percent clay

    data(sand(i), i=1,19)/92.,80.,66.,20.,5.,43.,60.,&
      10.,32.,51., 6.,22.,39.7,0.,100.,54.,17.,100.,92./

    data(clay(i), i=1,19)/ 3., 5.,10.,15.,5.,18.,27.,&
      33.,33.,41.,47.,58.,14.7,0., 0., 8.5,54.,  0., 3./


  !  real(kind_phys) :: dtime                  ! land model time step (sec)
    real(kind_phys) :: watsat(1,nlevsoil)      ! volumetric soil water at saturation (porosity)
    real(kind_phys) :: tksatu(1,nlevsoil)      ! thermal conductivity, saturated soil [W/m-K]
    real(kind_phys) :: tkmg(1,nlevsoil)        ! thermal conductivity, soil minerals  [W/m-K]
    real(kind_phys) :: tkdry(1,nlevsoil)       ! thermal conductivity, dry soil (W/m/Kelvin)
    real(kind_phys) :: csol(1,nlevsoil)        ! heat capacity, soil solids (J/m**3/Kelvin)
    CONTAINS
 

      !>\defgroup gfs_sice_main GFS Three-layer Thermodynomics Sea-Ice Scheme Module
      !> \section arg_table_clm_lake_finalize Argument Table
      !! \htmlinclude clm_lake_finalize.html
      !!
    SUBROUTINE clm_lake_finalize
    END SUBROUTINE clm_lake_finalize

      !>\defgroup gfs_sice_main GFS Three-layer Thermodynomics Sea-Ice Scheme Module
      !> \section arg_table_clm_lake_init Argument Table
      !! \htmlinclude clm_lake_init.html
      !!
    SUBROUTINE clm_lake_init
    END SUBROUTINE clm_lake_init

    !> \section arg_table_clm_lake_run Argument Table
    !! \htmlinclude clm_lake_run.html
    !!
    SUBROUTINE clm_lake_run( gt0,  prsi           ,con_rd,con_g ,qvcurr          ,&  !i
                     gu0          ,gv0            ,dlwsfci      ,emiss           ,&
                     rain         ,dtp            ,dswsfci      ,albedo          ,&
                     xlat         ,z_lake3d       ,dz_lake3d    ,lakedepth2d     ,&
                     watsat3d     ,csol3d         ,tkmg3d       ,tkdry3d         ,&
                     tksatu3d                     ,phii         ,xland           ,& 
                     iswater, xice, xice_threshold, lake_min_elev,im,km          ,&
                     h2osno2d     ,snowdp2d       ,snl2d        ,z3d             ,&  !h
                     dz3d         ,zi3d           ,h2osoi_vol3d ,h2osoi_liq3d    ,&
                     h2osoi_ice3d ,t_grnd2d       ,t_soisno3d   ,t_lake3d        ,&
                     savedtke12d  ,lake_icefrac3d ,oro          ,use_clm_lake      ,& 
!                    lakefrac                                                    ,&
                     con_cp       ,wet                                           ,&
                     hflx         ,evap           ,grdflx       ,tsk             ,&  !o
                     lake_t2m     ,lake_q2m       ,clm_lake_initialized          ,&
                     ivgtyp       ,isltyp         ,snow         ,use_lakedepth   ,&
                     restart      ,lakedepth_default, lake_ht   ,lake_rho0       ,&
                     xidx         ,yidx                                          ,&
                     me           ,master         ,errmsg       ,errflg )

      !==============================================================================
      ! This subroutine was first edited by Hongping Gu and Jiming Jin for coupling
      ! 07/20/2010
      !==============================================================================
    IMPLICIT NONE
    
    !in:
    
    INTEGER, INTENT(IN) :: iswater
    INTEGER, INTENT(OUT) :: errflg
    CHARACTER(*), INTENT(OUT) :: errmsg
    INTEGER , INTENT (IN) :: im,km,me,master
    LOGICAL, INTENT(IN) :: restart,use_lakedepth, wet(:)
    INTEGER, INTENT(INOUT) :: clm_lake_initialized(:)
    REAL(KIND_PHYS),     INTENT(IN)  :: xice_threshold, con_rd,con_g,con_cp,lakedepth_default
    REAL(KIND_PHYS), DIMENSION( : ), INTENT(IN)::   XICE
    REAL(KIND_PHYS), DIMENSION( : ), INTENT(IN):: ORO
!   REAL(KIND_PHYS), DIMENSION( : ), INTENT(INOUT)::   LAKEFRAC
 !   INTEGER, INTENT(IN)::   LAKEFLAG
    REAL(KIND_PHYS),    DIMENSION( : ), INTENT(IN)    :: SNOW

    LOGICAL, DIMENSION(:), INTENT(IN) :: use_clm_lake
    
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: gt0
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: prsi   
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: phii
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: qvcurr
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: gu0
    REAL(KIND_PHYS),           DIMENSION( :, : ),INTENT(IN)  :: gv0
    REAL(KIND_PHYS),           DIMENSION( : ),   INTENT(IN)  :: xlat
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(IN)  :: dlwsfci
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(IN)  :: dswsfci
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(IN)  :: emiss
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(IN)  :: rain
    !REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(IN)  :: swdown
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: albedo
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: lake_ht
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: lake_rho0
    INTEGER,                   DIMENSION( : )         ,INTENT(IN) :: XLAND ! FIXME: REMOVE
    INTEGER, DIMENSION( : ), INTENT(IN)       :: IVGTYP ! FIXME: REMOVE
    INTEGER, DIMENSION( : ), INTENT(IN)       :: ISLTYP ! FIXME: INTENT(IN)
    REAL(KIND_PHYS),                                                  INTENT(IN)  :: dtp
    integer, intent(in) :: xidx(:), yidx(:)
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: z_lake3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: dz_lake3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: watsat3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: csol3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: tkmg3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: tkdry3d
    REAL(KIND_PHYS),           DIMENSION( :,: ),INTENT(INOUT)  :: tksatu3d
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: lakedepth2d    
    REAL(KIND_PHYS),                                   INTENT(IN)  :: lake_min_elev

    !feedback to atmosphere:
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: hflx
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: evap
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: GRDFLX
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: TSK
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: lake_t2m
    REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(OUT) :: lake_q2m

    !in&out:

    real(kind_phys),           dimension(: )                ,intent(inout)  :: savedtke12d 
    real(kind_phys),           dimension(: )                ,intent(inout)  :: snowdp2d,       &    
                                                                                  h2osno2d,       &    
                                                                                  snl2d,          &    
                                                                                  t_grnd2d
    
    real(kind_phys),    dimension( :,: )           ,INTENT(inout)  :: t_lake3d,       &    
                                                                                  lake_icefrac3d
    real(kind_phys),    dimension( :,-nlevsnow+1: )  ,INTENT(inout)  :: t_soisno3d,     &    
                                                                                  h2osoi_ice3d,   &    
                                                                                  h2osoi_liq3d,   &    
                                                                                  h2osoi_vol3d,   &    
                                                                                  z3d,            &    
                                                                                  dz3d 
    real(kind_phys),    dimension( :,-nlevsnow+0: )  ,INTENT(inout)  :: zi3d    
       

    !local variable:

    REAL(kind_phys)     :: SFCTMP,PBOT,PSFC,ZLVL,Q2K,LWDN,PRCP,SOLDN,SOLNET
    INTEGER  :: C,i,j,k


      !tempory varibles in:
      real(kind_phys)  :: forc_t(1)          ! atmospheric temperature (Kelvin)
      real(kind_phys)  :: forc_pbot(1)       ! atm bottom level pressure (Pa) 
      real(kind_phys)  :: forc_psrf(1)       ! atmospheric surface pressure (Pa)
      real(kind_phys)  :: forc_hgt(1)        ! atmospheric reference height (m)
      real(kind_phys)  :: forc_hgt_q(1)      ! observational height of humidity [m]
      real(kind_phys)  :: forc_hgt_t(1)      ! observational height of temperature [m]
      real(kind_phys)  :: forc_hgt_u(1)      ! observational height of wind [m]
      real(kind_phys)  :: forc_q(1)          ! atmospheric specific humidity (kg/kg)
      real(kind_phys)  :: forc_u(1)          ! atmospheric wind speed in east direction (m/s)
      real(kind_phys)  :: forc_v(1)          ! atmospheric wind speed in north direction (m/s)
     ! real(kind_phys)  :: forc_rho(1)        ! density (kg/m**3)
      real(kind_phys)  :: forc_lwrad(1)      ! downward infrared (longwave) radiation (W/m**2)
      real(kind_phys)  :: prec(1)               ! snow or rain rate [mm/s]
      real(kind_phys)  :: sabg(1)            ! solar radiation absorbed by ground (W/m**2)
      real(kind_phys)  :: lat(1)             ! latitude (radians)
      real(kind_phys)  :: z_lake(1,nlevlake)  ! layer depth for lake (m)
      real(kind_phys)  :: dz_lake(1,nlevlake)                  ! layer thickness for lake (m)

      real(kind_phys)  :: lakedepth(1)       ! column lake depth (m)
      logical   :: do_capsnow(1)     ! true => do snow capping

      !in&out
      real(kind_phys)  :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  ! volumetric soil water (0<=h2osoi_vol<=watsat)[m3/m3]
      real(kind_phys)  :: t_grnd(1)          ! ground temperature (Kelvin)
      real(kind_phys)  :: h2osno(1)          ! snow water (mm H2O)
      real(kind_phys)  :: snowdp(1)          ! snow height (m)
      real(kind_phys)  :: z(1,-nlevsnow+1:nlevsoil)             ! layer depth for snow & soil (m)
      real(kind_phys)  :: dz(1,-nlevsnow+1:nlevsoil)            ! layer thickness for soil or snow (m)
      real(kind_phys)  :: t_soisno(1,-nlevsnow+1:nlevsoil)      ! soil (or snow) temperature (Kelvin)
      real(kind_phys)  :: t_lake(1,nlevlake)                   ! lake temperature (Kelvin)
      integer   :: snl(1)                              ! number of snow layers
      real(kind_phys)  :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    ! liquid water (kg/m2)
      real(kind_phys)  :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    ! ice lens (kg/m2)
      real(kind_phys)  :: savedtke1(1)       ! top level eddy conductivity from previous timestep (W/m.K)
      real(kind_phys)  :: zi(1,-nlevsnow+0:nlevsoil)            ! interface level below a "z" level (m)
      real(kind_phys)  :: lake_icefrac(1,nlevlake)  ! mass fraction of lake layer that is frozen


      !out:
      real(kind_phys)  :: eflx_gnet(1)       !net heat flux into ground (W/m**2)
      real(kind_phys)  :: eflx_lwrad_net(1)  ! net infrared (longwave) rad (W/m**2) [+ = to atm]
      real(kind_phys)  :: eflx_sh_tot(1)     ! total sensible heat flux (W/m**2) [+ to atm]
      real(kind_phys)  :: eflx_lh_tot(1)     ! total latent heat flux (W/m8*2)  [+ to atm]
      real(kind_phys)  :: t_ref2m(1)         ! 2 m height surface air temperature (Kelvin)
      real(kind_phys)  :: q_ref2m(1)         ! 2 m height surface specific humidity (kg/kg)
      real(kind_phys)  :: taux(1)            ! wind (shear) stress: e-w (kg/m/s**2)
      real(kind_phys)  :: tauy(1)            ! wind (shear) stress: n-s (kg/m/s**2)
      real(kind_phys)  :: ram1(1)            ! aerodynamical resistance (s/m)
                                               ! for calculation of decay of eddy diffusivity with depth
                                               ! Change the type variable to pass back to WRF.
      real(kind_phys)  :: z0mg(1)            ! roughness length over ground, momentum (m(
      real(kind_phys)  :: rho0               ! air density at surface
      real(kind_phys)  :: qfx                ! mass flux, old WRF qfx(:) variable, (kg/(sm^2))

#ifdef LAKE_DEBUG
      integer :: lake_points
#endif

      logical, parameter :: feedback_to_atmosphere = .false. ! FIXME: REMOVE


      errmsg = ' '
      errflg = 0

!     if(any(use_clm_lake .and. clm_lake_initialized==0)) then
        ! Still have some points to initialize
        call lakeini(IVGTYP,         ISLTYP,          gt0,             SNOW,           & !i
                     lake_min_elev,  restart,         lakedepth_default,               &
                     lakedepth2d,    savedtke12d,     snowdp2d,        h2osno2d,       & !o
                     snl2d,          t_grnd2d,        t_lake3d,        lake_icefrac3d, &
                     z_lake3d,       dz_lake3d,       t_soisno3d,      h2osoi_ice3d,   &
                     h2osoi_liq3d,   h2osoi_vol3d,    z3d,             dz3d,           &
                     zi3d,           watsat3d,        csol3d,          tkmg3d,         &
                     iswater,        xice,            xice_threshold,  xland,   tsk,   &
                     use_clm_lake,     use_lakedepth,   con_g,           con_rd,         &
                     tkdry3d,        tksatu3d,        im,              prsi,           &
                     lake_rho0,      lake_ht,         oro,             wet,            &
                     xidx,           yidx,            clm_lake_initialized,            &
                     me,             master,          errmsg,          errflg)
        if(errflg/=0) then
          return
        endif
!     endif

#ifdef LAKE_DEBUG
      lake_points=0
#endif

      dtime = dtp/2  ! Two surface scheme timesteps per physics timestep

        lake_top_loop: DO I = 1,im
           !        IF (XLAND(I).LT.1) THEN    

       !  if ( xice(i).gt.xice_threshold) then
       !   ivgtyp(i) = iswater
       !   xland(i) = 2
       !   lake_icefrac3d(i,1) = xice(i)
       !   endif

        if_lake_is_here: if (use_clm_lake(i)) THEN

           SFCTMP  = gt0(i,1)
           PBOT    = prsi(i,2)
           PSFC    = prsi(i,1) 
           ZLVL    = 0.5 * ((con_rd/con_g)*0.001) * abs(log(prsi(i,2)) - log(prsi(i,1))) * gt0(i,1)
           Q2K     = qvcurr(i,1)/(1.0 + qvcurr(i,1))
           LWDN    = DLWSFCI(I)*EMISS(I)
           PRCP    = RAIN(i)*1000.0_kind_phys/dtp      ! use physics timestep since PRCP comes from non-surface schemes
           SOLDN   = DSWSFCI(I)                        ! SOLDN is total incoming solar
           SOLNET  = SOLDN*(1.-ALBEDO(I))              ! use mid-day albedo to determine net downward solar
                                                       ! (no solar zenith angle correction) 

#ifdef LAKE_DEBUG
          lake_points = lake_points+1
#endif
           do c = 1,column
     
            forc_t(c)          = SFCTMP           ! [K]
            forc_pbot(c)       = PBOT             ! [Pa]
            forc_psrf(c)       = PSFC             ! [Pa]
            forc_hgt(c)        = ZLVL             ! [m]
            forc_hgt_q(c)      = ZLVL             ! [m]
            forc_hgt_t(c)      = ZLVL             ! [m]
            forc_hgt_u(c)      = ZLVL             ! [m]
            forc_q(c)          = Q2K              ! [kg/kg]
            forc_u(c)          = gu0(I,1)         ! [m/s]
            forc_v(c)          = gv0(I,1)         ! [m/s]
           ! forc_rho(c)        = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) ![kg/m/m/m] 
            forc_lwrad(c)      = LWDN             ! [W/m/m]
            prec(c)            = PRCP             ! [mm/s]
            sabg(c)            = SOLNET
            lat(c)             = XLAT(I)*pie/180  ! [radian] 
            do_capsnow(c)      = .false.

            lakedepth(c)           = lakedepth2d(i)
            savedtke1(c)           = savedtke12d(i)
            snowdp(c)              = snowdp2d(i)
            h2osno(c)              = h2osno2d(i)
            snl(c)                 = snl2d(i)
            t_grnd(c)              = t_grnd2d(i)
            do k = 1,nlevlake
               t_lake(c,k)        = t_lake3d(i,k)
               lake_icefrac(c,k)  = lake_icefrac3d(i,k)
               z_lake(c,k)        = z_lake3d(i,k)
               dz_lake(c,k)       = dz_lake3d(i,k)
            enddo
            do k = -nlevsnow+1,nlevsoil
               t_soisno(c,k)      = t_soisno3d(i,k)
	       h2osoi_ice(c,k)    = h2osoi_ice3d(i,k)
               h2osoi_liq(c,k)    = h2osoi_liq3d(i,k)
               h2osoi_vol(c,k)    = h2osoi_vol3d(i,k)
               z(c,k)             = z3d(i,k)
               dz(c,k)            = dz3d(i,k)
            enddo   
            do k = -nlevsnow+0,nlevsoil
               zi(c,k)            = zi3d(i,k)
            enddo
            do k = 1,nlevsoil
               watsat(c,k)        = watsat3d(i,k)
               csol(c,k)          = csol3d(i,k)
               tkmg(c,k)          = tkmg3d(i,k)
               tkdry(c,k)         = tkdry3d(i,k)
               tksatu(c,k)        = tksatu3d(i,k)
            enddo
            
          enddo
            CALL LakeMain(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,   & !I  
                          forc_hgt_t,forc_hgt_u,forc_q, forc_u,         &
                          forc_v,forc_lwrad,prec, sabg,lat,             &
                          z_lake,dz_lake,lakedepth,do_capsnow,          &
                          h2osno,snowdp,snl,z,dz,zi,                    & !H
                          h2osoi_vol,h2osoi_liq,h2osoi_ice,             &
                          t_grnd,t_soisno,t_lake,                       &
                          savedtke1,lake_icefrac,                       &
                          eflx_lwrad_net,eflx_gnet,                     & !O 
                          eflx_sh_tot,eflx_lh_tot,                      &
                          t_ref2m,q_ref2m,                              &
                          taux,tauy,ram1,z0mg,errmsg,errflg)
            if(errflg/=0) then
              return ! State is invalid now, so pass error to caller.
            endif
            
            if(feedback_to_atmosphere) then
              do c = 1,column
                rho0 = prsi(i,1)/(con_rd*gt0(i,1))
                HFLX(i)=eflx_sh_tot(i)/(rho0*con_cp)
                ! No equivalent in CCPP:
                ! LH(I)           = eflx_lh_tot(c)/rho1(i)    ![kg*m/(kg*s)]
                GRDFLX(I)       = eflx_gnet(c)              ![W/m/m]
                TSK(I)          = t_grnd(c)                 ![K]
                lake_t2m(I)     = t_ref2m(c)
                !TH2(I)          = T2(I)*(1.E5/PSFC)**RCP   ! potential temperature (CCPP doesn't want this)
                lake_q2m(I)     = q_ref2m(c)               ! [frac] specific humidity
                albedo(i)       = ( 0.6 * lake_icefrac(c,1) ) + ( (1.0-lake_icefrac(c,1)) * 0.08)  

                if( tsk(i) >= tfrz ) then
                  qfx         = eflx_lh_tot(c)/hvap
                else
                  qfx         = eflx_lh_tot(c)/hsub       ! heat flux (W/m^2)=>mass flux(kg/(sm^2))
                endif
                evap(i) = qfx/rho0
              enddo
            endif

           ! Renew Lake State Varialbes:(14)
           do c = 1,column

            savedtke12d(i)         = savedtke1(c)
            snowdp2d(i)            = snowdp(c)
            h2osno2d(i)            = h2osno(c)
	    snl2d(i)               = snl(c)
            t_grnd2d(i)            = t_grnd(c)
            do k = 1,nlevlake
               t_lake3d(i,k)       = t_lake(c,k)
	       lake_icefrac3d(i,k) = lake_icefrac(c,k)
            enddo
	    do k = -nlevsnow+1,nlevsoil
	       z3d(i,k)            = z(c,k)
	       dz3d(i,k)           = dz(c,k) 
	       t_soisno3d(i,k)     = t_soisno(c,k)
	       h2osoi_liq3d(i,k)   = h2osoi_liq(c,k)
	       h2osoi_ice3d(i,k)   = h2osoi_ice(c,k)
               h2osoi_vol3d(i,k)   = h2osoi_vol(c,k)
	   enddo
           do k = -nlevsnow+0,nlevsoil
               zi3d(i,k)           = zi(c,k)
           enddo
        
         enddo

        endif if_lake_is_here
        !        ENDIF    ! if xland = 0
        ENDDO lake_top_loop
#ifdef LAKE_DEBUG
        print *,'lake points: ',lake_points
#endif

    END SUBROUTINE clm_lake_run


    SUBROUTINE LakeMain(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,     & !I  
                          forc_hgt_t,forc_hgt_u,forc_q, forc_u,         &   
                          forc_v,forc_lwrad,prec, sabg,lat,             &   
                          z_lake,dz_lake,lakedepth,do_capsnow,          &
                          h2osno,snowdp,snl,z,dz,zi,                    & !H
                          h2osoi_vol,h2osoi_liq,h2osoi_ice,             &
                          t_grnd,t_soisno,t_lake,                       &  
                          savedtke1,lake_icefrac,                       &
                          eflx_lwrad_net,eflx_gnet,                     & !O 
                          eflx_sh_tot,eflx_lh_tot,                      &
                          t_ref2m,q_ref2m,                              &
                          taux,tauy,ram1,z0mg,errmsg,errflg)
    implicit none
    !in: 

    integer, intent(inout) :: errflg
    character(*), intent(inout) :: errmsg
    real(kind_phys),intent(in) :: forc_t(1)          ! atmospheric temperature (Kelvin)
    real(kind_phys),intent(in) :: forc_pbot(1)       ! atm bottom level pressure (Pa) 
    real(kind_phys),intent(in) :: forc_psrf(1)       ! atmospheric surface pressure (Pa)
    real(kind_phys),intent(in) :: forc_hgt(1)        ! atmospheric reference height (m)
    real(kind_phys),intent(in) :: forc_hgt_q(1)      ! observational height of humidity [m]
    real(kind_phys),intent(in) :: forc_hgt_t(1)      ! observational height of temperature [m]
    real(kind_phys),intent(in) :: forc_hgt_u(1)      ! observational height of wind [m]
    real(kind_phys),intent(in) :: forc_q(1)          ! atmospheric specific humidity (kg/kg)
    real(kind_phys),intent(in) :: forc_u(1)          ! atmospheric wind speed in east direction (m/s)
    real(kind_phys),intent(in) :: forc_v(1)          ! atmospheric wind speed in north direction (m/s)
   ! real(kind_phys),intent(in) :: forc_rho(1)        ! density (kg/m**3)
    real(kind_phys),intent(in) :: forc_lwrad(1)      ! downward infrared (longwave) radiation (W/m**2)
    real(kind_phys),intent(in) :: prec(1)               ! snow or rain rate [mm/s]
    real(kind_phys),intent(in) :: sabg(1)            ! solar radiation absorbed by ground (W/m**2)
    real(kind_phys),intent(in) :: lat(1)             ! latitude (radians)
    real(kind_phys),intent(in) :: z_lake(1,nlevlake)  ! layer depth for lake (m)
    real(kind_phys),intent(in) :: dz_lake(1,nlevlake)                  ! layer thickness for lake (m)

    real(kind_phys), intent(in) :: lakedepth(1)       ! column lake depth (m)
    !!!!!!!!!!!!!!!!tep(in),hydro(in)   
   ! real(kind_phys), intent(in) :: watsat(1,1:nlevsoil)      ! volumetric soil water at saturation (porosity)
    !!!!!!!!!!!!!!!!hydro
    logical , intent(in) :: do_capsnow(1)     ! true => do snow capping
   


    !in&out
    real(kind_phys),intent(inout) :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  ! volumetric soil water (0<=h2osoi_vol<=watsat)[m3/m3]
    real(kind_phys),intent(inout) :: t_grnd(1)          ! ground temperature (Kelvin)
    real(kind_phys),intent(inout) :: h2osno(1)          ! snow water (mm H2O)
    real(kind_phys),intent(inout) :: snowdp(1)          ! snow height (m)
    real(kind_phys),intent(inout) :: z(1,-nlevsnow+1:nlevsoil)             ! layer depth for snow & soil (m)
    real(kind_phys),intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)            ! layer thickness for soil or snow (m)
    real(kind_phys),intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)      ! soil (or snow) temperature (Kelvin)
    real(kind_phys),intent(inout) :: t_lake(1,nlevlake)                   ! lake temperature (Kelvin)
    integer ,intent(inout) :: snl(1)                              ! number of snow layers
    real(kind_phys),intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    ! liquid water (kg/m2)
    real(kind_phys),intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    ! ice lens (kg/m2)
    real(kind_phys),intent(inout) :: savedtke1(1)       ! top level eddy conductivity from previous timestep (W/m.K)
    real(kind_phys),intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)            ! interface level below a "z" level (m)
    real(kind_phys),intent(inout) :: lake_icefrac(1,nlevlake)  ! mass fraction of lake layer that is frozen


    !out:
    real(kind_phys),intent(out) :: eflx_gnet(1)       !net heat flux into ground (W/m**2)
    real(kind_phys),intent(out) :: eflx_lwrad_net(1)  ! net infrared (longwave) rad (W/m**2) [+ = to atm]
    real(kind_phys),intent(out) :: eflx_sh_tot(1)     ! total sensible heat flux (W/m**2) [+ to atm]
    real(kind_phys),intent(out) :: eflx_lh_tot(1)     ! total latent heat flux (W/m8*2)  [+ to atm]
    real(kind_phys),intent(out) :: t_ref2m(1)         ! 2 m height surface air temperature (Kelvin)
    real(kind_phys),intent(out) :: q_ref2m(1)         ! 2 m height surface specific humidity (kg/kg)
    real(kind_phys),intent(out) :: taux(1)            ! wind (shear) stress: e-w (kg/m/s**2)
    real(kind_phys),intent(out) :: tauy(1)            ! wind (shear) stress: n-s (kg/m/s**2)
    real(kind_phys),intent(out) :: ram1(1)            ! aerodynamical resistance (s/m)
                                               ! for calculation of decay of eddy diffusivity with depth
                                               ! Change the type variable to pass back to WRF.
    real(kind_phys),intent(out) :: z0mg(1)            ! roughness length over ground, momentum (m(


    !local output
    
    real(kind_phys) :: begwb(1)           ! water mass begining of the time step
    real(kind_phys) :: t_veg(1)           ! vegetation temperature (Kelvin)
    real(kind_phys) :: eflx_soil_grnd(1)  ! soil heat flux (W/m**2) [+ = into soil]
    real(kind_phys) :: eflx_lh_grnd(1)    ! ground evaporation heat flux (W/m**2) [+ to atm]
    real(kind_phys) :: eflx_sh_grnd(1)    ! sensible heat flux from ground (W/m**2) [+ to atm]
    real(kind_phys) :: eflx_lwrad_out(1)  ! emitted infrared (longwave) radiation (W/m**2)
    real(kind_phys) :: qflx_evap_tot(1)   ! qflx_evap_soi + qflx_evap_veg + qflx_tran_veg
    real(kind_phys) :: qflx_evap_soi(1)   ! soil evaporation (mm H2O/s) (+ = to atm)
    real(kind_phys) :: qflx_prec_grnd(1)  ! water onto ground including canopy runoff [kg/(m2 s)]
    real(kind_phys) :: forc_snow(1)       ! snow rate [mm/s]
    real(kind_phys) :: forc_rain(1)       ! rain rate [mm/s]
    real(kind_phys) :: ws(1)              ! surface friction velocity (m/s)
    real(kind_phys) :: ks(1)              ! coefficient passed to ShalLakeTemperature
    real(kind_phys) :: qflx_snomelt(1)    !snow melt (mm H2O /s) tem(out),snowwater(in)
    integer  :: imelt(1,-nlevsnow+1:nlevsoil)      !flag for melting (=1), freezing (=2), Not=0 (new)
    real(kind_phys) :: endwb(1)         ! water mass end of the time step
    real(kind_phys) :: snowage(1)       ! non dimensional snow age [-]
    real(kind_phys) :: snowice(1)       ! average snow ice lens
    real(kind_phys) :: snowliq(1)       ! average snow liquid water
    real(kind_phys) :: t_snow(1)        ! vertically averaged snow temperature
    real(kind_phys) :: qflx_drain(1)    ! sub-surface runoff (mm H2O /s)
    real(kind_phys) :: qflx_surf(1)     ! surface runoff (mm H2O /s)
    real(kind_phys) :: qflx_infl(1)     ! infiltration (mm H2O /s)
    real(kind_phys) :: qflx_qrgwl(1)    ! qflx_surf at glaciers, wetlands, lakes
    real(kind_phys) :: qcharge(1)       ! aquifer recharge rate (mm/s)
    real(kind_phys) :: qflx_snowcap(1)       ! excess precipitation due to snow capping (mm H2O /s) [+]
    real(kind_phys) :: qflx_snowcap_col(1)   ! excess precipitation due to snow capping (mm H2O /s) [+]
    real(kind_phys) :: qflx_snow_grnd_pft(1) ! snow on ground after interception (mm H2O/s) [+]
    real(kind_phys) :: qflx_snow_grnd_col(1) ! snow on ground after interception (mm H2O/s) [+]
    real(kind_phys) :: qflx_rain_grnd(1)     ! rain on ground after interception (mm H2O/s) [+]
    real(kind_phys) :: frac_iceold(1,-nlevsnow+1:nlevsoil)      ! fraction of ice relative to the tot water
    real(kind_phys) :: qflx_evap_tot_col(1) !pft quantity averaged to the column (assuming one pft)
    real(kind_phys) :: soilalpha(1)     !factor that reduces ground saturated specific humidity (-)
    real(kind_phys) :: zwt(1)           !water table depth
    real(kind_phys) :: fcov(1)          !fractional area with water table at surface
    real(kind_phys) :: rootr_column(1,1:nlevsoil) !effective fraction of roots in each soil layer
    real(kind_phys) :: qflx_evap_grnd(1)  ! ground surface evaporation rate (mm H2O/s) [+]
    real(kind_phys) :: qflx_sub_snow(1)   ! sublimation rate from snow pack (mm H2O /s) [+]
    real(kind_phys) :: qflx_dew_snow(1)   ! surface dew added to snow pack (mm H2O /s) [+]
    real(kind_phys) :: qflx_dew_grnd(1)   ! ground surface dew formation (mm H2O /s) [+]
    real(kind_phys) :: qflx_rain_grnd_col(1)   !rain on ground after interception (mm H2O/s) [+]
    begwb = 0    

    !    lat  = lat*pie/180  ! [radian]

    if (prec(1)> 0.) then
        if ( forc_t(1) > (tfrz + tcrit)) then
            forc_rain(1) = prec(1)
            forc_snow(1) = 0.
          !   flfall(1) = 1.
         else
            forc_rain(1) = 0.
            forc_snow(1) = prec(1)

          !  if ( forc_t(1) <= tfrz) then
          !      flfall(1) = 0.
          !  else if ( forc_t(1) <= tfrz+2.) then
          !      flfall(1) = -54.632 + 0.2 *  forc_t(1)
          !  else
          !      flfall(1) = 0.4
         endif
    else
         forc_rain(1) = 0.
         forc_snow(1) = 0.
       !  flfall(1) = 1.
    endif

    CALL ShalLakeFluxes(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,   &  !i
                          forc_hgt_t,forc_hgt_u,forc_q,                   &
                          forc_u,forc_v,forc_lwrad,forc_snow,             &
                          forc_rain,t_grnd,h2osno,snowdp,sabg,lat,        &
                          dz,dz_lake,t_soisno,t_lake,snl,h2osoi_liq,      &
                          h2osoi_ice,savedtke1,                           &
                          qflx_prec_grnd,qflx_evap_soi,qflx_evap_tot,     &  !o
                          eflx_sh_grnd,eflx_lwrad_out,eflx_lwrad_net,     &
                          eflx_soil_grnd,eflx_sh_tot,eflx_lh_tot,         &
                          eflx_lh_grnd,t_veg,t_ref2m,q_ref2m,taux,tauy,   &
                          ram1,ws,ks,eflx_gnet,z0mg,errmsg,errflg)
    if(errflg/=0) then
      return ! State is invalid now, so pass error to caller.
    endif

    CALL ShalLakeTemperature(t_grnd,h2osno,sabg,dz,dz_lake,z,zi,             & !i
                                 z_lake,ws,ks,snl,eflx_gnet,lakedepth,       &
                                 lake_icefrac,snowdp,                        & !i&o
                                 eflx_sh_grnd,eflx_sh_tot,eflx_soil_grnd,    & !o
                                 t_lake,t_soisno,h2osoi_liq,                 &
                                 h2osoi_ice,savedtke1,                       &
                                 frac_iceold,qflx_snomelt,imelt,errmsg,errflg)
    if(errflg/=0) then
      return ! State is invalid now, so pass error to caller.
    endif

    CALL ShalLakeHydrology(dz_lake,forc_rain,forc_snow,                          & !i
                               begwb,qflx_evap_tot,forc_t,do_capsnow,            &
                               t_grnd,qflx_evap_soi,                             &
                               qflx_snomelt,imelt,frac_iceold,                   & !i add by guhp
                               z,dz,zi,snl,h2osno,snowdp,lake_icefrac,t_lake,      & !i&o
                               endwb,snowage,snowice,snowliq,t_snow,             & !o
                               t_soisno,h2osoi_ice,h2osoi_liq,h2osoi_vol,        &
                               qflx_drain,qflx_surf,qflx_infl,qflx_qrgwl,        &
                               qcharge,qflx_prec_grnd,qflx_snowcap,              &
                               qflx_snowcap_col,qflx_snow_grnd_pft,              &
                               qflx_snow_grnd_col,qflx_rain_grnd,                &
                               qflx_evap_tot_col,soilalpha,zwt,fcov,             &
                               rootr_column,qflx_evap_grnd,qflx_sub_snow,        &
                               qflx_dew_snow,qflx_dew_grnd,qflx_rain_grnd_col,   &
                               errmsg,errflg)
    if(errflg/=0) then
      return ! State is invalid now, so pass error to caller.
    endif
                       
    !==================================================================================
    ! !DESCRIPTION:
    ! Calculation of Shallow Lake Hydrology. Full hydrology of snow layers is
    ! done. However, there is no infiltration, and the water budget is balanced with 
                       
   END SUBROUTINE LakeMain


SUBROUTINE ShalLakeFluxes(forc_t,forc_pbot,forc_psrf,forc_hgt,forc_hgt_q,           &  !i
                          forc_hgt_t,forc_hgt_u,forc_q,                   &
                          forc_u,forc_v,forc_lwrad,forc_snow,             &
                          forc_rain,t_grnd,h2osno,snowdp,sabg,lat,        &
                          dz,dz_lake,t_soisno,t_lake,snl,h2osoi_liq,      &
                          h2osoi_ice,savedtke1,                           &
                          qflx_prec_grnd,qflx_evap_soi,qflx_evap_tot,     &  !o
                          eflx_sh_grnd,eflx_lwrad_out,eflx_lwrad_net,     &
                          eflx_soil_grnd,eflx_sh_tot,eflx_lh_tot,         &
                          eflx_lh_grnd,t_veg,t_ref2m,q_ref2m,taux,tauy,   &
                          ram1,ws,ks,eflx_gnet,z0mg,errmsg,errflg)            
  !==============================================================================
  ! DESCRIPTION:
  ! Calculates lake temperatures and surface fluxes for shallow lakes.
  !
  ! Shallow lakes have variable depth, possible snow layers above, freezing & thawing of lake water,
  ! and soil layers with active temperature and gas diffusion below.
  !
  ! WARNING: This subroutine assumes lake columns have one and only one pft.
  !
  ! REVISION HISTORY:
  ! Created by Zack Subin, 2009
  ! Reedited by Hongping Gu, 2010 
  !==============================================================================
  
   ! implicit none
 
    implicit none

    !in: 

    integer, intent(inout) :: errflg
    character(len=*), intent(inout) :: errmsg
    real(kind_phys),intent(in) :: forc_t(1)          ! atmospheric temperature (Kelvin)
    real(kind_phys),intent(in) :: forc_pbot(1)       ! atmospheric pressure (Pa)
    real(kind_phys),intent(in) :: forc_psrf(1)       ! atmospheric surface pressure (Pa)
    real(kind_phys),intent(in) :: forc_hgt(1)        ! atmospheric reference height (m)
    real(kind_phys),intent(in) :: forc_hgt_q(1)      ! observational height of humidity [m]
    real(kind_phys),intent(in) :: forc_hgt_t(1)      ! observational height of temperature [m]
    real(kind_phys),intent(in) :: forc_hgt_u(1)      ! observational height of wind [m]
    real(kind_phys),intent(in) :: forc_q(1)          ! atmospheric specific humidity (kg/kg)
    real(kind_phys),intent(in) :: forc_u(1)          ! atmospheric wind speed in east direction (m/s)
    real(kind_phys),intent(in) :: forc_v(1)          ! atmospheric wind speed in north direction (m/s)
    real(kind_phys),intent(in) :: forc_lwrad(1)      ! downward infrared (longwave) radiation (W/m**2)
   ! real(kind_phys),intent(in) :: forc_rho(1)        ! density (kg/m**3)
    real(kind_phys),intent(in) :: forc_snow(1)       ! snow rate [mm/s]
    real(kind_phys),intent(in) :: forc_rain(1)       ! rain rate [mm/s]
    real(kind_phys),intent(in) :: h2osno(1)          ! snow water (mm H2O)
    real(kind_phys),intent(in) :: snowdp(1)          ! snow height (m)
    real(kind_phys),intent(in) :: sabg(1)            ! solar radiation absorbed by ground (W/m**2)
    real(kind_phys),intent(in) :: lat(1)             ! latitude (radians)
    real(kind_phys),intent(in) :: dz(1,-nlevsnow+1:nlevsoil)            ! layer thickness for soil or snow (m)
    real(kind_phys),intent(in) :: dz_lake(1,nlevlake)                  ! layer thickness for lake (m)
    real(kind_phys),intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)      ! soil (or snow) temperature (Kelvin)
    real(kind_phys),intent(in) :: t_lake(1,nlevlake)                   ! lake temperature (Kelvin)
    integer ,intent(in) :: snl(1)                              ! number of snow layers
    real(kind_phys),intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)    ! liquid water (kg/m2)
    real(kind_phys),intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)    ! ice lens (kg/m2)
    real(kind_phys),intent(in) :: savedtke1(1)       ! top level eddy conductivity from previous timestep (W/m.K)

    !inout:
    real(kind_phys),intent(inout) :: t_grnd(1)          ! ground temperature (Kelvin)
    !out:
    real(kind_phys),intent(out):: qflx_prec_grnd(1)  ! water onto ground including canopy runoff [kg/(m2 s)]
    real(kind_phys),intent(out):: qflx_evap_soi(1)   ! soil evaporation (mm H2O/s) (+ = to atm)
    real(kind_phys),intent(out):: qflx_evap_tot(1)   ! qflx_evap_soi + qflx_evap_veg + qflx_tran_veg
    real(kind_phys),intent(out):: eflx_sh_grnd(1)    ! sensible heat flux from ground (W/m**2) [+ to atm]
    real(kind_phys),intent(out):: eflx_lwrad_out(1)  ! emitted infrared (longwave) radiation (W/m**2)
    real(kind_phys),intent(out):: eflx_lwrad_net(1)  ! net infrared (longwave) rad (W/m**2) [+ = to atm]
    real(kind_phys),intent(out):: eflx_soil_grnd(1)  ! soil heat flux (W/m**2) [+ = into soil]
    real(kind_phys),intent(out):: eflx_sh_tot(1)     ! total sensible heat flux (W/m**2) [+ to atm]
    real(kind_phys),intent(out):: eflx_lh_tot(1)     ! total latent heat flux (W/m8*2)  [+ to atm]
    real(kind_phys),intent(out):: eflx_lh_grnd(1)    ! ground evaporation heat flux (W/m**2) [+ to atm]
    real(kind_phys),intent(out):: t_veg(1)           ! vegetation temperature (Kelvin)
    real(kind_phys),intent(out):: t_ref2m(1)         ! 2 m height surface air temperature (Kelvin)
    real(kind_phys),intent(out):: q_ref2m(1)         ! 2 m height surface specific humidity (kg/kg)
    real(kind_phys),intent(out):: taux(1)            ! wind (shear) stress: e-w (kg/m/s**2)
    real(kind_phys),intent(out):: tauy(1)            ! wind (shear) stress: n-s (kg/m/s**2)
    real(kind_phys),intent(out):: ram1(1)            ! aerodynamical resistance (s/m)
    real(kind_phys),intent(out):: ws(1)              ! surface friction velocity (m/s)
    real(kind_phys),intent(out):: ks(1)              ! coefficient passed to ShalLakeTemperature
                                               ! for calculation of decay of eddy diffusivity with depth
    real(kind_phys),intent(out):: eflx_gnet(1)       !net heat flux into ground (W/m**2)
                                               ! Change the type variable to pass back to WRF.
    real(kind_phys),intent(out):: z0mg(1)            ! roughness length over ground, momentum (m(



    !OTHER LOCAL VARIABLES:

    integer , parameter :: islak  = 2       ! index of lake, 1 = deep lake, 2 = shallow lake
    integer , parameter :: niters = 3       ! maximum number of iterations for surface temperature
    real(kind_phys), parameter :: beta1  = 1._kind_phys   ! coefficient of convective velocity (in computing W_*) [-]
    real(kind_phys), parameter :: emg    = 0.97_kind_phys ! ground emissivity (0.97 for snow)
    real(kind_phys), parameter :: zii    = 1000._kind_phys! convective boundary height [m]
    real(kind_phys), parameter :: tdmax  = 277._kind_phys ! temperature of maximum water density
    real(kind_phys) :: forc_th(1)         ! atmospheric potential temperature (Kelvin)
    real(kind_phys) :: forc_vp(1)         !atmospheric vapor pressure (Pa)
    real(kind_phys) :: forc_rho(1)        ! density (kg/m**3)
    integer  :: i,fc,fp,g,c,p           ! do loop or array index
    integer  :: fncopy                  ! number of values in pft filter copy
    integer  :: fnold                   ! previous number of pft filter values
    integer  :: fpcopy(num_shlakep)     ! pft filter copy for iteration loop
    integer  :: iter                    ! iteration index
    integer  :: nmozsgn(lbp:ubp)        ! number of times moz changes sign
    integer  :: jtop(lbc:ubc)           ! top level for each column (no longer all 1)
    !    real(kind_phys) :: dtime                   ! land model time step (sec)
    real(kind_phys) :: ax                      ! used in iteration loop for calculating t_grnd (numerator of NR solution)
    real(kind_phys) :: bx                      ! used in iteration loop for calculating t_grnd (denomin. of NR solution)
    real(kind_phys) :: degdT                   ! d(eg)/dT
    real(kind_phys) :: dqh(lbp:ubp)            ! diff of humidity between ref. height and surface
    real(kind_phys) :: dth(lbp:ubp)            ! diff of virtual temp. between ref. height and surface
    real(kind_phys) :: dthv                    ! diff of vir. poten. temp. between ref. height and surface
    real(kind_phys) :: dzsur(lbc:ubc)          ! 1/2 the top layer thickness (m)
    real(kind_phys) :: eg                      ! water vapor pressure at temperature T [pa]
    real(kind_phys) :: htvp(lbc:ubc)           ! latent heat of vapor of water (or sublimation) [j/kg]
    real(kind_phys) :: obu(lbp:ubp)            ! monin-obukhov length (m)
    real(kind_phys) :: obuold(lbp:ubp)         ! monin-obukhov length of previous iteration
    real(kind_phys) :: qsatg(lbc:ubc)          ! saturated humidity [kg/kg]
    real(kind_phys) :: qsatgdT(lbc:ubc)        ! d(qsatg)/dT
    real(kind_phys) :: qstar                   ! moisture scaling parameter
    real(kind_phys) :: ram(lbp:ubp)            ! aerodynamical resistance [s/m]
    real(kind_phys) :: rah(lbp:ubp)            ! thermal resistance [s/m]
    real(kind_phys) :: raw(lbp:ubp)            ! moisture resistance [s/m]
    real(kind_phys) :: stftg3(lbp:ubp)         ! derivative of fluxes w.r.t ground temperature
    real(kind_phys) :: temp1(lbp:ubp)          ! relation for potential temperature profile
    real(kind_phys) :: temp12m(lbp:ubp)        ! relation for potential temperature profile applied at 2-m
    real(kind_phys) :: temp2(lbp:ubp)          ! relation for specific humidity profile
    real(kind_phys) :: temp22m(lbp:ubp)        ! relation for specific humidity profile applied at 2-m
    real(kind_phys) :: tgbef(lbc:ubc)          ! initial ground temperature
    real(kind_phys) :: thm(lbc:ubc)            ! intermediate variable (forc_t+0.0098*forc_hgt_t)
    real(kind_phys) :: thv(lbc:ubc)            ! virtual potential temperature (kelvin)
    real(kind_phys) :: thvstar                 ! virtual potential temperature scaling parameter
    real(kind_phys) :: tksur                   ! thermal conductivity of snow/soil (w/m/kelvin)
    real(kind_phys) :: tsur                    ! top layer temperature
    real(kind_phys) :: tstar                   ! temperature scaling parameter
    real(kind_phys) :: um(lbp:ubp)             ! wind speed including the stablity effect [m/s]
    real(kind_phys) :: ur(lbp:ubp)             ! wind speed at reference height [m/s]
    real(kind_phys) :: ustar(lbp:ubp)          ! friction velocity [m/s]
    real(kind_phys) :: wc                      ! convective velocity [m/s]
    real(kind_phys) :: zeta                    ! dimensionless height used in Monin-Obukhov theory
    real(kind_phys) :: zldis(lbp:ubp)          ! reference height "minus" zero displacement height [m]
    real(kind_phys) :: displa(lbp:ubp)         ! displacement (always zero) [m]
    !    real(kind_phys) :: z0mg(lbp:ubp)           ! roughness length over ground, momentum [m]
    real(kind_phys) :: z0hg(lbp:ubp)           ! roughness length over ground, sensible heat [m]
    real(kind_phys) :: z0qg(lbp:ubp)           ! roughness length over ground, latent heat [m]
    real(kind_phys) :: beta(2)                 ! fraction solar rad absorbed at surface: depends on lake type
    real(kind_phys) :: u2m                     ! 2 m wind speed (m/s)
    real(kind_phys) :: u10(1)         ! 10-m wind (m/s) (for dust model)
    real(kind_phys) :: fv(1)          ! friction velocity (m/s) (for dust model)

    real(kind_phys) :: fm(lbp:ubp)             ! needed for BGC only to diagnose 10m wind speed
    real(kind_phys) :: bw                       ! partial density of water (ice + liquid)
    real(kind_phys) :: t_grnd_temp              ! Used in surface flux correction over frozen ground
    real(kind_phys) :: betaprime(lbc:ubc)       ! Effective beta: 1 for snow layers, beta(islak) otherwise
    character*256 :: message 
      ! This assumes all radiation is absorbed in the top snow layer and will need
      ! to be changed for CLM 4.
    !
    ! Constants for lake temperature model
    !
    data beta/0.4_kind_phys, 0.4_kind_phys/  ! (deep lake, shallow lake)
    ! This is the energy absorbed at the lake surface if no snow.
    !    data za  /0.6_kind_phys, 0.5_kind_phys/
    !    data eta /0.1_kind_phys, 0.5_kind_phys/
    !-----------------------------------------------------------------------
    
    
    !    dtime = get_step_size()
    
    ! Begin calculations
    
    !dir$ concurrent
    !cdir nodep
    forc_th(1)  = forc_t(1) * (forc_psrf(1)/ forc_pbot(1))**(rair/cpair)
    forc_vp(1)  = forc_q(1) * forc_pbot(1)/ (0.622 + 0.378 * forc_q(1))
    forc_rho(1) = (forc_pbot(1) - 0.378 * forc_vp(1)) / (rair * forc_t(1))

    do fc = 1, num_shlakec
       c = filter_shlakec(fc)
       g = cgridcell(c)

       ! Surface temperature and fluxes

       ! Find top layer
       if (snl(c) > 0 .or. snl(c) < -5) then
         errmsg='snl is not defined in ShalLakeFluxesMod; snl: out of range value'
         errflg=1
         return
       end if
       !       if (snl(c) /= 0) then
       !           write(6,*)'snl is not equal to zero in ShalLakeFluxesMod'
       !           call endrun()
       !       end if
       jtop(c) = snl(c) + 1


       if (snl(c) < 0) then
           betaprime(c) = 1._kind_phys  !Assume all solar rad. absorbed at the surface of the top snow layer. 
           dzsur(c) = dz(c,jtop(c))/2._kind_phys
       else
           betaprime(c) = beta(islak)
           dzsur(c) = dz_lake(c,1)/2._kind_phys
       end if
       ! Originally this was 1*dz, but shouldn't it be 1/2?

       ! Saturated vapor pressure, specific humidity and their derivatives
       ! at lake surface

       call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))

       ! Potential, virtual potential temperature, and wind speed at the
       ! reference height

       thm(c) = forc_t(g) + 0.0098_kind_phys*forc_hgt_t(g)   ! intermediate variable
       thv(c) = forc_th(g)*(1._kind_phys+0.61_kind_phys*forc_q(g))     ! virtual potential T
    end do

    !dir$ concurrent
    !cdir nodep
    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       nmozsgn(p) = 0
       obuold(p) = 0._kind_phys
       displa(p) = 0._kind_phys

       ! Roughness lengths
 

       ! changed by Hongping Gu
    !   if (t_grnd(c) >= tfrz) then   ! for unfrozen lake
    !      z0mg(p) = 0.01_kind_phys
    !   else                          ! for frozen lake
    !   ! Is this okay even if it is snow covered?  What is the roughness over
    !   non-veg. snow?
    !      z0mg(p) = 0.04_kind_phys
    !   end if
 
       if (t_grnd(c) >= tfrz) then   ! for unfrozen lake
          z0mg(p) = 0.001_kind_phys        !original 0.01
       else if(snl(c) == 0 ) then                         ! for frozen lake
       ! Is this okay even if it is snow covered?  What is the roughness over
       ! non-veg. snow?
          z0mg(p) = 0.005_kind_phys          !original 0.04, now for frozen lake without snow
       else                          ! for frozen lake with snow   
          z0mg(p) = 0.0024_kind_phys
       end if
 
 


       z0hg(p) = z0mg(p)
       z0qg(p) = z0mg(p)

       ! Latent heat

#if (defined PERGRO)
       htvp(c) = hvap
#else
       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if
#endif
       ! Zack Subin, 3/26/09: Shouldn't this be the ground temperature rather than the air temperature above?
       ! I'll change it for now.

       ! Initialize stability variables

       ur(p)    = max(1.0_kind_phys,sqrt(forc_u(g)*forc_u(g)+forc_v(g)*forc_v(g)))
       dth(p)   = thm(c)-t_grnd(c)
       dqh(p)   = forc_q(g)-qsatg(c)
       dthv     = dth(p)*(1._kind_phys+0.61_kind_phys*forc_q(g))+0.61_kind_phys*forc_th(g)*dqh(p)
       zldis(p) = forc_hgt_u(g) - 0._kind_phys

       ! Initialize Monin-Obukhov length and wind speed

       call MoninObukIni(ur(p), thv(c), dthv, zldis(p), z0mg(p), um(p), obu(p))

    end do

    iter = 1
    fncopy = num_shlakep
    fpcopy(1:num_shlakep) = filter_shlakep(1:num_shlakep)

    ! Begin stability iteration

    ITERATION : do while (iter <= niters .and. fncopy > 0)

       ! Determine friction velocity, and potential temperature and humidity
       ! profiles of the surface boundary layer

       call FrictionVelocity(pgridcell,forc_hgt,forc_hgt_u,          & !i
                             forc_hgt_t,forc_hgt_q,                  & !i
                             lbp, ubp, fncopy, fpcopy,               & !i
                             displa, z0mg, z0hg, z0qg,               & !i
                             obu, iter, ur, um,                      & !i
                             ustar,temp1, temp2, temp12m, temp22m,   & !o
                             u10,fv,                                 & !o
                             fm)  !i&o

       !dir$ concurrent
       !cdir nodep
       do fp = 1, fncopy
          p = fpcopy(fp)
          c = pcolumn(p)
          g = pgridcell(p)

          tgbef(c) = t_grnd(c)
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
             tksur = savedtke1(c)
             ! Set this to the eddy conductivity from the last
             ! timestep, as the molecular conductivity will be orders of magnitude too small.
             ! Will have to deal with first timestep.
             tsur = t_lake(c,1)
          else if (snl(c) == 0) then  !frozen but no snow layers
             tksur = tkice
             tsur = t_lake(c,1)
          else
          !Need to calculate thermal conductivity of the top snow layer
             bw = (h2osoi_ice(c,jtop(c))+h2osoi_liq(c,jtop(c)))/dz(c,jtop(c))
             tksur = tkairc + (7.75e-5_kind_phys *bw + 1.105e-6_kind_phys*bw*bw)*(tkice-tkairc)
             tsur = t_soisno(c,jtop(c))
          end if

          ! Determine aerodynamic resistances

          ram(p)  = 1._kind_phys/(ustar(p)*ustar(p)/um(p))
          rah(p)  = 1._kind_phys/(temp1(p)*ustar(p))
          raw(p)  = 1._kind_phys/(temp2(p)*ustar(p))
          ram1(p) = ram(p)   !pass value to global variable

          ! Get derivative of fluxes with respect to ground temperature

          stftg3(p) = emg*sb*tgbef(c)*tgbef(c)*tgbef(c)

          ! Changed surface temperature from t_lake(c,1) to tsur.
          ! Also adjusted so that if there are snow layers present, all radiation is absorbed in the top layer.
          ax  = betaprime(c)*sabg(p) + emg*forc_lwrad(g) + 3._kind_phys*stftg3(p)*tgbef(c) &
               + forc_rho(g)*cpair/rah(p)*thm(c) &
               - htvp(c)*forc_rho(g)/raw(p)*(qsatg(c)-qsatgdT(c)*tgbef(c) - forc_q(g)) &
               + tksur*tsur/dzsur(c)
          !Changed sabg(p) and to betaprime(c)*sabg(p).
          bx  = 4._kind_phys*stftg3(p) + forc_rho(g)*cpair/rah(p) &
               + htvp(c)*forc_rho(g)/raw(p)*qsatgdT(c) + tksur/dzsur(c)

          t_grnd(c) = ax/bx

          ! Update htvp
#ifndef PERGRO
       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if
#endif

          ! Surface fluxes of momentum, sensible and latent heat
          ! using ground temperatures from previous time step

          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-tgbef(c))-forc_q(g))/raw(p)

          ! Re-calculate saturated vapor pressure, specific humidity and their
          ! derivatives at lake surface

          call QSat(t_grnd(c), forc_pbot(g), eg, degdT, qsatg(c), qsatgdT(c))

          dth(p)=thm(c)-t_grnd(c)
          dqh(p)=forc_q(g)-qsatg(c)

          tstar = temp1(p)*dth(p)
          qstar = temp2(p)*dqh(p)

          thvstar=tstar*(1._kind_phys+0.61_kind_phys*forc_q(g)) + 0.61_kind_phys*forc_th(g)*qstar
          zeta=zldis(p)*vkc * grav*thvstar/(ustar(p)**2*thv(c))

          if (zeta >= 0._kind_phys) then     !stable
             zeta = min(2._kind_phys,max(zeta,0.01_kind_phys))
             um(p) = max(ur(p),0.1_kind_phys)
          else                     !unstable
             zeta = max(-100._kind_phys,min(zeta,-0.01_kind_phys))
             wc = beta1*(-grav*ustar(p)*thvstar*zii/thv(c))**0.333_kind_phys
             um(p) = sqrt(ur(p)*ur(p)+wc*wc)
          end if
          obu(p) = zldis(p)/zeta

          if (obuold(p)*obu(p) < 0._kind_phys) nmozsgn(p) = nmozsgn(p)+1

          obuold(p) = obu(p)

       end do   ! end of filtered pft loop

       iter = iter + 1
       if (iter <= niters ) then
          ! Rebuild copy of pft filter for next pass through the ITERATION loop

          fnold = fncopy
          fncopy = 0
          do fp = 1, fnold
             p = fpcopy(fp)
             if (nmozsgn(p) < 3) then
                fncopy = fncopy + 1
                fpcopy(fncopy) = p
             end if
          end do   ! end of filtered pft loop
       end if

    end do ITERATION   ! end of stability iteration

    !dir$ concurrent
    !cdir nodep
    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       ! If there is snow on the ground and t_grnd > tfrz: reset t_grnd = tfrz.
       ! Re-evaluate ground fluxes.
       ! h2osno > 0.5 prevents spurious fluxes.
       ! note that qsatg and qsatgdT should be f(tgbef) (PET: not sure what this
       ! comment means)
       ! Zack Subin, 3/27/09: Since they are now a function of whatever t_grnd was before cooling
       !    to freezing temperature, then this value should be used in the derivative correction term.
       ! Should this happen if the lake temperature is below freezing, too? I'll assume that for now.
       ! Also, allow convection if ground temp is colder than lake but warmer than 4C, or warmer than 
       !    lake which is warmer than freezing but less than 4C.
       !#ifndef SHLAKETEST
       if ( (h2osno(c) > 0.5_kind_phys .or. t_lake(c,1) <= tfrz) .and. t_grnd(c) > tfrz) then
         !#else
         !       if ( t_lake(c,1) <= tfrz .and. t_grnd(c) > tfrz) then
         !#endif
          t_grnd_temp = t_grnd(c)
          t_grnd(c) = tfrz
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-t_grnd_temp) - forc_q(g))/raw(p)
       else if ( (t_lake(c,1) > t_grnd(c) .and. t_grnd(c) > tdmax) .or. &
                 (t_lake(c,1) < t_grnd(c) .and. t_lake(c,1) > tfrz .and. t_grnd(c) < tdmax) ) then
                 ! Convective mixing will occur at surface
          t_grnd_temp = t_grnd(c)
          t_grnd(c) = t_lake(c,1)
          eflx_sh_grnd(p) = forc_rho(g)*cpair*(t_grnd(c)-thm(c))/rah(p)
          qflx_evap_soi(p) = forc_rho(g)*(qsatg(c)+qsatgdT(c)*(t_grnd(c)-t_grnd_temp) - forc_q(g))/raw(p)
       end if

          ! Update htvp
#ifndef PERGRO
       if (t_grnd(c) > tfrz) then
          htvp(c) = hvap
       else
          htvp(c) = hsub
       end if
#endif

       ! Net longwave from ground to atmosphere

       !       eflx_lwrad_out(p) = (1._kind_phys-emg)*forc_lwrad(g) + stftg3(p)*(-3._kind_phys*tgbef(c)+4._kind_phys*t_grnd(c))
       ! What is tgbef doing in this equation? Can't it be exact now? --Zack Subin, 4/14/09
       eflx_lwrad_out(p) = (1._kind_phys-emg)*forc_lwrad(g) + emg*sb*t_grnd(c)**4

       ! Ground heat flux

       eflx_soil_grnd(p) = sabg(p) + forc_lwrad(g) - eflx_lwrad_out(p) - &
            eflx_sh_grnd(p) - htvp(c)*qflx_evap_soi(p)
       !Why is this sabg(p) and not beta*sabg(p)??
       !I've kept this as the incorrect sabg so that the energy balance check will be correct.
       !This is the effective energy flux into the ground including the lake solar absorption
       !below the surface.  The variable eflx_gnet will be used to pass the actual heat flux
       !from the ground interface into the lake.

       taux(p) = -forc_rho(g)*forc_u(g)/ram(p)
       tauy(p) = -forc_rho(g)*forc_v(g)/ram(p)

       eflx_sh_tot(p)   = eflx_sh_grnd(p)
       qflx_evap_tot(p) = qflx_evap_soi(p)
       eflx_lh_tot(p)   = htvp(c)*qflx_evap_soi(p)
       eflx_lh_grnd(p)  = htvp(c)*qflx_evap_soi(p)
#if (defined LAKEDEBUG)
       print *,'c, sensible heat = ', c, eflx_sh_tot(p), 'latent heat = ', eflx_lh_tot(p) &
              , 'ground temp = ', t_grnd(c), 'h2osno = ', h2osno(c)
       if (abs(eflx_sh_tot(p)) > 1500 .or. abs(eflx_lh_tot(p)) > 1500) then
           print *,'WARNING: SH, LH = ', eflx_sh_tot(p), eflx_lh_tot(p)
       end if
       if (abs(eflx_sh_tot(p)) > 10000 .or. abs(eflx_lh_tot(p)) > 10000 &
             .or. abs(t_grnd(c)-288)>200 ) then
         errmsg='CLM_Lake ShalLakeFluxes: t_grnd is out of range'
         errflg=1
         return
       endif
#endif
       ! 2 m height air temperature
       t_ref2m(p) = thm(c) + temp1(p)*dth(p)*(1._kind_phys/temp12m(p) - 1._kind_phys/temp1(p))

       ! 2 m height specific humidity
       q_ref2m(p) = forc_q(g) + temp2(p)*dqh(p)*(1._kind_phys/temp22m(p) - 1._kind_phys/temp2(p))

       ! Energy residual used for melting snow
       ! Effectively moved to ShalLakeTemp

       ! Prepare for lake layer temperature calculations below
       ! fin(c) = betaprime * sabg(p) + forc_lwrad(g) - (eflx_lwrad_out(p) + &
       !          eflx_sh_tot(p) + eflx_lh_tot(p))
       ! NOW this is just the net ground heat flux calculated below.

       eflx_gnet(p) = betaprime(c) * sabg(p) + forc_lwrad(g) - (eflx_lwrad_out(p) + &
            eflx_sh_tot(p) + eflx_lh_tot(p))
       ! This is the actual heat flux from the ground interface into the lake, not including
       ! the light that penetrates the surface.

       !       u2m = max(1.0_kind_phys,ustar(p)/vkc*log(2._kind_phys/z0mg(p)))
       ! u2 often goes below 1 m/s; it seems like the only reason for this minimum is to
       ! keep it from being zero in the ks equation below; 0.1 m/s is a better limit for
       ! stable conditions --ZS
       u2m = max(0.1_kind_phys,ustar(p)/vkc*log(2._kind_phys/z0mg(p)))

       ws(c) = 1.2e-03_kind_phys * u2m
       ks(c) = 6.6_kind_phys*sqrt(abs(sin(lat(g))))*(u2m**(-1.84_kind_phys))

    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! End of surface flux relevant code in original BiogeophysicsLakeMod until history loop.

    ! The following are needed for global average on history tape.

    !dir$ concurrent
    !cdir nodep
    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)
       !       t_veg(p) = forc_t(g)
        !This is an odd choice, since elsewhere t_veg = t_grnd for bare ground.
        !Zack Subin, 4/09
       t_veg(p) = t_grnd(c)
       eflx_lwrad_net(p)  = eflx_lwrad_out(p) - forc_lwrad(g)
       qflx_prec_grnd(p) = forc_rain(g) + forc_snow(g)
    end do

END SUBROUTINE ShalLakeFluxes
 
SUBROUTINE ShalLakeTemperature(t_grnd,h2osno,sabg,dz,dz_lake,z,zi,           & !i
                                 z_lake,ws,ks,snl,eflx_gnet,lakedepth,       &
                                 lake_icefrac,snowdp,                        & !i&o
                                 eflx_sh_grnd,eflx_sh_tot,eflx_soil_grnd,    & !o
                                 t_lake,t_soisno,h2osoi_liq,                 &
                                 h2osoi_ice,savedtke1,                       &
                                 frac_iceold,qflx_snomelt,imelt,errmsg,errflg)
  !=======================================================================================================
  ! !DESCRIPTION:
  ! Calculates temperatures in the 20-25 layer column of (possible) snow,
  ! lake water, and soil beneath lake.
  ! Snow and soil temperatures are determined as in SoilTemperature, except
  ! for appropriate boundary conditions at the top of the snow (the flux is fixed
  ! to be the ground heat flux calculated in ShalLakeFluxes), the bottom of the snow
  ! (adjacent to top lake layer), and the top of the soil (adjacent to the bottom
  ! lake layer). Also, the soil is assumed to be always fully saturated (ShalLakeHydrology
  ! will have to insure this). The whole column is solved simultaneously as one tridiagonal matrix.
  ! Lake temperatures are determined from the Hostetler model as before, except now:
  !    i) Lake water layers can freeze by any fraction and release latent heat; thermal
  !       and mechanical properties are adjusted for ice fraction.
  !   ii) Convective mixing (though not eddy diffusion) still occurs for frozen lakes.
  !  iii) No sunlight is absorbed in the lake if there are snow layers.
  !   iv) Light is allowed to reach the top soil layer (where it is assumed to be completely absorbed).
  !    v) Lakes have variable depth, set ultimately in surface data set but now in initShalLakeMod.
  !
  ! Eddy + molecular diffusion:
  ! d ts    d            d ts     1 ds
  ! ---- = -- [(km + ke) ----] + -- --
  !  dt    dz             dz     cw dz
  !
  ! where: ts = temperature (kelvin)
  !         t = time (s)
  !         z = depth (m)
  !        km = molecular diffusion coefficient (m**2/s)
  !        ke = eddy diffusion coefficient (m**2/s)
  !        cw = heat capacity (j/m**3/kelvin)
  !         s = heat source term (w/m**2)
  !
  !   Shallow lakes are allowed to have variable depth, set in _____.
  !
  !   For shallow lakes:    ke > 0 if unfrozen,
  !       and convective mixing occurs WHETHER OR NOT frozen. (See e.g. Martynov...)
  !
  ! Use the Crank-Nicholson method to set up tridiagonal system of equations to
  ! solve for ts at time n+1, where the temperature equation for layer i is
  ! r_i = a_i [ts_i-1] n+1 + b_i [ts_i] n+1 + c_i [ts_i+1] n+1
  !
  ! The solution conserves energy as:
  !
  ! [For lake layers]
  ! cw*([ts(      1)] n+1 - [ts(      1)] n)*dz(      1)/dt + ... +
  ! cw*([ts(nlevlake)] n+1 - [ts(nlevlake)] n)*dz(nlevlake)/dt = fin
  ! But now there is phase change, so cv is not constant and there is
  ! latent heat.
  !
  ! where:
  ! [ts] n   = old temperature (kelvin)
  ! [ts] n+1 = new temperature (kelvin)
  ! fin      = heat flux into lake (w/m**2)
  !          = betaprime*sabg + forc_lwrad - eflx_lwrad_out - eflx_sh_tot - eflx_lh_tot
  !          (This is now the same as the ground heat flux.)
  !            + phi(1) + ... + phi(nlevlake) + phi(top soil level)
  ! betaprime = beta(islak) for no snow layers, and 1 for snow layers.
  ! This assumes all radiation is absorbed in the top snow layer and will need
  ! to be changed for CLM 4.
  !
  ! WARNING: This subroutine assumes lake columns have one and only one pft.
  !
  ! Outline:
  ! 1!) Initialization
  ! 2!) Lake density
  ! 3!) Diffusivity
  ! 4!) Heat source term from solar radiation penetrating lake
  ! 5!) Set thermal props and find initial energy content
  ! 6!) Set up vectors for tridiagonal matrix solution
  ! 7!) Solve tridiagonal and back-substitute
  ! 8!) (Optional) Do first energy check using temperature change at constant heat capacity.
  ! 9!) Phase change
  ! 9.5!) (Optional) Do second energy check using temperature change and latent heat, considering changed heat capacity.
  !                  Also do soil water balance check.
  !10!) Convective mixing 
  !11!) Do final energy check to detect small numerical errors (especially from convection)
  !     and dump small imbalance into sensible heat, or pass large errors to BalanceCheckMod for abort.
  !
  ! REVISION HISTORY:
  ! Created by Zack Subin, 2009.
  ! Reedited by Hongping Gu, 2010.
  !=========================================================================================================
  

  !    use TridiagonalMod     , only : Tridiagonal
    
    implicit none

    !in:
    integer, intent(inout) :: errflg
    character(*), intent(inout) :: errmsg
    real(kind_phys), intent(in) :: t_grnd(1)          ! ground temperature (Kelvin)
    real(kind_phys), intent(inout) :: h2osno(1)          ! snow water (mm H2O)
    real(kind_phys), intent(in) :: sabg(1)            ! solar radiation absorbed by ground (W/m**2)
    real(kind_phys), intent(in) :: dz(1,-nlevsnow + 1:nlevsoil)          ! layer thickness for snow & soil (m)
    real(kind_phys), intent(in) :: dz_lake(1,nlevlake)                  ! layer thickness for lake (m)
    real(kind_phys), intent(in) :: z(1,-nlevsnow+1:nlevsoil)             ! layer depth for snow & soil (m)
    real(kind_phys), intent(in) :: zi(1,-nlevsnow+0:nlevsoil)            ! interface level below a "z" level (m)
                                                                ! the other z and dz variables
    real(kind_phys), intent(in) :: z_lake(1,nlevlake)  ! layer depth for lake (m)
    real(kind_phys), intent(in) :: ws(1)              ! surface friction velocity (m/s)
    real(kind_phys), intent(in) :: ks(1)              ! coefficient passed to ShalLakeTemperature
                                               ! for calculation of decay of eddy diffusivity with depth
    integer , intent(in) :: snl(1)             ! negative of number of snow layers
    real(kind_phys), intent(inout) :: eflx_gnet(1)       ! net heat flux into ground (W/m**2) at the surface interface
    real(kind_phys), intent(in) :: lakedepth(1)       ! column lake depth (m)
    
   ! real(kind_phys), intent(in) :: watsat(1,nlevsoil)      ! volumetric soil water at saturation (porosity)
    real(kind_phys), intent(inout) :: snowdp(1)        !snow height (m)
    !out: 

    real(kind_phys), intent(out) :: eflx_sh_grnd(1)    ! sensible heat flux from ground (W/m**2) [+ to atm]
    real(kind_phys), intent(out) :: eflx_sh_tot(1)     ! total sensible heat flux (W/m**2) [+ to atm]
    real(kind_phys), intent(out) :: eflx_soil_grnd(1)  ! heat flux into snow / lake (W/m**2) [+ = into soil]
                                               ! Here this includes the whole lake radiation absorbed.
#if (defined SHLAKETEST)
    real(kind_phys), intent(out) :: qmelt(1)           ! snow melt [mm/s] [temporary]
#endif
    real(kind_phys), intent(inout) :: t_lake(1,nlevlake)                 ! lake temperature (Kelvin)
    real(kind_phys), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)    ! soil (or snow) temperature (Kelvin)
    real(kind_phys), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  ! liquid water (kg/m2) [for snow & soil layers]
    real(kind_phys), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  ! ice lens (kg/m2) [for snow & soil layers]
    real(kind_phys), intent(inout) :: lake_icefrac(1,nlevlake)           ! mass fraction of lake layer that is frozen
    real(kind_phys), intent(out) :: savedtke1(1)                      ! top level thermal conductivity (W/mK)
    real(kind_phys), intent(out) :: frac_iceold(1,-nlevsnow+1:nlevsoil) ! fraction of ice relative to the tot water
    real(kind_phys), intent(out) :: qflx_snomelt(1)  !snow melt (mm H2O /s)
    integer, intent(out)  :: imelt(1,-nlevsnow+1:nlevsoil)        !flag for melting (=1), freezing (=2), Not=0 (new)


    ! OTHER LOCAL VARIABLES:

    integer , parameter  :: islak = 2     ! index of lake, 1 = deep lake, 2 = shallow lake
    real(kind_phys), parameter  :: p0 = 1._kind_phys     ! neutral value of turbulent prandtl number
    integer  :: i,j,fc,fp,g,c,p         ! do loop or array index
    !    real(kind_phys) :: dtime                   ! land model time step (sec)
    real(kind_phys) :: beta(2)                 ! fraction solar rad absorbed at surface: depends on lake type
    real(kind_phys) :: za(2)                   ! base of surface absorption layer (m): depends on lake type
    real(kind_phys) :: eta(2)                  ! light extinction coefficient (/m): depends on lake type
    real(kind_phys) :: cwat                    ! specific heat capacity of water (j/m**3/kelvin)
    real(kind_phys) :: cice_eff                ! effective heat capacity of ice (using density of
                                          ! water because layer depth is not adjusted when freezing
    real(kind_phys) :: cfus                    ! effective heat of fusion per unit volume
                                          ! using water density as above
    real(kind_phys) :: km                      ! molecular diffusion coefficient (m**2/s)
    real(kind_phys) :: tkice_eff               ! effective conductivity since layer depth is constant
    real(kind_phys) :: a(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      ! "a" vector for tridiagonal matrix
    real(kind_phys) :: b(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      ! "b" vector for tridiagonal matrix
    real(kind_phys) :: c1(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)     ! "c" vector for tridiagonal matrix
    real(kind_phys) :: r(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)      ! "r" vector for tridiagonal solution
    real(kind_phys) :: rhow(lbc:ubc,nlevlake)   ! density of water (kg/m**3)
    real(kind_phys) :: phi(lbc:ubc,nlevlake)    ! solar radiation absorbed by layer (w/m**2)
    real(kind_phys) :: kme(lbc:ubc,nlevlake)    ! molecular + eddy diffusion coefficient (m**2/s)
    real(kind_phys) :: rsfin                   ! relative flux of solar radiation into layer
    real(kind_phys) :: rsfout                  ! relative flux of solar radiation out of layer
    real(kind_phys) :: phi_soil(lbc:ubc)       ! solar radiation into top soil layer (W/m**2)
    real(kind_phys) :: ri                      ! richardson number
    real(kind_phys) :: fin(lbc:ubc)            ! net heat flux into lake at ground interface (w/m**2)
    real(kind_phys) :: ocvts(lbc:ubc)          ! (cwat*(t_lake[n  ])*dz
    real(kind_phys) :: ncvts(lbc:ubc)          ! (cwat*(t_lake[n+1])*dz
    real(kind_phys) :: ke                      ! eddy diffusion coefficient (m**2/s)
    real(kind_phys) :: zin                     ! depth at top of layer (m)
    real(kind_phys) :: zout                    ! depth at bottom of layer (m)
    real(kind_phys) :: drhodz                  ! d [rhow] /dz (kg/m**4)
    real(kind_phys) :: n2                      ! brunt-vaisala frequency (/s**2)
    real(kind_phys) :: num                     ! used in calculating ri
    real(kind_phys) :: den                     ! used in calculating ri
    real(kind_phys) :: tav_froz(lbc:ubc)       ! used in aver temp for convectively mixed layers (C)
    real(kind_phys) :: tav_unfr(lbc:ubc)       ! "
    real(kind_phys) :: nav(lbc:ubc)            ! used in aver temp for convectively mixed layers
    real(kind_phys) :: phidum                  ! temporary value of phi
    real(kind_phys) :: iceav(lbc:ubc)          ! used in calc aver ice for convectively mixed layers
    real(kind_phys) :: qav(lbc:ubc)            ! used in calc aver heat content for conv. mixed layers
    integer  :: jtop(lbc:ubc)           ! top level for each column (no longer all 1)
    real(kind_phys) :: cv (lbc:ubc,-nlevsnow+1:nlevsoil)  !heat capacity of soil/snow [J/(m2 K)]
    real(kind_phys) :: tk (lbc:ubc,-nlevsnow+1:nlevsoil)  !thermal conductivity of soil/snow [W/(m K)]
                                                 !(at interface below, except for j=0)
    real(kind_phys) :: cv_lake (lbc:ubc,1:nlevlake)      !heat capacity [J/(m2 K)]
    real(kind_phys) :: tk_lake (lbc:ubc,1:nlevlake)  !thermal conductivity at layer node [W/(m K)]
    real(kind_phys) :: cvx (lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) !heat capacity for whole column [J/(m2 K)]
    real(kind_phys) :: tkix(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) !thermal conductivity at layer interfaces
                                                         !for whole column [W/(m K)]
    real(kind_phys) :: tx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) ! temperature of whole column [K]
    real(kind_phys) :: tktopsoillay(lbc:ubc)          ! thermal conductivity [W/(m K)]
    real(kind_phys) :: fnx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)  !heat diffusion through the layer interface below [W/m2]
    real(kind_phys) :: phix(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) !solar source term for whole column [W/m**2]
    real(kind_phys) :: zx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil)   !interface depth (+ below surface) for whole column [m]
    real(kind_phys) :: dzm                              !used in computing tridiagonal matrix [m]
    real(kind_phys) :: dzp                              !used in computing tridiagonal matrix [m]
    integer  :: jprime                   ! j - nlevlake
    real(kind_phys) :: factx(lbc:ubc,-nlevsnow+1:nlevlake+nlevsoil) !coefficient used in computing tridiagonal matrix
    real(kind_phys) :: t_lake_bef(lbc:ubc,1:nlevlake)    !beginning lake temp for energy conservation check [K]
    real(kind_phys) :: t_soisno_bef(lbc:ubc,-nlevsnow+1:nlevsoil) !beginning soil temp for E cons. check [K]
    real(kind_phys) :: lhabs(lbc:ubc)       ! total per-column latent heat abs. from phase change  (J/m^2)
    real(kind_phys) :: esum1(lbc:ubc)        ! temp for checking energy (J/m^2)
    real(kind_phys) :: esum2(lbc:ubc)        ! ""
    real(kind_phys) :: zsum(lbc:ubc)        ! temp for putting ice at the top during convection (m)
    real(kind_phys) :: wsum(lbc:ubc)        ! temp for checking water (kg/m^2)
    real(kind_phys) :: wsum_end(lbc:ubc)    ! temp for checking water (kg/m^2)
    real(kind_phys) :: errsoi(1)                         ! soil/lake energy conservation error (W/m**2)
    real(kind_phys) :: eflx_snomelt(1)  !snow melt heat flux (W/m**2)
    CHARACTER*256 :: message
    !
    ! Constants for lake temperature model
    !
    data beta/0.4_kind_phys, 0.4_kind_phys/  ! (deep lake, shallow lake)
    data za  /0.6_kind_phys, 0.6_kind_phys/
    !   For now, keep beta and za for shallow lake the same as deep lake, until better data is found.
    !   It looks like eta is key and that larger values give better results for shallow lakes.  Use
    !   empirical expression from Hakanson (below). This is still a very unconstrained parameter
    !   that deserves more attention.
    !   Some radiation will be allowed to reach the soil.
    !-----------------------------------------------------------------------
    
    
    ! 1!) Initialization
    ! Determine step size

    !    dtime = get_step_size()

    ! Initialize constants
    cwat = cpliq*denh2o ! water heat capacity per unit volume
    cice_eff = cpice*denh2o !use water density because layer depth is not adjusted
                              !for freezing
    cfus = hfus*denh2o  ! latent heat per unit volume
    tkice_eff = tkice * denice/denh2o !effective conductivity since layer depth is constant
    km = tkwat/cwat     ! a constant (molecular diffusivity)

    ! Begin calculations

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakec
       c = filter_shlakec(fc)

       ! Initialize Ebal quantities computed below

       ocvts(c) = 0._kind_phys
       ncvts(c) = 0._kind_phys
       esum1(c) = 0._kind_phys
       esum2(c) = 0._kind_phys

    end do

    ! Initialize set of previous time-step variables as in DriverInit,
    ! which is currently not called over lakes. This has to be done
    ! here because phase change will occur in this routine.
    ! Ice fraction of snow at previous time step

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
      do fc = 1, num_shlakec
         c = filter_shlakec(fc)
         if (j >= snl(c) + 1) then
            frac_iceold(c,j) = h2osoi_ice(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))
         end if
      end do
    end do

    ! Sum soil water.
    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          if (j == 1) wsum(c) = 0._kind_phys
          wsum(c) = wsum(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)


       ! Prepare for lake layer temperature calculations below

       ! fin(c) = betaprime * sabg(p) + forc_lwrad(g) - (eflx_lwrad_out(p) + &
       !     eflx_sh_tot(p) + eflx_lh_tot(p)) 
       ! fin(c) now passed from ShalLakeFluxes as eflx_gnet
       fin(c) = eflx_gnet(p)

    end do

    ! 2!) Lake density

    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          rhow(c,j) = (1._kind_phys - lake_icefrac(c,j)) * & 
                      1000._kind_phys*( 1.0_kind_phys - 1.9549e-05_kind_phys*(abs(t_lake(c,j)-277._kind_phys))**1.68_kind_phys ) &
                    + lake_icefrac(c,j)*denice
                    ! Allow for ice fraction; assume constant ice density.
                    ! Is this the right weighted average?
                    ! Using this average will make sure that surface ice is treated properly during
                    ! convective mixing.
       end do
    end do

    ! 3!) Diffusivity and implied thermal "conductivity" = diffusivity * cwat
    do j = 1, nlevlake-1
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          drhodz = (rhow(c,j+1)-rhow(c,j)) / (z_lake(c,j+1)-z_lake(c,j))
          n2 = grav / rhow(c,j) * drhodz
          ! Fixed sign error here: our z goes up going down into the lake, so no negative
          ! sign is needed to make this positive unlike in Hostetler. --ZS
          num = 40._kind_phys * n2 * (vkc*z_lake(c,j))**2
          den = max( (ws(c)**2) * exp(-2._kind_phys*ks(c)*z_lake(c,j)), 1.e-10_kind_phys )
          ri = ( -1._kind_phys + sqrt( max(1._kind_phys+num/den, 0._kind_phys) ) ) / 20._kind_phys
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
            ! ke = vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)

             if( t_lake(c,1) > 277.15_kind_phys ) then 
                if (lakedepth(c) > 15.0 ) then 
                   ke = 1.e+2_kind_phys*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)
                else 
                   ke = vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)
                endif
             else 
                if (lakedepth(c) > 15.0 ) then 
                  if (lakedepth(c) > 150.0 ) then 
                    ke = 1.e+5_kind_phys*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)
                  else 
                    ke =1.e+4_kind_phys*vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)
                  end if
                else 
                  ke = vkc*ws(c)*z_lake(c,j)/p0 * exp(-ks(c)*z_lake(c,j)) / (1._kind_phys+37._kind_phys*ri*ri)
                endif 
             end if

             kme(c,j) = km + ke
             tk_lake(c,j) = kme(c,j)*cwat
             ! If there is some ice in this layer (this should rarely happen because the surface
             ! is unfrozen and it will be unstable), still use the cwat to get out the tk b/c the eddy
             ! diffusivity equation assumes water.
          else
             kme(c,j) = km
             tk_lake(c,j) = tkwat*tkice_eff / ( (1._kind_phys-lake_icefrac(c,j))*tkice_eff &
                            + tkwat*lake_icefrac(c,j) )
             ! Assume the resistances add as for the calculation of conductivities at layer interfaces.
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakec
       c = filter_shlakec(fc)

       j = nlevlake
       kme(c,nlevlake) = kme(c,nlevlake-1)

       if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
          tk_lake(c,j) = tk_lake(c,j-1)
       else
          tk_lake(c,j) = tkwat*tkice_eff / ( (1._kind_phys-lake_icefrac(c,j))*tkice_eff &
                            + tkwat*lake_icefrac(c,j) )
       end if

       ! Use in surface flux calculation for next timestep.
       savedtke1(c) = kme(c,1)*cwat ! Will only be used if unfrozen
       ! set number of column levels for use by Tridiagonal below
       jtop(c) = snl(c) + 1
    end do

    ! 4!) Heat source term: unfrozen lakes only
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fp = 1, num_shlakep
          p = filter_shlakep(fp)
          c = pcolumn(p)

          ! Set eta(:), the extinction coefficient, according to L Hakanson, Aquatic Sciences, 1995
          ! (regression of Secchi Depth with lake depth for small glacial basin lakes), and the
          ! Poole & Atkins expression for extinction coeffient of 1.7 / Secchi Depth (m).
#ifndef ETALAKE
          eta(:) = 1.1925_kind_phys*lakedepth(c)**(-0.424)
#else
          eta(:) = ETALAKE
#endif

          zin  = z_lake(c,j) - 0.5_kind_phys*dz_lake(c,j)
          zout = z_lake(c,j) + 0.5_kind_phys*dz_lake(c,j)
          rsfin  = exp( -eta(islak)*max(  zin-za(islak),0._kind_phys ) )
          rsfout = exp( -eta(islak)*max( zout-za(islak),0._kind_phys ) )

          ! Let rsfout for bottom layer go into soil.
          ! This looks like it should be robust even for pathological cases,
            ! like lakes thinner than za.
          if (t_grnd(c) > tfrz .and. t_lake(c,1) > tfrz .and. snl(c) == 0) then
             phidum = (rsfin-rsfout) * sabg(p) * (1._kind_phys-beta(islak))
             if (j == nlevlake) then
                phi_soil(c) = rsfout * sabg(p) * (1._kind_phys-beta(islak))
             end if
          else if (j == 1 .and. snl(c) == 0) then !if frozen but no snow layers
             phidum = sabg(p) * (1._kind_phys-beta(islak))
          else !radiation absorbed at surface
             phidum = 0._kind_phys
             if (j == nlevlake) phi_soil(c) = 0._kind_phys
          end if
          phi(c,j) = phidum

       end do
    end do

    ! 5!) Set thermal properties and check initial energy content.

    ! For lake
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          cv_lake(c,j) = dz_lake(c,j) * (cwat*(1._kind_phys-lake_icefrac(c,j)) + cice_eff*lake_icefrac(c,j))
       end do
    end do

    ! For snow / soil
  call SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay,errmsg,errflg)
  if(errflg/=0) then
    ! State is no longer valid, so return error to caller
    return
  endif

    ! Sum cv*t_lake for energy check
    ! Include latent heat term, and correction for changing heat capacity with phase change.

    ! This will need to be over all soil / lake / snow layers. Lake is below.
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          !          ocvts(c) = ocvts(c) + cv_lake(c,j)*t_lake(c,j) &
          ocvts(c) = ocvts(c) + cv_lake(c,j)*(t_lake(c,j)-tfrz) &
                   + cfus*dz_lake(c,j)*(1._kind_phys-lake_icefrac(c,j)) !&
          !                   + (cwat-cice_eff)*lake_icefrac(c)*tfrz*dz_lake(c,j) !enthalpy reconciliation term
          t_lake_bef(c,j) = t_lake(c,j)
       end do
    end do

    ! Now do for soil / snow layers
    do j = -nlevsnow + 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then
            !             ocvts(c) = ocvts(c) + cv(c,j)*t_soisno(c,j) &
             ocvts(c) = ocvts(c) + cv(c,j)*(t_soisno(c,j)-tfrz) &
                      + hfus*h2osoi_liq(c,j) !&
             !                      + (cpliq-cpice)*h2osoi_ice(c,j)*tfrz !enthalpy reconciliation term
             if (j == 1 .and. h2osno(c) > 0._kind_phys .and. j == jtop(c)) then
                ocvts(c) = ocvts(c) - h2osno(c)*hfus
             end if
             t_soisno_bef(c,j) = t_soisno(c,j)
             if(abs(t_soisno(c,j)-288) > 150)   then 
                WRITE( message,* ) 'WARNING: Extreme t_soisno at c, level',c, j
                errmsg=trim(message)
                errflg=1
                return
             endif
          end if
       end do
    end do

    !!!!!!!!!!!!!!!!!!!
    ! 6!) Set up vector r and vectors a, b, c1 that define tridiagonal matrix

    ! Heat capacity and resistance of snow without snow layers (<1cm) is ignored during diffusion,
    ! but its capacity to absorb latent heat may be used during phase change.

    ! Set up interface depths, zx, heat capacities, cvx, solar source terms, phix, and temperatures, tx.
    do j = -nlevsnow+1, nlevlake+nlevsoil
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake

          if (j >= jtop(c)) then
             if (j < 1) then !snow layer
                zx(c,j) = z(c,j)
                cvx(c,j) = cv(c,j)
                phix(c,j) = 0._kind_phys
                tx(c,j) = t_soisno(c,j)
             else if (j <= nlevlake) then !lake layer
                zx(c,j) = z_lake(c,j)
                cvx(c,j) = cv_lake(c,j)
                phix(c,j) = phi(c,j)
                tx(c,j) = t_lake(c,j)
             else !soil layer
                zx(c,j) = zx(c,nlevlake) + dz_lake(c,nlevlake)/2._kind_phys + z(c,jprime)
                cvx(c,j) = cv(c,jprime)
                if (j == nlevlake + 1) then !top soil layer
                   phix(c,j) = phi_soil(c)
                else !middle or bottom soil layer
                   phix(c,j) = 0._kind_phys
                end if
                tx(c,j) = t_soisno(c,jprime)
             end if
          end if

       end do
    end do

    ! Determine interface thermal conductivities, tkix

    do j = -nlevsnow+1, nlevlake+nlevsoil
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake

          if (j >= jtop(c)) then
             if (j < 0) then !non-bottom snow layer
                tkix(c,j) = tk(c,j)
             else if (j == 0) then !bottom snow layer
                dzp = zx(c,j+1) - zx(c,j)
                tkix(c,j) = tk_lake(c,1)*tk(c,j)*dzp / &
                      (tk(c,j)*z_lake(c,1) + tk_lake(c,1)*(-z(c,j)) )
                ! tk(c,0) is the conductivity at the middle of that layer, as defined in SoilThermProp_Lake
             else if (j < nlevlake) then !non-bottom lake layer
                tkix(c,j) = ( tk_lake(c,j)*tk_lake(c,j+1) * (dz_lake(c,j+1)+dz_lake(c,j)) ) &
                           / ( tk_lake(c,j)*dz_lake(c,j+1) + tk_lake(c,j+1)*dz_lake(c,j) )
             else if (j == nlevlake) then !bottom lake layer
                dzp = zx(c,j+1) - zx(c,j)
                tkix(c,j) = (tktopsoillay(c)*tk_lake(c,j)*dzp / &
                    (tktopsoillay(c)*dz_lake(c,j)/2._kind_phys + tk_lake(c,j)*z(c,1) ) )
                    ! tktopsoillay is the conductivity at the middle of that layer, as defined in SoilThermProp_Lake
             else !soil layer
                tkix(c,j) = tk(c,jprime)
             end if
         end if

      end do 
   end do


    ! Determine heat diffusion through the layer interface and factor used in computing
    ! tridiagonal matrix and set up vector r and vectors a, b, c1 that define tridiagonal
    ! matrix and solve system

    do j = -nlevsnow+1, nlevlake+nlevsoil
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= jtop(c)) then
             if (j < nlevlake+nlevsoil) then !top or interior layer
                factx(c,j) = dtime/cvx(c,j)
                fnx(c,j) = tkix(c,j)*(tx(c,j+1)-tx(c,j))/(zx(c,j+1)-zx(c,j))
             else !bottom soil layer
                factx(c,j) = dtime/cvx(c,j)
                fnx(c,j) = 0._kind_phys !not used
             end if
          end if
       enddo
    end do

    do j = -nlevsnow+1,nlevlake+nlevsoil
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= jtop(c)) then
             if (j == jtop(c)) then !top layer
                dzp    = zx(c,j+1)-zx(c,j)
                a(c,j) = 0._kind_phys
                b(c,j) = 1+(1._kind_phys-cnfac)*factx(c,j)*tkix(c,j)/dzp
                c1(c,j) =  -(1._kind_phys-cnfac)*factx(c,j)*tkix(c,j)/dzp
                r(c,j) = tx(c,j) + factx(c,j)*( fin(c) + phix(c,j) + cnfac*fnx(c,j) )
             else if (j < nlevlake+nlevsoil) then !middle layer
                dzm    = (zx(c,j)-zx(c,j-1))
                dzp    = (zx(c,j+1)-zx(c,j))
                a(c,j) =   - (1._kind_phys-cnfac)*factx(c,j)* tkix(c,j-1)/dzm
                b(c,j) = 1._kind_phys+ (1._kind_phys-cnfac)*factx(c,j)*(tkix(c,j)/dzp + tkix(c,j-1)/dzm)
                c1(c,j) =   - (1._kind_phys-cnfac)*factx(c,j)* tkix(c,j)/dzp
                r(c,j) = tx(c,j) + cnfac*factx(c,j)*( fnx(c,j) - fnx(c,j-1) ) + factx(c,j)*phix(c,j)
             else  !bottom soil layer
                dzm     = (zx(c,j)-zx(c,j-1))
                a(c,j) =   - (1._kind_phys-cnfac)*factx(c,j)*tkix(c,j-1)/dzm
                b(c,j) = 1._kind_phys+ (1._kind_phys-cnfac)*factx(c,j)*tkix(c,j-1)/dzm
                c1(c,j) = 0._kind_phys
                r(c,j) = tx(c,j) - cnfac*factx(c,j)*fnx(c,j-1)
             end if
          end if
       enddo
    end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! 7!) Solve for tdsolution

    call Tridiagonal(lbc, ubc, -nlevsnow + 1, nlevlake + nlevsoil, jtop, num_shlakec, filter_shlakec, &
                     a, b, c1, r, tx)
 
    ! Set t_soisno and t_lake
    do j = -nlevsnow+1, nlevlake + nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          jprime = j - nlevlake

          ! Don't do anything with invalid snow layers.
          if (j >= jtop(c)) then
             if (j < 1) then !snow layer
             t_soisno(c,j) = tx(c,j)
             else if (j <= nlevlake) then !lake layer
             t_lake(c,j)   = tx(c,j)
             else !soil layer
             t_soisno(c,jprime) = tx(c,j)
             end if
          end if
       end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!

    ! 8!) Sum energy content and total energy into lake for energy check. Any errors will be from the
    !     Tridiagonal solution.

#if (defined LAKEDEBUG)
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          esum1(c) = esum1(c) + (t_lake(c,j)-t_lake_bef(c,j))*cv_lake(c,j)
          esum2(c) = esum2(c) + (t_lake(c,j)-tfrz)*cv_lake(c,j)
       end do
    end do

    do j = -nlevsnow+1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then
             esum1(c) = esum1(c) + (t_soisno(c,j)-t_soisno_bef(c,j))*cv(c,j)
             esum2(c) = esum2(c) + (t_soisno(c,j)-tfrz)*cv(c,j)
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
       do fp = 1, num_shlakep
          p = filter_shlakep(fp)
          c = pcolumn(p)
          ! Again assuming only one pft per column
          !          esum1(c) = esum1(c) + lhabs(c)
          errsoi(c) = esum1(c)/dtime - eflx_soil_grnd(p)
                    ! eflx_soil_grnd includes all the solar radiation absorbed in the lake,
                    ! unlike eflx_gnet
          if(abs(errsoi(c)) > 1.e-5_kind_phys) then
             WRITE( message,* )'Primary soil energy conservation error in shlake &
                                column during Tridiagonal Solution,', 'error (W/m^2):', c, errsoi(c) 
             errmsg=trim(message)
             errflg=1
             return
          end if
       end do
       ! This has to be done before convective mixing because the heat capacities for each layer
       ! will get scrambled.

#endif

    !!!!!!!!!!!!!!!!!!!!!!!

    ! 9!) Phase change
    call PhaseChange_Lake (snl,h2osno,dz,dz_lake,                            & !i
                               t_soisno,h2osoi_liq,h2osoi_ice,               & !i&o
                               lake_icefrac,t_lake, snowdp,                  & !i&o
                               qflx_snomelt,eflx_snomelt,imelt,              & !o  
                               cv, cv_lake,                                  & !i&o
                               lhabs)                                          !o

    !!!!!!!!!!!!!!!!!!!!!!!

    ! 9.5!) Second energy check and water check.  Now check energy balance before and after phase
    !       change, considering the possibility of changed heat capacity during phase change, by
    !       using initial heat capacity in the first step, final heat capacity in the second step,
    !       and differences from tfrz only to avoid enthalpy correction for (cpliq-cpice)*melt*tfrz.
    !       Also check soil water sum.

#if (defined LAKEDEBUG)
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          esum2(c) = esum2(c) - (t_lake(c,j)-tfrz)*cv_lake(c,j)
       end do
    end do

    do j = -nlevsnow+1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then
             esum2(c) = esum2(c) - (t_soisno(c,j)-tfrz)*cv(c,j)
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
       do fp = 1, num_shlakep
          p = filter_shlakep(fp)
          c = pcolumn(p)
          ! Again assuming only one pft per column
          esum2(c) = esum2(c) - lhabs(c)
          errsoi(c) = esum2(c)/dtime
          if(abs(errsoi(c)) > 1.e-5_kind_phys) then
             write(message,*)'Primary soil energy conservation error in shlake column during Phase Change, error (W/m^2):', &
                       c, errsoi(c)
             errmsg=trim(message)
             errflg=1
             return
          end if
       end do

    ! Check soil water
    ! Sum soil water.
    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          if (j == 1) wsum_end(c) = 0._kind_phys
          wsum_end(c) = wsum_end(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
          if (j == nlevsoil) then
             if (abs(wsum(c)-wsum_end(c))>1.e-7_kind_phys) then
                write(message,*)'Soil water balance error during phase change in ShalLakeTemperature.', &
                          'column, error (kg/m^2):', c, wsum_end(c)-wsum(c)
                errmsg=trim(message)
                errflg=1
                return
             end if
          end if
       end do
    end do

#endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!
    ! 10!) Convective mixing: make sure fracice*dz is conserved, heat content c*dz*T is conserved, and
    ! all ice ends up at the top. Done over all lakes even if frozen.
    ! Either an unstable density profile or ice in a layer below an incompletely frozen layer will trigger.

    !Recalculate density
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          rhow(c,j) = (1._kind_phys - lake_icefrac(c,j)) * &
                      1000._kind_phys*( 1.0_kind_phys - 1.9549e-05_kind_phys*(abs(t_lake(c,j)-277._kind_phys))**1.68_kind_phys ) &
                    + lake_icefrac(c,j)*denice
       end do
    end do

    do j = 1, nlevlake-1
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          qav(c) = 0._kind_phys
          nav(c) = 0._kind_phys
          iceav(c) = 0._kind_phys
       end do

       do i = 1, j+1
         !dir$ concurrent
         !cdir nodep
          do fc = 1, num_shlakec
             c = filter_shlakec(fc)
             if (rhow(c,j) > rhow(c,j+1) .or. &
                (lake_icefrac(c,j) < 1._kind_phys .and. lake_icefrac(c,j+1) > 0._kind_phys) ) then
#if (defined LAKEDEBUG)
                if (i==1)  then
                  print *, 'Convective Mixing in column ', c, '.'
                endif
#endif
                qav(c) = qav(c) + dz_lake(c,i)*(t_lake(c,i)-tfrz) * & 
                        ((1._kind_phys - lake_icefrac(c,i))*cwat + lake_icefrac(c,i)*cice_eff)
                !                tav(c) = tav(c) + t_lake(c,i)*dz_lake(c,i)
                iceav(c) = iceav(c) + lake_icefrac(c,i)*dz_lake(c,i)
                nav(c) = nav(c) + dz_lake(c,i)
             end if
          end do
       end do

       !dir$ concurrent
       !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          if (rhow(c,j) > rhow(c,j+1) .or. &
             (lake_icefrac(c,j) < 1._kind_phys .and. lake_icefrac(c,j+1) > 0._kind_phys) ) then
             qav(c) = qav(c)/nav(c)
             iceav(c) = iceav(c)/nav(c)
             !If the average temperature is above freezing, put the extra energy into the water.
             !If it is below freezing, take it away from the ice.
             if (qav(c) > 0._kind_phys) then
                tav_froz(c) = 0._kind_phys !Celsius
                tav_unfr(c) = qav(c) / ((1._kind_phys - iceav(c))*cwat)
             else if (qav(c) < 0._kind_phys) then
                tav_froz(c) = qav(c) / (iceav(c)*cice_eff)
                tav_unfr(c) = 0._kind_phys !Celsius
             else
                tav_froz(c) = 0._kind_phys
                tav_unfr(c) = 0._kind_phys
             end if
          end if
       end do

       do i = 1, j+1
         !dir$ concurrent
         !cdir nodep
          do fc = 1, num_shlakec
             c = filter_shlakec(fc)
             if (nav(c) > 0._kind_phys) then
               !             if(0==1) then

                !Put all the ice at the top.!
                !If the average temperature is above freezing, put the extra energy into the water.
                !If it is below freezing, take it away from the ice.
                !For the layer with both ice & water, be careful to use the average temperature
                !that preserves the correct total heat content given what the heat capacity of that
                !layer will actually be.
                if (i == 1) zsum(c) = 0._kind_phys
                if ((zsum(c)+dz_lake(c,i))/nav(c) <= iceav(c)) then
                   lake_icefrac(c,i) = 1._kind_phys
                   t_lake(c,i) = tav_froz(c) + tfrz
                else if (zsum(c)/nav(c) < iceav(c)) then
                   lake_icefrac(c,i) = (iceav(c)*nav(c) - zsum(c)) / dz_lake(c,i)
                   ! Find average value that preserves correct heat content.
                   t_lake(c,i) = ( lake_icefrac(c,i)*tav_froz(c)*cice_eff &
                               + (1._kind_phys - lake_icefrac(c,i))*tav_unfr(c)*cwat ) &
                               / ( lake_icefrac(c,i)*cice_eff + (1-lake_icefrac(c,i))*cwat ) + tfrz
                else
                   lake_icefrac(c,i) = 0._kind_phys
                   t_lake(c,i) = tav_unfr(c) + tfrz
                end if
                zsum(c) = zsum(c) + dz_lake(c,i)

                rhow(c,i) = (1._kind_phys - lake_icefrac(c,i)) * & 
                            1000._kind_phys*( 1.0_kind_phys - 1.9549e-05_kind_phys*(abs(t_lake(c,i)-277._kind_phys))**1.68_kind_phys ) &
                          + lake_icefrac(c,i)*denice
             end if
          end do
       end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!
    ! 11!) Re-evaluate thermal properties and sum energy content.
    ! For lake
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          cv_lake(c,j) = dz_lake(c,j) * (cwat*(1._kind_phys-lake_icefrac(c,j)) + cice_eff*lake_icefrac(c,j))
#if (defined LAKEDEBUG)
          print *,'Lake Ice Fraction, c, level:', c, j, lake_icefrac(c,j)
#endif
       end do
    end do
    ! For snow / soil
  !  call SoilThermProp_Lake(lbc, ubc, num_shlakec, filter_shlakec, tk, cv, tktopsoillay)
  call SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay,errmsg,errflg)


    ! Do as above to sum energy content
    do j = 1, nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          !          ncvts(c) = ncvts(c) + cv_lake(c,j)*t_lake(c,j) &
          ncvts(c) = ncvts(c) + cv_lake(c,j)*(t_lake(c,j)-tfrz) &
                   + cfus*dz_lake(c,j)*(1._kind_phys-lake_icefrac(c,j)) !&
          !                   + (cwat-cice_eff)*lake_icefrac(c)*tfrz*dz_lake(c,j) !enthalpy reconciliation term
          fin(c) = fin(c) + phi(c,j)
       end do
    end do

    do j = -nlevsnow + 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (j >= jtop(c)) then
            !             ncvts(c) = ncvts(c) + cv(c,j)*t_soisno(c,j) &
             ncvts(c) = ncvts(c) + cv(c,j)*(t_soisno(c,j)-tfrz) &
                      + hfus*h2osoi_liq(c,j) !&
             !                      + (cpliq-cpice)*h2osoi_ice(c,j)*tfrz !enthalpy reconciliation term
             if (j == 1 .and. h2osno(c) > 0._kind_phys .and. j == jtop(c)) then
                ncvts(c) = ncvts(c) - h2osno(c)*hfus
             end if
          end if
          if (j == 1) fin(c) = fin(c) + phi_soil(c)
       end do
    end do


    ! Check energy conservation.

    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       errsoi(c) = (ncvts(c)-ocvts(c)) / dtime - fin(c)
#ifndef LAKEDEBUG
       !       if (abs(errsoi(c)) < 0.10_kind_phys) then ! else send to Balance Check and abort
       if (abs(errsoi(c)) < 10._kind_phys) then ! else send to Balance Check and abort
#else
       if (abs(errsoi(c)) < 1._kind_phys) then
#endif
          eflx_sh_tot(p) = eflx_sh_tot(p) - errsoi(c)
          eflx_sh_grnd(p) = eflx_sh_grnd(p) - errsoi(c)
          eflx_soil_grnd(p) = eflx_soil_grnd(p) + errsoi(c)
          eflx_gnet(p) = eflx_gnet(p) + errsoi(c)
          !          if (abs(errsoi(c)) > 1.e-3_kind_phys) then
          if (abs(errsoi(c)) > 1.e-1_kind_phys) then
             print *,'errsoi incorporated into sensible heat in ShalLakeTemperature: c, (W/m^2):', c, errsoi(c)
          end if
          errsoi(c) = 0._kind_phys
#if (defined LAKEDEBUG)
       else
          print *,'Soil Energy Balance Error at column, ', c, 'G, fintotal, column E tendency = ', &
             eflx_gnet(p), fin(c), (ncvts(c)-ocvts(c)) / dtime
#endif
       end if
    end do
    ! This loop assumes only one point per column.

  end subroutine ShalLakeTemperature

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------
  !BOP
  !
  ! ROUTINE: SoilThermProp_Lake
  !
  ! !INTERFACE:
  subroutine SoilThermProp_Lake (snl,dz,zi,z,t_soisno,h2osoi_liq,h2osoi_ice,    &
                           tk, cv, tktopsoillay,errmsg,errflg)

    !
    ! !DESCRIPTION:
    ! Calculation of thermal conductivities and heat capacities of
    ! snow/soil layers
    ! (1) The volumetric heat capacity is calculated as a linear combination
    !     in terms of the volumetric fraction of the constituent phases.
    !
    ! (2) The thermal conductivity of soil is computed from the algorithm of
    !     Johansen (as reported by Farouki 1981), and of snow is from the
    !     formulation used in SNTHERM (Jordan 1991).
    ! The thermal conductivities at the interfaces between two neighboring
    ! layers (j, j+1) are derived from an assumption that the flux across
    ! the interface is equal to that from the node j to the interface and the
    ! flux from the interface to the node j+1.
    !
    ! For lakes, the proper soil layers (not snow) should always be saturated.
    !
    ! !USES:
    
    implicit none
    !in

    integer, intent(inout) :: errflg
    character(*), intent(inout) :: errmsg
    integer , intent(in) :: snl(1)           ! number of snow layers
    !    real(kind_phys), intent(in) :: h2osno(1)        ! snow water (mm H2O)
   ! real(kind_phys), intent(in) :: watsat(1,nlevsoil)      ! volumetric soil water at saturation (porosity)
   ! real(kind_phys), intent(in) :: tksatu(1,nlevsoil)      ! thermal conductivity, saturated soil [W/m-K]
   ! real(kind_phys), intent(in) :: tkmg(1,nlevsoil)        ! thermal conductivity, soil minerals  [W/m-K]
   ! real(kind_phys), intent(in) :: tkdry(1,nlevsoil)       ! thermal conductivity, dry soil (W/m/Kelvin)
   ! real(kind_phys), intent(in) :: csol(1,nlevsoil)        ! heat capacity, soil solids (J/m**3/Kelvin)
    real(kind_phys), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)          ! layer thickness (m)
    real(kind_phys), intent(in) :: zi(1,-nlevsnow+0:nlevsoil)          ! interface level below a "z" level (m)
    real(kind_phys), intent(in) :: z(1,-nlevsnow+1:nlevsoil)           ! layer depth (m)
    real(kind_phys), intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)    ! soil temperature (Kelvin)
    real(kind_phys), intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  ! liquid water (kg/m2)
    real(kind_phys), intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  ! ice lens (kg/m2)

    !out
    real(kind_phys), intent(out) :: cv(lbc:ubc,-nlevsnow+1:nlevsoil) ! heat capacity [J/(m2 K)]
    real(kind_phys), intent(out) :: tk(lbc:ubc,-nlevsnow+1:nlevsoil) ! thermal conductivity [W/(m K)]
    real(kind_phys), intent(out) :: tktopsoillay(lbc:ubc)          ! thermal conductivity [W/(m K)]
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !CALLED FROM:
    ! subroutine ShalLakeTemperature in this module.
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    ! 2/13/02, Peter Thornton: migrated to new data structures
    ! 7/01/03, Mariana Vertenstein: migrated to vector code
    ! 4/09, Zack Subin, adjustment for ShalLake code.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !LOCAL VARIABLES:
    !
    ! local pointers to original implicit in scalars
    !
    !    integer , pointer :: clandunit(:)     ! column's landunit
    !    integer , pointer :: ityplun(:)       ! landunit type
    !
    !EOP
    
    
    ! OTHER LOCAL VARIABLES:
    
    integer  :: l,c,j                     ! indices
    integer  :: fc                        ! lake filtered column indices
    real(kind_phys) :: bw                        ! partial density of water (ice + liquid)
    real(kind_phys) :: dksat                     ! thermal conductivity for saturated soil (j/(k s m))
    real(kind_phys) :: dke                       ! kersten number
    real(kind_phys) :: fl                        ! fraction of liquid or unfrozen water to total water
    real(kind_phys) :: satw                      ! relative total water content of soil.
    real(kind_phys) :: thk(lbc:ubc,-nlevsnow+1:nlevsoil) ! thermal conductivity of layer
    character*256 :: message 

    ! Thermal conductivity of soil from Farouki (1981)

    do j = -nlevsnow+1,nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          ! Only examine levels from 1->nlevsoil
          if (j >= 1) then
            !             l = clandunit(c)
            !             if (ityplun(l) /= istwet .AND. ityplun(l) /= istice) then
              ! This could be altered later for allowing this to be over glaciers.

          ! Soil should be saturated.
#if (defined LAKEDEBUG)
                satw = (h2osoi_liq(c,j)/denh2o + h2osoi_ice(c,j)/denice)/(dz(c,j)*watsat(c,j))
                !                satw = min(1._kind_phys, satw)
                if (satw < 0.999_kind_phys) then
                   write(message,*)'WARNING: soil layer unsaturated in SoilThermProp_Lake, satw, j = ', satw, j
                   errmsg=trim(message)
                   errflg=1
                   return
                end if
          ! Could use denice because if it starts out frozen, the volume of water will go below sat.,
          ! since we're not yet doing excess ice.
          ! But take care of this in HydrologyLake.
#endif
                satw = 1._kind_phys
                   fl = h2osoi_liq(c,j)/(h2osoi_ice(c,j)+h2osoi_liq(c,j))
                   if (t_soisno(c,j) >= tfrz) then       ! Unfrozen soil
                      dke = max(0._kind_phys, log10(satw) + 1.0_kind_phys)
                      dksat = tksatu(c,j)
                   else                               ! Frozen soil
                      dke = satw
                      dksat = tkmg(c,j)*0.249_kind_phys**(fl*watsat(c,j))*2.29_kind_phys**watsat(c,j)
                   endif
                   thk(c,j) = dke*dksat + (1._kind_phys-dke)*tkdry(c,j)
                   !             else
                   !                thk(c,j) = tkwat
                   !                if (t_soisno(c,j) < tfrz) thk(c,j) = tkice
                   !             endif
          endif

          ! Thermal conductivity of snow, which from Jordan (1991) pp. 18
          ! Only examine levels from snl(c)+1 -> 0 where snl(c) < 1
          if (snl(c)+1 < 1 .AND. (j >= snl(c)+1) .AND. (j <= 0)) then
             bw = (h2osoi_ice(c,j)+h2osoi_liq(c,j))/dz(c,j)
             thk(c,j) = tkairc + (7.75e-5_kind_phys *bw + 1.105e-6_kind_phys*bw*bw)*(tkice-tkairc)
          end if

       end do
    end do

    ! Thermal conductivity at the layer interface

    ! Have to correct for the fact that bottom snow layer and top soil layer border lake.
    ! For the first case, the snow layer conductivity for the middle of the layer will be returned.
    ! Because the interfaces are below the soil layers, the conductivity for the top soil layer
    ! will have to be returned separately.
    do j = -nlevsnow+1,nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (j >= snl(c)+1 .AND. j <= nlevsoil-1 .AND. j /= 0) then
             tk(c,j) = thk(c,j)*thk(c,j+1)*(z(c,j+1)-z(c,j)) &
                  /(thk(c,j)*(z(c,j+1)-zi(c,j))+thk(c,j+1)*(zi(c,j)-z(c,j)))
          else if (j == 0) then
             tk(c,j) = thk(c,j)
          else if (j == nlevsoil) then
             tk(c,j) = 0._kind_phys
          end if
          ! For top soil layer.
          if (j == 1) tktopsoillay(c) = thk(c,j)
       end do
    end do

    ! Soil heat capacity, from de Vires (1963)

    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          !          l = clandunit(c)
          !          if (ityplun(l) /= istwet .AND. ityplun(l) /= istice) then
             cv(c,j) = csol(c,j)*(1-watsat(c,j))*dz(c,j) +   &
               (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
             !          else
             !             cv(c,j) = (h2osoi_ice(c,j)*cpice + h2osoi_liq(c,j)*cpliq)
             !          endif
             !          if (j == 1) then
             !             if (snl(c)+1 == 1 .AND. h2osno(c) > 0._kind_phys) then
             !                cv(c,j) = cv(c,j) + cpice*h2osno(c)
             !             end if
             !          end if
             ! Won't worry about heat capacity for thin snow on lake with no snow layers.
       enddo
    end do

    ! Snow heat capacity

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
      do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          if (snl(c)+1 < 1 .and. j >= snl(c)+1) then
             cv(c,j) = cpliq*h2osoi_liq(c,j) + cpice*h2osoi_ice(c,j)
          end if
       end do
    end do

  end subroutine SoilThermProp_Lake


  !-----------------------------------------------------------------------
  !BOP
  !
  ! ROUTINE: PhaseChange_Lake
  !
  ! !INTERFACE:
  subroutine PhaseChange_Lake (snl,h2osno,dz,dz_lake,                        & !i
                               t_soisno,h2osoi_liq,h2osoi_ice,               & !i&o
                               lake_icefrac,t_lake, snowdp,                  & !i&o
                               qflx_snomelt,eflx_snomelt,imelt,              & !o  
                               cv, cv_lake,                                  & !i&o
                               lhabs)                                          !o
    !=============================================================================================
    ! !DESCRIPTION:
    ! Calculation of the phase change within snow, soil, & lake layers:
    ! (1) Check the conditions for which the phase change may take place,
    !     i.e., the layer temperature is great than the freezing point
    !     and the ice mass is not equal to zero (i.e. melting),
    !     or the layer temperature is less than the freezing point
    !     and the liquid water mass is greater than the allowable supercooled 
    !    (i.e. freezing).
    ! (2) Assess the amount of phase change from the energy excess (or deficit)
    !     after setting the layer temperature to freezing point, depending on
    !     how much water or ice is available.
    ! (3) Re-adjust the ice and liquid mass, and the layer temperature: either to
    !     the freezing point if enough water or ice is available to fully compensate,
    !     or to a remaining temperature.
    ! The specific heats are assumed constant. Potential cycling errors resulting from
    ! this assumption will be trapped at the end of ShalLakeTemperature.
    ! !CALLED FROM:
    ! subroutine ShalLakeTemperature in this module
    !
    ! !REVISION HISTORY:
    ! 04/2009 Zack Subin: Initial code
    !==============================================================================================
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    !in: 

    integer , intent(in) :: snl(1)           !number of snow layers
    real(kind_phys), intent(inout) :: h2osno(1)        !snow water (mm H2O)
    real(kind_phys), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)          !layer thickness (m)
    real(kind_phys), intent(in) :: dz_lake(1,nlevlake)     !lake layer thickness (m)
    ! Needed in case snow height is less than critical value.

    !inout: 

    real(kind_phys), intent(inout) :: snowdp(1)        !snow height (m)
    real(kind_phys), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     !soil temperature (Kelvin)
    real(kind_phys), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   !liquid water (kg/m2)
    real(kind_phys), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   !ice lens (kg/m2)
    real(kind_phys), intent(inout) :: lake_icefrac(1,nlevlake) ! mass fraction of lake layer that is frozen
    real(kind_phys), intent(inout) :: t_lake(1,nlevlake)       ! lake temperature (Kelvin)
    !out: 

    real(kind_phys), intent(out) :: qflx_snomelt(1)  !snow melt (mm H2O /s)
    real(kind_phys), intent(out) :: eflx_snomelt(1)  !snow melt heat flux (W/m**2)
    integer, intent(out)  :: imelt(1,-nlevsnow+1:nlevsoil)        !flag for melting (=1), freezing (=2), Not=0 (new)
                                          !What's the sign of this? Is it just output?
    real(kind_phys), intent(inout) :: cv(lbc:ubc,-nlevsnow+1:nlevsoil)       ! heat capacity [J/(m2 K)]
    real(kind_phys), intent(inout) :: cv_lake (lbc:ubc,1:nlevlake)          ! heat capacity [J/(m2 K)]
    real(kind_phys), intent(out):: lhabs(lbc:ubc)                       ! total per-column latent heat abs. (J/m^2)


    ! OTHER LOCAL VARIABLES:

    integer  :: j,c,g                              !do loop index
    integer  :: fc                                 !lake filtered column indices
    !    real(kind_phys) :: dtime                              !land model time step (sec)
    real(kind_phys) :: heatavail                          !available energy for melting or freezing (J/m^2)
    real(kind_phys) :: heatrem                            !energy residual or loss after melting or freezing
    real(kind_phys) :: melt                               !actual melting (+) or freezing (-) [kg/m2]
    real(kind_phys), parameter :: smallnumber = 1.e-7_kind_phys  !to prevent tiny residuals from rounding error
    logical  :: dophasechangeflag
    !-----------------------------------------------------------------------
    
    !    dtime = get_step_size()
    
    ! Initialization

    !dir$ concurrent
    !cdir nodep
    do fc = 1,num_shlakec
       c = filter_shlakec(fc)

       qflx_snomelt(c) = 0._kind_phys
       eflx_snomelt(c) = 0._kind_phys
       lhabs(c)        = 0._kind_phys
    end do

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          if (j >= snl(c) + 1) imelt(c,j) = 0
       end do
    end do

    ! Check for case of snow without snow layers and top lake layer temp above freezing.

    !dir$ concurrent
    !cdir nodep
    do fc = 1,num_shlakec
       c = filter_shlakec(fc)

       if (snl(c) == 0 .and. h2osno(c) > 0._kind_phys .and. t_lake(c,1) > tfrz) then
          heatavail = (t_lake(c,1) - tfrz) * cv_lake(c,1)
          melt = min(h2osno(c), heatavail/hfus)
          heatrem = max(heatavail - melt*hfus, 0._kind_phys)
                       !catch small negative value to keep t at tfrz
          t_lake(c,1) = tfrz + heatrem/(cv_lake(c,1))
          snowdp(c) = snowdp(c)*(1._kind_phys - melt/h2osno(c))
          h2osno(c) = h2osno(c) - melt
          lhabs(c) = lhabs(c) + melt*hfus
          qflx_snomelt(c) = qflx_snomelt(c) + melt
          ! Prevent tiny residuals
          if (h2osno(c) < smallnumber) h2osno(c) = 0._kind_phys
          if (snowdp(c) < smallnumber) snowdp(c) = 0._kind_phys
       end if
    end do

    ! Lake phase change

    do j = 1,nlevlake
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)

          dophasechangeflag = .false.
          if (t_lake(c,j) > tfrz .and. lake_icefrac(c,j) > 0._kind_phys) then ! melting
             dophasechangeflag = .true.
             heatavail = (t_lake(c,j) - tfrz) * cv_lake(c,j)
             melt = min(lake_icefrac(c,j)*denh2o*dz_lake(c,j), heatavail/hfus)
                        !denh2o is used because layer thickness is not adjusted for freezing
             heatrem = max(heatavail - melt*hfus, 0._kind_phys)
                       !catch small negative value to keep t at tfrz
          else if (t_lake(c,j) < tfrz .and. lake_icefrac(c,j) < 1._kind_phys) then !freezing
             dophasechangeflag = .true.
             heatavail = (t_lake(c,j) - tfrz) * cv_lake(c,j)
             melt = max(-(1._kind_phys-lake_icefrac(c,j))*denh2o*dz_lake(c,j), heatavail/hfus)
                        !denh2o is used because layer thickness is not adjusted for freezing
             heatrem = min(heatavail - melt*hfus, 0._kind_phys)
                       !catch small positive value to keep t at tfrz
          end if
          ! Update temperature and ice fraction.
          if (dophasechangeflag) then
             lake_icefrac(c,j) = lake_icefrac(c,j) - melt/(denh2o*dz_lake(c,j))
             lhabs(c) = lhabs(c) + melt*hfus
          ! Update heat capacity
             cv_lake(c,j) = cv_lake(c,j) + melt*(cpliq-cpice)
             t_lake(c,j) = tfrz + heatrem/cv_lake(c,j)
             ! Prevent tiny residuals
             if (lake_icefrac(c,j) > 1._kind_phys - smallnumber) lake_icefrac(c,j) = 1._kind_phys
             if (lake_icefrac(c,j) < smallnumber)         lake_icefrac(c,j) = 0._kind_phys
          end if
       end do
    end do

    ! Snow & soil phase change

    do j = -nlevsnow+1,nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1,num_shlakec
          c = filter_shlakec(fc)
          dophasechangeflag = .false.

          if (j >= snl(c) + 1) then

             if (t_soisno(c,j) > tfrz .and. h2osoi_ice(c,j) > 0._kind_phys) then ! melting
                dophasechangeflag = .true.
                heatavail = (t_soisno(c,j) - tfrz) * cv(c,j)
                melt = min(h2osoi_ice(c,j), heatavail/hfus)
                heatrem = max(heatavail - melt*hfus, 0._kind_phys)
                          !catch small negative value to keep t at tfrz
                if (j <= 0) then !snow
                   imelt(c,j) = 1
                   qflx_snomelt(c) = qflx_snomelt(c) + melt
                end if
             else if (t_soisno(c,j) < tfrz .and. h2osoi_liq(c,j) > 0._kind_phys) then !freezing
                dophasechangeflag = .true.
                heatavail = (t_soisno(c,j) - tfrz) * cv(c,j)
                melt = max(-h2osoi_liq(c,j), heatavail/hfus)
                heatrem = min(heatavail - melt*hfus, 0._kind_phys)
                          !catch small positive value to keep t at tfrz
                if (j <= 0) then !snow
                   imelt(c,j) = 2
                   qflx_snomelt(c) = qflx_snomelt(c) + melt
                   ! Does this works for both signs of melt in SnowHydrology? I think
                   ! qflx_snomelt(c) is just output.
                end if
             end if

             ! Update temperature and soil components.
             if (dophasechangeflag) then
                h2osoi_ice(c,j) = h2osoi_ice(c,j) - melt
                h2osoi_liq(c,j) = h2osoi_liq(c,j) + melt
                lhabs(c) = lhabs(c) + melt*hfus
             ! Update heat capacity
                cv(c,j) = cv(c,j) + melt*(cpliq-cpice)
                t_soisno(c,j) = tfrz + heatrem/cv(c,j)
                ! Prevent tiny residuals
                if (h2osoi_ice(c,j) < smallnumber) h2osoi_ice(c,j) = 0._kind_phys
                if (h2osoi_liq(c,j) < smallnumber) h2osoi_liq(c,j) = 0._kind_phys
             end if

         end if
      end do
   end do

   ! Update eflx_snomelt(c)
   !dir$ concurrent
   !cdir nodep
    do fc = 1,num_shlakec
       c = filter_shlakec(fc)
       eflx_snomelt(c) = qflx_snomelt(c)*hfus
    end do
   !!!

   end subroutine PhaseChange_Lake


  subroutine ShalLakeHydrology(dz_lake,forc_rain,forc_snow,                      & !i
                               begwb,qflx_evap_tot,forc_t,do_capsnow,            &
                               t_grnd,qflx_evap_soi,                             &
                               qflx_snomelt,imelt,frac_iceold,                   & !i add by guhp
                               z,dz,zi,snl,h2osno,snowdp,lake_icefrac,t_lake,      & !i&o
                               endwb,snowage,snowice,snowliq,t_snow,             & !o
                               t_soisno,h2osoi_ice,h2osoi_liq,h2osoi_vol,        &
                               qflx_drain,qflx_surf,qflx_infl,qflx_qrgwl,        &
                               qcharge,qflx_prec_grnd,qflx_snowcap,              &
                               qflx_snowcap_col,qflx_snow_grnd_pft,              &
                               qflx_snow_grnd_col,qflx_rain_grnd,                &
                               qflx_evap_tot_col,soilalpha,zwt,fcov,             &
                               rootr_column,qflx_evap_grnd,qflx_sub_snow,        &
                               qflx_dew_snow,qflx_dew_grnd,qflx_rain_grnd_col,   &
                               errmsg,errflg)
                       
    !==================================================================================
    ! !DESCRIPTION:
    ! Calculation of Shallow Lake Hydrology. Full hydrology of snow layers is
    ! done. However, there is no infiltration, and the water budget is balanced with 
    ! qflx_qrgwl. Lake water mass is kept constant. The soil is simply maintained at
    ! volumetric saturation if ice melting frees up pore space. Likewise, if the water
    ! portion alone at some point exceeds pore capacity, it is reduced. This is consistent
    ! with the possibility of initializing the soil layer with excess ice. The only
    ! real error with that is that the thermal conductivity will ignore the excess ice
    ! (and accompanying thickness change).
    ! 
    ! If snow layers are present over an unfrozen lake, and the top layer of the lake
    ! is capable of absorbing the latent heat without going below freezing, 
    ! the snow-water is runoff and the latent heat is subtracted from the lake.
    !
    ! WARNING: This subroutine assumes lake columns have one and only one pft.
    !
    ! Sequence is:
    !  ShalLakeHydrology:
    !    Do needed tasks from Hydrology1, Biogeophysics2, & top of Hydrology2.
    !    -> SnowWater:             change of snow mass and snow water onto soil
    !    -> SnowCompaction:        compaction of snow layers
    !    -> CombineSnowLayers:     combine snow layers that are thinner than minimum
    !    -> DivideSnowLayers:      subdivide snow layers that are thicker than maximum
    !    Add water to soil if melting has left it with open pore space.
    !    Cleanup and do water balance.
    !    If snow layers are found above a lake with unfrozen top layer, whose top
    !    layer has enough heat to melt all the snow ice without freezing, do so
    !    and eliminate the snow layers.
    !
    ! !REVISION HISTORY:
    ! Created by Zack Subin, 2009
    !
    !============================================================================================
    
    ! USES:
    !
    implicit none

    ! in:

    integer, intent(inout) :: errflg
    character(*), intent(inout) :: errmsg

   ! integer , intent(in) :: clandunit(1)     ! column's landunit
   ! integer , intent(in) :: ityplun(1)       ! landunit type
   ! real(kind_phys), intent(in) :: watsat(1,1:nlevsoil)      ! volumetric soil water at saturation (porosity)
    real(kind_phys), intent(in) :: dz_lake(1,nlevlake)     ! layer thickness for lake (m)
    real(kind_phys), intent(in) :: forc_rain(1)     ! rain rate [mm/s]
    real(kind_phys), intent(in) :: forc_snow(1)     ! snow rate [mm/s]
    real(kind_phys), intent(in) :: qflx_evap_tot(1) ! qflx_evap_soi + qflx_evap_veg + qflx_tran_veg
    real(kind_phys), intent(in) :: forc_t(1)        ! atmospheric temperature (Kelvin)
#if (defined OFFLINE)
    real(kind_phys), intent(in) :: flfall(1)        ! fraction of liquid water within falling precipitation
#endif
    logical , intent(in) :: do_capsnow(1)     ! true => do snow capping
    real(kind_phys), intent(in) :: t_grnd(1)          ! ground temperature (Kelvin)
    real(kind_phys), intent(in) :: qflx_evap_soi(1)   ! soil evaporation (mm H2O/s) (+ = to atm)
    real(kind_phys), intent(in) :: qflx_snomelt(1)     !snow melt (mm H2O /s)
    integer,  intent(in) :: imelt(1,-nlevsnow+1:nlevsoil)        !flag for melting (=1), freezing (=2), Not=0

    !inout:

    real(kind_phys), intent(inout) :: begwb(1)         ! water mass begining of the time step

    ! inout: 

    
    real(kind_phys), intent(inout) :: z(1,-nlevsnow+1:nlevsoil)           ! layer depth  (m)
    real(kind_phys), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)          ! layer thickness depth (m)
    real(kind_phys), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)          ! interface depth (m)
    integer , intent(inout) :: snl(1)           ! number of snow layers
    real(kind_phys), intent(inout) :: h2osno(1)        ! snow water (mm H2O)
    real(kind_phys), intent(inout) :: snowdp(1)        ! snow height (m)
    real(kind_phys), intent(inout) :: lake_icefrac(1,nlevlake)  ! mass fraction of lake layer that is frozen
    real(kind_phys), intent(inout) :: t_lake(1,nlevlake)        ! lake temperature (Kelvin)

    real(kind_phys), intent(inout) :: frac_iceold(1,-nlevsnow+1:nlevsoil)      ! fraction of ice relative to the tot water
    ! out: 


    real(kind_phys), intent(out) :: endwb(1)         ! water mass end of the time step
    real(kind_phys), intent(out) :: snowage(1)       ! non dimensional snow age [-]
    real(kind_phys), intent(out) :: snowice(1)       ! average snow ice lens
    real(kind_phys), intent(out) :: snowliq(1)       ! average snow liquid water
    real(kind_phys), intent(out) :: t_snow(1)        ! vertically averaged snow temperature
    real(kind_phys), intent(out) :: t_soisno(1,-nlevsnow+1:nlevsoil)    ! snow temperature (Kelvin)
    real(kind_phys), intent(out) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)  ! ice lens (kg/m2)
    real(kind_phys), intent(out) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)  ! liquid water (kg/m2)
    real(kind_phys), intent(out) :: h2osoi_vol(1,-nlevsnow+1:nlevsoil)  ! volumetric soil water (0<=h2osoi_vol<=watsat)[m3/m3]
    real(kind_phys), intent(out) :: qflx_drain(1)    ! sub-surface runoff (mm H2O /s)
    real(kind_phys), intent(out) :: qflx_surf(1)     ! surface runoff (mm H2O /s)
    real(kind_phys), intent(out) :: qflx_infl(1)     ! infiltration (mm H2O /s)
    real(kind_phys), intent(out) :: qflx_qrgwl(1)    ! qflx_surf at glaciers, wetlands, lakes
    real(kind_phys), intent(out) :: qcharge(1)       ! aquifer recharge rate (mm/s)
    real(kind_phys), intent(out) :: qflx_prec_grnd(1)     ! water onto ground including canopy runoff [kg/(m2 s)]
    real(kind_phys), intent(out) :: qflx_snowcap(1)       ! excess precipitation due to snow capping (mm H2O /s) [+]
    real(kind_phys), intent(out) :: qflx_snowcap_col(1)   ! excess precipitation due to snow capping (mm H2O /s) [+]
    real(kind_phys), intent(out) :: qflx_snow_grnd_pft(1) ! snow on ground after interception (mm H2O/s) [+]
    real(kind_phys), intent(out) :: qflx_snow_grnd_col(1) ! snow on ground after interception (mm H2O/s) [+]
    real(kind_phys), intent(out) :: qflx_rain_grnd(1)     ! rain on ground after interception (mm H2O/s) [+]
    real(kind_phys), intent(out) :: qflx_evap_tot_col(1) !pft quantity averaged to the column (assuming one pft)
    real(kind_phys) ,intent(out) :: soilalpha(1)     !factor that reduces ground saturated specific humidity (-)
    real(kind_phys), intent(out) :: zwt(1)           !water table depth
    real(kind_phys), intent(out) :: fcov(1)          !fractional area with water table at surface
    real(kind_phys), intent(out) :: rootr_column(1,1:nlevsoil) !effective fraction of roots in each soil layer
    real(kind_phys), intent(out) :: qflx_evap_grnd(1)  ! ground surface evaporation rate (mm H2O/s) [+]
    real(kind_phys), intent(out) :: qflx_sub_snow(1)   ! sublimation rate from snow pack (mm H2O /s) [+]
    real(kind_phys), intent(out) :: qflx_dew_snow(1)   ! surface dew added to snow pack (mm H2O /s) [+]
    real(kind_phys), intent(out) :: qflx_dew_grnd(1)   ! ground surface dew formation (mm H2O /s) [+]
    real(kind_phys), intent(out) :: qflx_rain_grnd_col(1)   !rain on ground after interception (mm H2O/s) [+]

    ! Block of biogeochem currently not used.
#ifndef SHLAKE
    real(kind_phys), pointer :: sucsat(:,:)      ! minimum soil suction (mm)
    real(kind_phys), pointer :: bsw(:,:)         ! Clapp and Hornberger "b"
    real(kind_phys), pointer :: bsw2(:,:)        ! Clapp and Hornberger "b" for CN code
    real(kind_phys), pointer :: psisat(:,:)      ! soil water potential at saturation for CN code (MPa)
    real(kind_phys), pointer :: vwcsat(:,:)      ! volumetric water content at saturation for CN code (m3/m3)
    real(kind_phys), pointer :: wf(:)            ! soil water as frac. of whc for top 0.5 m
    real(kind_phys), pointer :: soilpsi(:,:)     ! soil water potential in each soil layer (MPa)
    real(kind_phys) :: psi,vwc,fsat               ! temporary variables for soilpsi calculation
#if (defined DGVM) || (defined CN)
    real(kind_phys) :: watdry                     ! temporary
    real(kind_phys) :: rwat(lbc:ubc)              ! soil water wgted by depth to maximum depth of 0.5 m
    real(kind_phys) :: swat(lbc:ubc)              ! same as rwat but at saturation
    real(kind_phys) :: rz(lbc:ubc)                ! thickness of soil layers contributing to rwat (m)
    real(kind_phys) :: tsw                        ! volumetric soil water to 0.5 m
    real(kind_phys) :: stsw                       ! volumetric soil water to 0.5 m at saturation
#endif
#endif


    ! OTHER LOCAL VARIABLES:

    integer  :: p,fp,g,l,c,j,fc,jtop             ! indices
    integer  :: num_shlakesnowc                  ! number of column snow points
    integer  :: filter_shlakesnowc(ubc-lbc+1)    ! column filter for snow points
    integer  :: num_shlakenosnowc                ! number of column non-snow points
    integer  :: filter_shlakenosnowc(ubc-lbc+1)  ! column filter for non-snow points
    !    real(kind_phys) :: dtime                      ! land model time step (sec)
    integer  :: newnode                      ! flag when new snow node is set, (1=yes, 0=no)
    real(kind_phys) :: dz_snowf                     ! layer thickness rate change due to precipitation [mm/s]
    real(kind_phys) :: bifall                       ! bulk density of newly fallen dry snow [kg/m3]
    real(kind_phys) :: fracsnow(lbp:ubp)            ! frac of precipitation that is snow
    real(kind_phys) :: fracrain(lbp:ubp)            ! frac of precipitation that is rain
    real(kind_phys) :: qflx_prec_grnd_snow(lbp:ubp) ! snow precipitation incident on ground [mm/s]
    real(kind_phys) :: qflx_prec_grnd_rain(lbp:ubp) ! rain precipitation incident on ground [mm/s]
    real(kind_phys) :: qflx_evap_soi_lim            ! temporary evap_soi limited by top snow layer content [mm/s]
    real(kind_phys) :: h2osno_temp                  ! temporary h2osno [kg/m^2]
    real(kind_phys), parameter :: snow_bd = 250._kind_phys  !constant snow bulk density (only used in special case here) [kg/m^3]
    real(kind_phys) :: sumsnowice(lbc:ubc)             ! sum of snow ice if snow layers found above unfrozen lake [kg/m&2]
    logical  :: unfrozen(lbc:ubc)            ! true if top lake layer is unfrozen with snow layers above
    real(kind_phys) :: heatrem                      ! used in case above [J/m^2]
    real(kind_phys) :: heatsum(lbc:ubc)             ! used in case above [J/m^2]
    real(kind_phys) :: qflx_top_soil(1)     !net water input into soil from top (mm/s)
    character*256 :: message 

#if (defined LAKEDEBUG)
    real(kind_phys) :: snow_water(lbc:ubc)           ! temporary sum of snow water for Bal Check [kg/m^2]
#endif
    !-----------------------------------------------------------------------


    ! Determine step size

    !    dtime = get_step_size()

    ! Add soil water to water balance.
    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
      do fc = 1, num_shlakec
         c = filter_shlakec(fc)
         begwb(c) = begwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
      end do
    end do

    !!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Do precipitation onto ground, etc., from Hydrology1.

    !dir$ concurrent
    !cdir nodep
    do fp = 1, num_shlakep
       p = filter_shlakep(fp)
       g = pgridcell(p)
       !       l = plandunit(p)
       c = pcolumn(p)

       ! Precipitation onto ground (kg/(m2 s))
       !       ! PET, 1/18/2005: Added new terms for mass balance correction
       !       ! due to dynamic pft weight shifting (column-level h2ocan_loss)
       !       ! Because the fractionation between rain and snow is indeterminate if
       !       ! rain + snow = 0, I am adding this very small flux only to the rain
       !       ! components.
       ! Not relevant unless PFTs are added to lake later.
       !       if (frac_veg_nosno(p) == 0) then
          qflx_prec_grnd_snow(p) = forc_snow(g)
          qflx_prec_grnd_rain(p) = forc_rain(g) !+ h2ocan_loss(c)
          !       else
          !          qflx_prec_grnd_snow(p) = qflx_through_snow(p) + (qflx_candrip(p) * fracsnow(p))
          !          qflx_prec_grnd_rain(p) = qflx_through_rain(p) + (qflx_candrip(p) * fracrain(p)) + h2ocan_loss(c)
          !       end if
       qflx_prec_grnd(p) = qflx_prec_grnd_snow(p) + qflx_prec_grnd_rain(p)

       if (do_capsnow(c)) then
          qflx_snowcap(p) = qflx_prec_grnd_snow(p) + qflx_prec_grnd_rain(p)
          qflx_snow_grnd_pft(p) = 0._kind_phys
          qflx_rain_grnd(p) = 0._kind_phys
       else
          qflx_snowcap(p) = 0._kind_phys
#if (defined OFFLINE)
          qflx_snow_grnd_pft(p) = qflx_prec_grnd(p)*(1._kind_phys-flfall(g)) ! ice onto ground (mm/s)
          qflx_rain_grnd(p)     = qflx_prec_grnd(p)*flfall(g)      ! liquid water onto ground (mm/s)
#else
          qflx_snow_grnd_pft(p) = qflx_prec_grnd_snow(p)           ! ice onto ground (mm/s)
          qflx_rain_grnd(p)     = qflx_prec_grnd_rain(p)           ! liquid water onto ground (mm/s)
#endif
       end if
       ! Assuming one PFT; needed for below
       qflx_snow_grnd_col(c) = qflx_snow_grnd_pft(p)
       qflx_rain_grnd_col(c) = qflx_rain_grnd(p)

    end do ! (end pft loop)

    ! Determine snow height and snow water

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakec
       c = filter_shlakec(fc)
       !       l = clandunit(c)
       g = cgridcell(c)

       ! Use Alta relationship, Anderson(1976); LaChapelle(1961),
       ! U.S.Department of Agriculture Forest Service, Project F,
       ! Progress Rep. 1, Alta Avalanche Study Center:Snow Layer Densification.

       if (do_capsnow(c)) then
          dz_snowf = 0._kind_phys
       else
          if (forc_t(g) > tfrz + 2._kind_phys) then
             bifall=50._kind_phys + 1.7_kind_phys*(17.0_kind_phys)**1.5_kind_phys
          else if (forc_t(g) > tfrz - 15._kind_phys) then
             bifall=50._kind_phys + 1.7_kind_phys*(forc_t(g) - tfrz + 15._kind_phys)**1.5_kind_phys
          else
             bifall=50._kind_phys
          end if
          dz_snowf = qflx_snow_grnd_col(c)/bifall
          snowdp(c) = snowdp(c) + dz_snowf*dtime
          h2osno(c) = h2osno(c) + qflx_snow_grnd_col(c)*dtime  ! snow water equivalent (mm)
       end if

       !       if (itype(l)==istwet .and. t_grnd(c)>tfrz) then
       !          h2osno(c)=0._kind_phys
       !          snowdp(c)=0._kind_phys
       !          snowage(c)=0._kind_phys
       !       end if
       ! Take care of this later in function.

       ! When the snow accumulation exceeds 10 mm, initialize snow layer
       ! Currently, the water temperature for the precipitation is simply set
       ! as the surface air temperature

       newnode = 0    ! flag for when snow node will be initialized
       if (snl(c) == 0 .and. qflx_snow_grnd_col(c) > 0.0_kind_phys .and. snowdp(c) >= 0.01_kind_phys) then
          newnode = 1
          snl(c) = -1
          dz(c,0) = snowdp(c)                       ! meter
          z(c,0) = -0.5_kind_phys*dz(c,0)
          zi(c,-1) = -dz(c,0)
          snowage(c) = 0._kind_phys                        ! snow age
          t_soisno(c,0) = min(tfrz, forc_t(g))      ! K
          h2osoi_ice(c,0) = h2osno(c)               ! kg/m2
          h2osoi_liq(c,0) = 0._kind_phys                   ! kg/m2
          frac_iceold(c,0) = 1._kind_phys
       end if

       ! The change of ice partial density of surface node due to precipitation.
       ! Only ice part of snowfall is added here, the liquid part will be added
       ! later.

       if (snl(c) < 0 .and. newnode == 0) then
          h2osoi_ice(c,snl(c)+1) = h2osoi_ice(c,snl(c)+1)+dtime*qflx_snow_grnd_col(c)
          dz(c,snl(c)+1) = dz(c,snl(c)+1)+dz_snowf*dtime
       end if

    end do

    ! Calculate sublimation and dew, adapted from HydrologyLake and Biogeophysics2.

    !dir$ concurrent
    !cdir nodep
    do fp = 1,num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       jtop = snl(c)+1

       ! Use column variables here
       qflx_evap_grnd(c) = 0._kind_phys
       qflx_sub_snow(c) = 0._kind_phys
       qflx_dew_snow(c) = 0._kind_phys
       qflx_dew_grnd(c) = 0._kind_phys

       if (jtop <= 0) then ! snow layers
          j = jtop
          ! Assign ground evaporation to sublimation from soil ice or to dew
          ! on snow or ground

          if (qflx_evap_soi(p) >= 0._kind_phys) then
          ! for evaporation partitioning between liquid evap and ice sublimation, 
          ! use the ratio of liquid to (liquid+ice) in the top layer to determine split
          ! Since we're not limiting evap over lakes, but still can't remove more from top
          ! snow layer than there is there, create temp. limited evap_soi.
             qflx_evap_soi_lim = min(qflx_evap_soi(p), (h2osoi_liq(c,j)+h2osoi_ice(c,j))/dtime)
             if ((h2osoi_liq(c,j)+h2osoi_ice(c,j)) > 0._kind_phys) then
                qflx_evap_grnd(c) = max(qflx_evap_soi_lim*(h2osoi_liq(c,j)/(h2osoi_liq(c,j)+h2osoi_ice(c,j))), 0._kind_phys)
             else
                qflx_evap_grnd(c) = 0._kind_phys
             end if
             qflx_sub_snow(c) = qflx_evap_soi_lim - qflx_evap_grnd(c)     
          else
             if (t_grnd(c) < tfrz) then
                qflx_dew_snow(c) = abs(qflx_evap_soi(p))
             else
                qflx_dew_grnd(c) = abs(qflx_evap_soi(p))
             end if
          end if
          ! Update the pft-level qflx_snowcap
          ! This was moved in from Hydrology2 to keep all pft-level
          ! calculations out of Hydrology2
          if (do_capsnow(c)) qflx_snowcap(p) = qflx_snowcap(p) + qflx_dew_snow(c) + qflx_dew_grnd(c)

       else ! No snow layers: do as in HydrologyLake but with actual clmtype variables
          if (qflx_evap_soi(p) >= 0._kind_phys) then
             ! Sublimation: do not allow for more sublimation than there is snow
             ! after melt.  Remaining surface evaporation used for infiltration.
             qflx_sub_snow(c) = min(qflx_evap_soi(p), h2osno(c)/dtime)
             qflx_evap_grnd(c) = qflx_evap_soi(p) - qflx_sub_snow(c)
          else
             if (t_grnd(c) < tfrz-0.1_kind_phys) then
                qflx_dew_snow(c) = abs(qflx_evap_soi(p))
             else
                qflx_dew_grnd(c) = abs(qflx_evap_soi(p))
             end if
          end if

          ! Update snow pack for dew & sub.
          h2osno_temp = h2osno(c)
          if (do_capsnow(c)) then
             h2osno(c) = h2osno(c) - qflx_sub_snow(c)*dtime
             qflx_snowcap(p) = qflx_snowcap(p) + qflx_dew_snow(c) + qflx_dew_grnd(c)
          else
             h2osno(c) = h2osno(c) + (-qflx_sub_snow(c)+qflx_dew_snow(c))*dtime
          end if
          if (h2osno_temp > 0._kind_phys) then
             snowdp(c) = snowdp(c) * h2osno(c) / h2osno_temp
          else
             snowdp(c) = h2osno(c)/snow_bd !Assume a constant snow bulk density = 250.
          end if

#if (defined PERGRO)
          if (abs(h2osno(c)) < 1.e-10_kind_phys) h2osno(c) = 0._kind_phys
#else
          h2osno(c) = max(h2osno(c), 0._kind_phys)
#endif

       end if

    qflx_snowcap_col(c) = qflx_snowcap(p)

    end do


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Determine initial snow/no-snow filters (will be modified possibly by
    ! routines CombineSnowLayers and DivideSnowLayers below

    call BuildSnowFilter(lbc, ubc, num_shlakec, filter_shlakec,snl,       &            !i
         num_shlakesnowc, filter_shlakesnowc, num_shlakenosnowc, filter_shlakenosnowc) !o

    ! Determine the change of snow mass and the snow water onto soil

    call SnowWater(lbc, ubc, num_shlakesnowc, filter_shlakesnowc,         & !i 
                   num_shlakenosnowc, filter_shlakenosnowc,               & !i 
                   snl,do_capsnow,qflx_snomelt,qflx_rain_grnd,            & !i 
                   qflx_sub_snow,qflx_evap_grnd,                          & !i   
                   qflx_dew_snow,qflx_dew_grnd,dz,                        & !i   
                   h2osoi_ice,h2osoi_liq,                                 & !i&o 
                   qflx_top_soil)                                           !o                        


    ! Determine soil hydrology
    ! Here this consists only of making sure that soil is saturated even as it melts and 10%
    ! of pore space opens up. Conversely, if excess ice is melting and the liquid water exceeds the
    ! saturation value, then remove water.

    do j = 1,nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (h2osoi_vol(c,j) < watsat(c,j)) then
             h2osoi_liq(c,j) = (watsat(c,j)*dz(c,j) - h2osoi_ice(c,j)/denice)*denh2o
          ! h2osoi_vol will be updated below, and this water addition will come from qflx_qrgwl
          else if (h2osoi_liq(c,j) > watsat(c,j)*denh2o*dz(c,j)) then
             h2osoi_liq(c,j) = watsat(c,j)*denh2o*dz(c,j)
          end if

       end do
    end do
    !!!!!!!!!!

    !    if (.not. is_perpetual()) then
    if (1==1) then

       ! Natural compaction and metamorphosis.

       call SnowCompaction(lbc, ubc, num_shlakesnowc, filter_shlakesnowc,   &!i
                           snl,imelt,frac_iceold,t_soisno,                  &!i
                           h2osoi_ice,h2osoi_liq,                           &!i
                           dz)                                               !&o

       ! Combine thin snow elements

       call CombineSnowLayers(lbc, ubc,                            & !i
                              num_shlakesnowc, filter_shlakesnowc, & !i&o
                              snl,h2osno,snowdp,dz,zi,             & !i&o
                              t_soisno,h2osoi_ice,h2osoi_liq,      & !i&o
                              z)  !o                              


       ! Divide thick snow elements

       call DivideSnowLayers(lbc, ubc,                             & !i
                             num_shlakesnowc, filter_shlakesnowc,  & !i&o
                             snl,dz,zi,t_soisno,                   & !i&o
                             h2osoi_ice,h2osoi_liq,                & !i&o
                             z)  !o


    else

       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          h2osno(c) = 0._kind_phys
       end do
       do j = -nlevsnow+1,0
          do fc = 1, num_shlakesnowc
             c = filter_shlakesnowc(fc)
             if (j >= snl(c)+1) then
                h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             end if
          end do
       end do

    end if

    ! Check for snow layers above lake with unfrozen top layer.  Mechanically,
    ! the snow will fall into the lake and melt or turn to ice.  If the top layer has
    ! sufficient heat to melt the snow without freezing, then that will be done.
    ! Otherwise, the top layer will undergo freezing, but only if the top layer will
    ! not freeze completely.  Otherwise, let the snow layers persist and melt by diffusion.
    !dir$ concurrent
    !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (t_lake(c,1) > tfrz .and. lake_icefrac(c,1) == 0._kind_phys .and. snl(c) < 0) then
             unfrozen(c) = .true.
          else
             unfrozen(c) = .false.
          end if
       end do

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (unfrozen(c)) then
             if (j == -nlevsnow+1) then
                sumsnowice(c) = 0._kind_phys
                heatsum(c) = 0._kind_phys
             end if
             if (j >= snl(c)+1) then
                sumsnowice(c) = sumsnowice(c) + h2osoi_ice(c,j)
                heatsum(c) = heatsum(c) + h2osoi_ice(c,j)*cpice*(tfrz - t_soisno(c,j)) &
                           + h2osoi_liq(c,j)*cpliq*(tfrz - t_soisno(c,j))
             end if
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)

          if (unfrozen(c)) then
             heatsum(c) = heatsum(c) + sumsnowice(c)*hfus
             heatrem = (t_lake(c,1) - tfrz)*cpliq*denh2o*dz_lake(c,1) - heatsum(c)

             if (heatrem + denh2o*dz_lake(c,1)*hfus > 0._kind_phys) then            
                ! Remove snow and subtract the latent heat from the top layer.
                h2osno(c) = 0._kind_phys
                snl(c) = 0
                ! The rest of the bookkeeping for the removed snow will be done below.
#if (defined LAKEDEBUG)
                pring *,'Snow layers removed above unfrozen lake for column, snowice:', &
                          c, sumsnowice(c)
#endif
                if (heatrem > 0._kind_phys) then ! simply subtract the heat from the layer
                   t_lake(c,1) = t_lake(c,1) - heatrem/(cpliq*denh2o*dz_lake(c,1))
                else !freeze part of the layer
                   t_lake(c,1) = tfrz
                   lake_icefrac(c,1) = -heatrem/(denh2o*dz_lake(c,1)*hfus)
                end if
             end if
          end if
       end do
    !!!!!!!!!!!!

    ! Set snow age to zero if no snow

       !dir$ concurrent
       !cdir nodep
    do fc = 1, num_shlakesnowc
       c = filter_shlakesnowc(fc)
       if (snl(c) == 0) then
          snowage(c) = 0._kind_phys
       end if
    end do

    ! Set empty snow layers to zero

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          if (j <= snl(c) .and. snl(c) > -nlevsnow) then
             h2osoi_ice(c,j) = 0._kind_phys
             h2osoi_liq(c,j) = 0._kind_phys
             t_soisno(c,j) = 0._kind_phys
             dz(c,j) = 0._kind_phys
             z(c,j) = 0._kind_phys
             zi(c,j-1) = 0._kind_phys
          end if
       end do
    end do

    ! Build new snow filter

    call BuildSnowFilter(lbc, ubc, num_shlakec, filter_shlakec, snl,&   !i
         num_shlakesnowc, filter_shlakesnowc, num_shlakenosnowc, filter_shlakenosnowc) !o

    ! Vertically average t_soisno and sum of h2osoi_liq and h2osoi_ice
    ! over all snow layers for history output

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakesnowc
       c = filter_shlakesnowc(fc)
       t_snow(c)  = 0._kind_phys
       snowice(c) = 0._kind_phys
       snowliq(c) = 0._kind_phys
    end do
    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakenosnowc
       c = filter_shlakenosnowc(fc)
       t_snow(c)  = spval
       snowice(c) = spval
       snowliq(c) = spval
    end do

    do j = -nlevsnow+1, 0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakesnowc
          c = filter_shlakesnowc(fc)
          if (j >= snl(c)+1) then
             t_snow(c)  = t_snow(c) + t_soisno(c,j)
             snowice(c) = snowice(c) + h2osoi_ice(c,j)
             snowliq(c) = snowliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do

    ! Determine ending water balance and volumetric soil water

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_shlakec
       
       c = filter_shlakec(fc)
       if (snl(c) < 0) t_snow(c) = t_snow(c)/abs(snl(c))
       endwb(c) = h2osno(c)
    end do

    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_shlakec
          c = filter_shlakec(fc)
          endwb(c) = endwb(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
          h2osoi_vol(c,j) = h2osoi_liq(c,j)/(dz(c,j)*denh2o) + h2osoi_ice(c,j)/(dz(c,j)*denice)
       end do
    end do

#if (defined LAKEDEBUG)
    ! Check to make sure snow water adds up correctly.
    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
      do fc = 1, num_shlakec
         c = filter_shlakec(fc)
 
         jtop = snl(c)+1
         if(j == jtop) snow_water(c) = 0._kind_phys
         if(j >= jtop) then
            snow_water(c) = snow_water(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
            if(j == 0 .and. abs(snow_water(c)-h2osno(c))>1.e-7_kind_phys) then
               write(message,*)'h2osno does not equal sum of snow layers in ShalLakeHydrology:', &
                         'column, h2osno, sum of snow layers =', c, h2osno(c), snow_water(c)
               errmsg=trim(message)
               errflg=1
               return
            end if
         end if
      end do
    end do
#endif

    !!!!!!!!!!!!!
    ! Do history variables and set special landunit runoff (adapted from end of HydrologyLake)
    !dir$ concurrent
    !cdir nodep
    do fp = 1,num_shlakep
       p = filter_shlakep(fp)
       c = pcolumn(p)
       g = pgridcell(p)

       qflx_infl(c)      = 0._kind_phys
       qflx_surf(c)      = 0._kind_phys
       qflx_drain(c)     = 0._kind_phys
       rootr_column(c,:) = spval
       soilalpha(c)      = spval
       zwt(c)            = spval
       fcov(c)           = spval
       qcharge(c)        = spval
       !       h2osoi_vol(c,:)   = spval

       ! Insure water balance using qflx_qrgwl
       qflx_qrgwl(c)     = forc_rain(g) + forc_snow(g) - qflx_evap_tot(p) - (endwb(c)-begwb(c))/dtime
#if (defined LAKEDEBUG)
    print *,'c, rain, snow, evap, endwb, begwb, qflx_qrgwl:', &
       c, forc_rain(g), forc_snow(g), qflx_evap_tot(p), endwb(c), begwb(c), qflx_qrgwl(c)
#endif

       ! The pft average must be done here for output to history tape
       qflx_evap_tot_col(c) = qflx_evap_tot(p)
    end do

    !!!!!!!!!!!!!
    !For now, bracket off the remaining biogeochem code.  May need to bring it back
    !to do soil carbon and methane beneath lakes.
#if (defined CN)
#ifndef SHLAKE
    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_soilc
          c = filter_soilc(fc)
          
          if (h2osoi_liq(c,j) > 0._kind_phys) then
             vwc = h2osoi_liq(c,j)/(dz(c,j)*denh2o)
            
             ! the following limit set to catch very small values of 
             ! fractional saturation that can crash the calculation of psi
           
             fsat = max(vwc/vwcsat(c,j), 0.001_kind_phys)
             psi = psisat(c,j) * (fsat)**bsw2(c,j)
             soilpsi(c,j) = min(max(psi,-15.0_kind_phys),0._kind_phys)
          else 
             soilpsi(c,j) = -15.0_kind_phys
          end if
       end do
    end do
#endif
#endif

#if (defined DGVM) || (defined CN)
#ifndef SHLAKE
    ! Available soil water up to a depth of 0.5 m.
    ! Potentially available soil water (=whc) up to a depth of 0.5 m.
    ! Water content as fraction of whc up to a depth of 0.5 m.

    !dir$ concurrent
    !cdir nodep
    do c = lbc,ubc
       l = clandunit(c)
       if (ityplun(l) == istsoil) then
          rwat(c) = 0._kind_phys
          swat(c) = 0._kind_phys
          rz(c)   = 0._kind_phys
       end if
    end do

    do j = 1, nlevsoil
      !dir$ concurrent
      !cdir nodep
       do c = lbc,ubc
          l = clandunit(c)
          if (ityplun(l) == istsoil) then
             if (z(c,j)+0.5_kind_phys*dz(c,j) <= 0.5_kind_phys) then
                watdry = watsat(c,j) * (316230._kind_phys/sucsat(c,j)) ** (-1._kind_phys/bsw(c,j))
                rwat(c) = rwat(c) + (h2osoi_vol(c,j)-watdry) * dz(c,j)
                swat(c) = swat(c) + (watsat(c,j)    -watdry) * dz(c,j)
                rz(c) = rz(c) + dz(c,j)
             end if
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do c = lbc,ubc
       l = clandunit(c)
       if (ityplun(l) == istsoil) then
          if (rz(c) /= 0._kind_phys) then
             tsw  = rwat(c)/rz(c)
             stsw = swat(c)/rz(c)
          else
             watdry = watsat(c,1) * (316230._kind_phys/sucsat(c,1)) ** (-1._kind_phys/bsw(c,1))
             tsw = h2osoi_vol(c,1) - watdry
             stsw = watsat(c,1) - watdry
          end if
          wf(c) = tsw/stsw
       else
          wf(c) = 1.0_kind_phys
       end if
    end do

#endif
#endif

  end subroutine ShalLakeHydrology

  subroutine QSat (T, p, es, esdT, qs, qsdT)
    !
    ! !DESCRIPTION:
    ! Computes saturation mixing ratio and the change in saturation
    ! mixing ratio with respect to temperature.
    ! Reference:  Polynomial approximations from:
    !             Piotr J. Flatau, et al.,1992:  Polynomial fits to saturation
    !             vapor pressure.  Journal of Applied Meteorology, 31, 1507-1513.
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    real(kind_phys), intent(in)  :: T        ! temperature (K)
    real(kind_phys), intent(in)  :: p        ! surface atmospheric pressure (pa)
    real(kind_phys), intent(out) :: es       ! vapor pressure (pa)
    real(kind_phys), intent(out) :: esdT     ! d(es)/d(T)
    real(kind_phys), intent(out) :: qs       ! humidity (kg/kg)
    real(kind_phys), intent(out) :: qsdT     ! d(qs)/d(T)
    !
    ! !CALLED FROM:
    ! subroutine Biogeophysics1 in module Biogeophysics1Mod
    ! subroutine BiogeophysicsLake in module BiogeophysicsLakeMod
    ! subroutine CanopyFluxesMod CanopyFluxesMod
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    !
    !EOP
    !
    ! !LOCAL VARIABLES:
    !
    real(kind_phys) :: T_limit
    real(kind_phys) :: td,vp,vp1,vp2
    !
    ! For water vapor (temperature range 0C-100C)
    !
    real(kind_phys), parameter :: a0 =  6.11213476
    real(kind_phys), parameter :: a1 =  0.444007856
    real(kind_phys), parameter :: a2 =  0.143064234e-01
    real(kind_phys), parameter :: a3 =  0.264461437e-03
    real(kind_phys), parameter :: a4 =  0.305903558e-05
    real(kind_phys), parameter :: a5 =  0.196237241e-07
    real(kind_phys), parameter :: a6 =  0.892344772e-10
    real(kind_phys), parameter :: a7 = -0.373208410e-12
    real(kind_phys), parameter :: a8 =  0.209339997e-15
    !
    ! For derivative:water vapor
    !
    real(kind_phys), parameter :: b0 =  0.444017302
    real(kind_phys), parameter :: b1 =  0.286064092e-01
    real(kind_phys), parameter :: b2 =  0.794683137e-03
    real(kind_phys), parameter :: b3 =  0.121211669e-04
    real(kind_phys), parameter :: b4 =  0.103354611e-06
    real(kind_phys), parameter :: b5 =  0.404125005e-09
    real(kind_phys), parameter :: b6 = -0.788037859e-12
    real(kind_phys), parameter :: b7 = -0.114596802e-13
    real(kind_phys), parameter :: b8 =  0.381294516e-16
    !
    ! For ice (temperature range -75C-0C)
    !
    real(kind_phys), parameter :: c0 =  6.11123516
    real(kind_phys), parameter :: c1 =  0.503109514
    real(kind_phys), parameter :: c2 =  0.188369801e-01
    real(kind_phys), parameter :: c3 =  0.420547422e-03
    real(kind_phys), parameter :: c4 =  0.614396778e-05
    real(kind_phys), parameter :: c5 =  0.602780717e-07
    real(kind_phys), parameter :: c6 =  0.387940929e-09
    real(kind_phys), parameter :: c7 =  0.149436277e-11
    real(kind_phys), parameter :: c8 =  0.262655803e-14
    !
    ! For derivative:ice
    !
    real(kind_phys), parameter :: d0 =  0.503277922
    real(kind_phys), parameter :: d1 =  0.377289173e-01
    real(kind_phys), parameter :: d2 =  0.126801703e-02
    real(kind_phys), parameter :: d3 =  0.249468427e-04
    real(kind_phys), parameter :: d4 =  0.313703411e-06
    real(kind_phys), parameter :: d5 =  0.257180651e-08
    real(kind_phys), parameter :: d6 =  0.133268878e-10
    real(kind_phys), parameter :: d7 =  0.394116744e-13
    real(kind_phys), parameter :: d8 =  0.498070196e-16
    !-----------------------------------------------------------------------

    T_limit = T - tfrz
    if (T_limit > 100.0) T_limit=100.0
    if (T_limit < -75.0) T_limit=-75.0

    td       = T_limit
    if (td >= 0.0) then
       es   = a0 + td*(a1 + td*(a2 + td*(a3 + td*(a4 &
            + td*(a5 + td*(a6 + td*(a7 + td*a8)))))))
       esdT = b0 + td*(b1 + td*(b2 + td*(b3 + td*(b4 &
            + td*(b5 + td*(b6 + td*(b7 + td*b8)))))))
    else
       es   = c0 + td*(c1 + td*(c2 + td*(c3 + td*(c4 &
            + td*(c5 + td*(c6 + td*(c7 + td*c8)))))))
       esdT = d0 + td*(d1 + td*(d2 + td*(d3 + td*(d4 &
            + td*(d5 + td*(d6 + td*(d7 + td*d8)))))))
    endif

    es    = es    * 100.            ! pa
    esdT  = esdT  * 100.            ! pa/K

    vp    = 1.0   / (p - 0.378*es)
    vp1   = 0.622 * vp
    vp2   = vp1   * vp

    qs    = es    * vp1             ! kg/kg
    qsdT  = esdT  * vp2 * p         ! 1 / K

  end subroutine QSat


  subroutine Tridiagonal (lbc, ubc, lbj, ubj, jtop, numf, filter, &
                          a, b, c, r, u)
    !
    ! !DESCRIPTION:
    ! Tridiagonal matrix solution
    !
    ! !USES:
    !  use shr_kind_mod, only: kind_phys => shr_kind_kind_phys
    !
    ! !ARGUMENTS:
    implicit none
    integer , intent(in)    :: lbc, ubc               ! lbinning and ubing column indices
    integer , intent(in)    :: lbj, ubj               ! lbinning and ubing level indices
    integer , intent(in)    :: jtop(lbc:ubc)          ! top level for each column
    integer , intent(in)    :: numf                   ! filter dimension
    integer , intent(in)    :: filter(1:numf)         ! filter
    real(kind_phys), intent(in)    :: a(lbc:ubc, lbj:ubj)    ! "a" left off diagonal of tridiagonal matrix
    real(kind_phys), intent(in)    :: b(lbc:ubc, lbj:ubj)    ! "b" diagonal column for tridiagonal matrix
    real(kind_phys), intent(in)    :: c(lbc:ubc, lbj:ubj)    ! "c" right off diagonal tridiagonal matrix
    real(kind_phys), intent(in)    :: r(lbc:ubc, lbj:ubj)    ! "r" forcing term of tridiagonal matrix
    real(kind_phys), intent(inout) :: u(lbc:ubc, lbj:ubj)    ! solution
    !
    ! !CALLED FROM:
    ! subroutine BiogeophysicsLake in module BiogeophysicsLakeMod
    ! subroutine SoilTemperature in module SoilTemperatureMod
    ! subroutine SoilWater in module HydrologyMod
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    !  1 July 2003: Mariana Vertenstein; modified for vectorization
    !
    !EOP
    !
    ! !OTHER LOCAL VARIABLES:
    !
    integer  :: j,ci,fc                   !indices
    real(kind_phys) :: gam(lbc:ubc,lbj:ubj)      !temporary
    real(kind_phys) :: bet(lbc:ubc)              !temporary
    !-----------------------------------------------------------------------

    ! Solve the matrix

    !dir$ concurrent
    !cdir nodep
    do fc = 1,numf
       ci = filter(fc)
       bet(ci) = b(ci,jtop(ci))
    end do

    do j = lbj, ubj
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             if (j == jtop(ci)) then
                u(ci,j) = r(ci,j) / bet(ci)
             else
                gam(ci,j) = c(ci,j-1) / bet(ci)
                bet(ci) = b(ci,j) - a(ci,j) * gam(ci,j)
                u(ci,j) = (r(ci,j) - a(ci,j)*u(ci,j-1)) / bet(ci)
             end if
          end if
       end do
    end do

    !Cray X1 unroll directive used here as work-around for compiler issue 2003/10/20
    !dir$ unroll 0
    do j = ubj-1,lbj,-1
      !dir$ prefervector
      !dir$ concurrent
      !cdir nodep
       do fc = 1,numf
          ci = filter(fc)
          if (j >= jtop(ci)) then
             u(ci,j) = u(ci,j) - gam(ci,j+1) * u(ci,j+1)
          end if
       end do
    end do

  end subroutine Tridiagonal


  subroutine SnowWater(lbc, ubc, num_snowc, filter_snowc,         & !i
                   num_nosnowc, filter_nosnowc,               & !i 
                   snl,do_capsnow,qflx_snomelt,qflx_rain_grnd,            & !i
                   qflx_sub_snow,qflx_evap_grnd,                          & !i   
                   qflx_dew_snow,qflx_dew_grnd,dz,                        & !i   
                   h2osoi_ice,h2osoi_liq,                                 & !i&o 
                   qflx_top_soil)                                           !o                        
    !===============================================================================
    ! !DESCRIPTION:
    ! Evaluate the change of snow mass and the snow water onto soil.
    ! Water flow within snow is computed by an explicit and non-physical
    ! based scheme, which permits a part of liquid water over the holding
    ! capacity (a tentative value is used, i.e. equal to 0.033*porosity) to
    ! percolate into the underlying layer.  Except for cases where the
    ! porosity of one of the two neighboring layers is less than 0.05, zero
    ! flow is assumed. The water flow out of the bottom of the snow pack will
    ! participate as the input of the soil water and runoff.  This subroutine
    ! uses a filter for columns containing snow which must be constructed prior
    ! to being called.
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    ! 15 November 2000: Mariana Vertenstein
    ! 2/26/02, Peter Thornton: Migrated to new data structures.
    !=============================================================================
    ! !USES:
    !  use clmtype

    implicit none

    !in:
    integer, intent(in) :: lbc, ubc                    ! column bounds
    integer, intent(in) :: num_snowc                   ! number of snow points in column filter
    integer, intent(in) :: filter_snowc(ubc-lbc+1)     ! column filter for snow points
    integer, intent(in) :: num_nosnowc                 ! number of non-snow points in column filter
    integer, intent(in) :: filter_nosnowc(ubc-lbc+1)   ! column filter for non-snow points

    integer , intent(in) :: snl(1)              !number of snow layers
    logical , intent(in) :: do_capsnow(1)       !true => do snow capping
    real(kind_phys), intent(in) :: qflx_snomelt(1)     !snow melt (mm H2O /s)
    real(kind_phys), intent(in) :: qflx_rain_grnd(1)   !rain on ground after interception (mm H2O/s) [+]
    real(kind_phys), intent(in) :: qflx_sub_snow(1)    !sublimation rate from snow pack (mm H2O /s) [+]
    real(kind_phys), intent(in) :: qflx_evap_grnd(1)   !ground surface evaporation rate (mm H2O/s) [+]
    real(kind_phys), intent(in) :: qflx_dew_snow(1)    !surface dew added to snow pack (mm H2O /s) [+]
    real(kind_phys), intent(in) :: qflx_dew_grnd(1)    !ground surface dew formation (mm H2O /s) [+]
    real(kind_phys), intent(in) :: dz(1,-nlevsnow+1:nlevsoil)             !layer depth (m)


    !inout: 

    real(kind_phys), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)     !ice lens (kg/m2)
    real(kind_phys), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)     !liquid water (kg/m2)
    
    !out:
    
    real(kind_phys), intent(out) :: qflx_top_soil(1)     !net water input into soil from top (mm/s)


    ! OTHER LOCAL VARIABLES:

    integer  :: c, j, fc                           !do loop/array indices
    real(kind_phys) :: qin(lbc:ubc)                       !water flow into the elmement (mm/s)
    real(kind_phys) :: qout(lbc:ubc)                      !water flow out of the elmement (mm/s)
    real(kind_phys) :: wgdif                              !ice mass after minus sublimation
    real(kind_phys) :: vol_liq(lbc:ubc,-nlevsnow+1:0)      !partial volume of liquid water in layer
    real(kind_phys) :: vol_ice(lbc:ubc,-nlevsnow+1:0)      !partial volume of ice lens in layer
    real(kind_phys) :: eff_porosity(lbc:ubc,-nlevsnow+1:0) !effective porosity = porosity - vol_ice
    !-----------------------------------------------------------------------
    ! Renew the mass of ice lens (h2osoi_ice) and liquid (h2osoi_liq) in the
    ! surface snow layer resulting from sublimation (frost) / evaporation (condense)

    !dir$ concurrent
    !cdir nodep
    do fc = 1,num_snowc
       c = filter_snowc(fc)
       if (do_capsnow(c)) then
          wgdif = h2osoi_ice(c,snl(c)+1) - qflx_sub_snow(c)*dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0.) then
             h2osoi_ice(c,snl(c)+1) = 0.
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) - qflx_evap_grnd(c) * dtime
       else
          wgdif = h2osoi_ice(c,snl(c)+1) + (qflx_dew_snow(c) - qflx_sub_snow(c)) * dtime
          h2osoi_ice(c,snl(c)+1) = wgdif
          if (wgdif < 0.) then
             h2osoi_ice(c,snl(c)+1) = 0.
             h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) + wgdif
          end if
          h2osoi_liq(c,snl(c)+1) = h2osoi_liq(c,snl(c)+1) +  &
               (qflx_rain_grnd(c) + qflx_dew_grnd(c) - qflx_evap_grnd(c)) * dtime
       end if
       h2osoi_liq(c,snl(c)+1) = max(0._kind_phys, h2osoi_liq(c,snl(c)+1))
    end do

    ! Porosity and partial volume

    do j = -nlevsnow+1, 0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             vol_ice(c,j) = min(1._kind_phys, h2osoi_ice(c,j)/(dz(c,j)*denice))
             eff_porosity(c,j) = 1. - vol_ice(c,j)
             vol_liq(c,j) = min(eff_porosity(c,j),h2osoi_liq(c,j)/(dz(c,j)*denh2o))
          end if
       end do
    end do

    ! Capillary forces within snow are usually two or more orders of magnitude
    ! less than those of gravity. Only gravity terms are considered.
    ! the genernal expression for water flow is "K * ss**3", however,
    ! no effective parameterization for "K".  Thus, a very simple consideration
    ! (not physically based) is introduced:
    ! when the liquid water of layer exceeds the layer's holding
    ! capacity, the excess meltwater adds to the underlying neighbor layer.

    qin(:) = 0._kind_phys

    do j = -nlevsnow+1, 0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osoi_liq(c,j) = h2osoi_liq(c,j) + qin(c)
             if (j <= -1) then
                ! No runoff over snow surface, just ponding on surface
                if (eff_porosity(c,j) < wimp .OR. eff_porosity(c,j+1) < wimp) then
                   qout(c) = 0._kind_phys
                else
                   qout(c) = max(0._kind_phys,(vol_liq(c,j)-ssi*eff_porosity(c,j))*dz(c,j))
                   qout(c) = min(qout(c),(1.-vol_ice(c,j+1)-vol_liq(c,j+1))*dz(c,j+1))
                end if
             else
                qout(c) = max(0._kind_phys,(vol_liq(c,j) - ssi*eff_porosity(c,j))*dz(c,j))
             end if
             qout(c) = qout(c)*1000.
             h2osoi_liq(c,j) = h2osoi_liq(c,j) - qout(c)
             qin(c) = qout(c)
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       ! Qout from snow bottom
       qflx_top_soil(c) = qout(c) / dtime
    end do

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_nosnowc
       c = filter_nosnowc(fc)
       qflx_top_soil(c) = qflx_rain_grnd(c) + qflx_snomelt(c)
    end do

  end subroutine SnowWater

  subroutine SnowCompaction(lbc, ubc, num_snowc, filter_snowc,   &!i  
                           snl,imelt,frac_iceold,t_soisno,                  &!i  
                           h2osoi_ice,h2osoi_liq,                           &!i  
                           dz)                                               !i&o   

    
    !================================================================================
    ! !DESCRIPTION:
    ! Determine the change in snow layer thickness due to compaction and
    ! settling.
    ! Three metamorphisms of changing snow characteristics are implemented,
    ! i.e., destructive, overburden, and melt. The treatments of the former
    ! two are from SNTHERM.89 and SNTHERM.99 (1991, 1999). The contribution
    ! due to melt metamorphism is simply taken as a ratio of snow ice
    ! fraction after the melting versus before the melting.
    !
    ! CALLED FROM:
    ! subroutine Hydrology2 in module Hydrology2Mod
    !
    ! REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    ! 2/28/02, Peter Thornton: Migrated to new data structures
    !==============================================================================
    ! USES:
    !  use clmtype
    !
    ! !ARGUMENTS:
    implicit none

    !in:
    integer, intent(in) :: lbc, ubc                ! column bounds
    integer, intent(in) :: num_snowc               ! number of column snow points in column filter
    integer, intent(in) :: filter_snowc(ubc-lbc+1) ! column filter for snow points
    integer,  intent(in) :: snl(1)             !number of snow layers
    integer,  intent(in) :: imelt(1,-nlevsnow+1:nlevsoil)        !flag for melting (=1), freezing (=2), Not=0
    real(kind_phys), intent(in) :: frac_iceold(1,-nlevsnow+1:nlevsoil)  !fraction of ice relative to the tot water
    real(kind_phys), intent(in) :: t_soisno(1,-nlevsnow+1:nlevsoil)     !soil temperature (Kelvin)
    real(kind_phys), intent(in) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   !ice lens (kg/m2)
    real(kind_phys), intent(in) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   !liquid water (kg/m2)

    !inout:

    real(kind_phys), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           !layer depth (m)

    ! OTHER LOCAL VARIABLES:

    integer :: j, c, fc                   ! indices
    real(kind_phys), parameter :: c2 = 23.e-3    ! [m3/kg]
    real(kind_phys), parameter :: c3 = 2.777e-6  ! [1/s]
    real(kind_phys), parameter :: c4 = 0.04      ! [1/K]
    real(kind_phys), parameter :: c5 = 2.0       !
    real(kind_phys), parameter :: dm = 100.0     ! Upper Limit on Destructive Metamorphism Compaction [kg/m3]
    real(kind_phys), parameter :: eta0 = 9.e+5   ! The Viscosity Coefficient Eta0 [kg-s/m2]
    real(kind_phys) :: burden(lbc:ubc) ! pressure of overlying snow [kg/m2]
    real(kind_phys) :: ddz1   ! Rate of settling of snowpack due to destructive metamorphism.
    real(kind_phys) :: ddz2   ! Rate of compaction of snowpack due to overburden.
    real(kind_phys) :: ddz3   ! Rate of compaction of snowpack due to melt [1/s]
    real(kind_phys) :: dexpf  ! expf=exp(-c4*(273.15-t_soisno)).
    real(kind_phys) :: fi     ! Fraction of ice relative to the total water content at current time step
    real(kind_phys) :: td     ! t_soisno - tfrz [K]
    real(kind_phys) :: pdzdtc ! Nodal rate of change in fractional-thickness due to compaction [fraction/s]
    real(kind_phys) :: void   ! void (1 - vol_ice - vol_liq)
    real(kind_phys) :: wx     ! water mass (ice+liquid) [kg/m2]
    real(kind_phys) :: bi     ! partial density of ice [kg/m3]

    !-----------------------------------------------------------------------


    ! Begin calculation - note that the following column loops are only invoked if snl(c) < 0

    burden(:) = 0._kind_phys

    do j = -nlevsnow+1, 0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then

             wx = h2osoi_ice(c,j) + h2osoi_liq(c,j)
             void = 1. - (h2osoi_ice(c,j)/denice + h2osoi_liq(c,j)/denh2o) / dz(c,j)

             ! Allow compaction only for non-saturated node and higher ice lens node.
             if (void > 0.001 .and. h2osoi_ice(c,j) > .1) then
                bi = h2osoi_ice(c,j) / dz(c,j)
                fi = h2osoi_ice(c,j) / wx
                td = tfrz-t_soisno(c,j)
                dexpf = exp(-c4*td)

                ! Settling as a result of destructive metamorphism

                ddz1 = -c3*dexpf
                if (bi > dm) ddz1 = ddz1*exp(-46.0e-3*(bi-dm))

                ! Liquid water term

                if (h2osoi_liq(c,j) > 0.01*dz(c,j)) ddz1=ddz1*c5

                ! Compaction due to overburden

                ddz2 = -burden(c)*exp(-0.08*td - c2*bi)/eta0

                ! Compaction occurring during melt

                if (imelt(c,j) == 1) then
                   ddz3 = - 1./dtime * max(0._kind_phys,(frac_iceold(c,j) - fi)/frac_iceold(c,j))
                else
                   ddz3 = 0._kind_phys
                end if

                ! Time rate of fractional change in dz (units of s-1)

                pdzdtc = ddz1 + ddz2 + ddz3

                ! The change in dz due to compaction

                dz(c,j) = dz(c,j) * (1.+pdzdtc*dtime)
             end if

             ! Pressure of overlying snow

             burden(c) = burden(c) + wx

          end if
       end do
    end do

  end subroutine SnowCompaction

  subroutine CombineSnowLayers(lbc, ubc,                            & !i
                              num_snowc, filter_snowc, & !i&o
                              snl,h2osno,snowdp,dz,zi,             & !i&o
                              t_soisno,h2osoi_ice,h2osoi_liq,      & !i&o
                              z)  !o
    !==========================================================================
    ! !DESCRIPTION:
    ! Combine snow layers that are less than a minimum thickness or mass
    ! If the snow element thickness or mass is less than a prescribed minimum,
    ! then it is combined with a neighboring element.  The subroutine
    ! clm\_combo.f90 then executes the combination of mass and energy.
    ! !CALLED FROM:
    ! subroutine Hydrology2 in module Hydrology2Mod
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    ! 2/28/02, Peter Thornton: Migrated to new data structures.
    !=========================================================================
    ! !USES:
    !  use clmtype
    !
    ! !ARGUMENTS:
    implicit none
    !in:
    integer, intent(in)    :: lbc, ubc                    ! column bounds
   ! integer, intent(in) :: clandunit(1)       !landunit index for each column
   ! integer, intent(in) :: ityplun(1)         !landunit type

    !inout:
    integer, intent(inout) :: num_snowc                   ! number of column snow points in column filter
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)     ! column filter for snow points
    integer , intent(inout) :: snl(1)            !number of snow layers
    real(kind_phys), intent(inout) :: h2osno(1)         !snow water (mm H2O)
    real(kind_phys), intent(inout) :: snowdp(1)         !snow height (m)
    real(kind_phys), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           !layer depth (m)
    real(kind_phys), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)           !interface level below a "z" level (m)
    real(kind_phys), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     !soil temperature (Kelvin)
    real(kind_phys), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   !ice lens (kg/m2)
    real(kind_phys), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   !liquid water (kg/m2)

    !out:

    real(kind_phys), intent(out) :: z(1,-nlevsnow+1:nlevsoil)            !layer thickness (m)
    !
    !EOP
    !
    ! !OTHER LOCAL VARIABLES:
    !
    integer :: c, fc                 ! column indices
    integer :: i,k                   ! loop indices
    integer :: j,l                   ! node indices
    integer :: msn_old(lbc:ubc)      ! number of top snow layer
    integer :: mssi(lbc:ubc)         ! node index
    integer :: neibor                ! adjacent node selected for combination
    real(kind_phys):: zwice(lbc:ubc)        ! total ice mass in snow
    real(kind_phys):: zwliq (lbc:ubc)       ! total liquid water in snow
    real(kind_phys):: dzmin(5)              ! minimum of top snow layer

    data dzmin /0.010, 0.015, 0.025, 0.055, 0.115/
    !-----------------------------------------------------------------------
    
    ! Check the mass of ice lens of snow, when the total is less than a small value,
    ! combine it with the underlying neighbor.
    
    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       msn_old(c) = snl(c)
    end do

    ! The following loop is NOT VECTORIZED

    do fc = 1, num_snowc
       c = filter_snowc(fc)
   !    l = clandunit(c)                                                    
       do j = msn_old(c)+1,0
          if (h2osoi_ice(c,j) <= .1) then
           !  if (ityplun(l) == istsoil) then                                
           !     h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) + h2osoi_liq(c,j)        
           !     h2osoi_ice(c,j+1) = h2osoi_ice(c,j+1) + h2osoi_ice(c,j)       
           !  else if (ityplun(l) /= istsoil .and. j /= 0) then               
             h2osoi_liq(c,j+1) = h2osoi_liq(c,j+1) + h2osoi_liq(c,j)
             h2osoi_ice(c,j+1) = h2osoi_ice(c,j+1) + h2osoi_ice(c,j)
           !  end if 

             ! shift all elements above this down one.
             if (j > snl(c)+1 .and. snl(c) < -1) then
                do i = j, snl(c)+2, -1
                   t_soisno(c,i)   = t_soisno(c,i-1)
                   h2osoi_liq(c,i) = h2osoi_liq(c,i-1)
                   h2osoi_ice(c,i) = h2osoi_ice(c,i-1)
                   dz(c,i)         = dz(c,i-1)
                end do
             end if
             snl(c) = snl(c) + 1
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_snowc
       c = filter_snowc(fc)
       h2osno(c) = 0._kind_phys
       snowdp(c) = 0._kind_phys
       zwice(c)  = 0._kind_phys
       zwliq(c)  = 0._kind_phys
    end do

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
      do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             h2osno(c) = h2osno(c) + h2osoi_ice(c,j) + h2osoi_liq(c,j)
             snowdp(c) = snowdp(c) + dz(c,j)
             zwice(c)  = zwice(c) + h2osoi_ice(c,j)
             zwliq(c)  = zwliq(c) + h2osoi_liq(c,j)
          end if
       end do
    end do

    ! Check the snow depth - all snow gone
    ! The liquid water assumes ponding on soil surface.

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_snowc
       c = filter_snowc(fc)
      ! l = clandunit(c)                                         
       if (snowdp(c) < 0.01 .and. snowdp(c) > 0.) then
          snl(c) = 0
          h2osno(c) = zwice(c)
          if (h2osno(c) <= 0.) snowdp(c) = 0._kind_phys
      !    if (ityplun(l) == istsoil) h2osoi_liq(c,1) = h2osoi_liq(c,1) + zwliq(c)    !change by guhp
       end if
    end do

    ! Check the snow depth - snow layers combined
    ! The following loop IS NOT VECTORIZED

    do fc = 1, num_snowc
       c = filter_snowc(fc)

       ! Two or more layers

       if (snl(c) < -1) then

          msn_old(c) = snl(c)
          mssi(c) = 1

          do i = msn_old(c)+1,0
             if (dz(c,i) < dzmin(mssi(c))) then

                if (i == snl(c)+1) then
                   ! If top node is removed, combine with bottom neighbor.
                   neibor = i + 1
                else if (i == 0) then
                   ! If the bottom neighbor is not snow, combine with the top neighbor.
                   neibor = i - 1
                else
                   ! If none of the above special cases apply, combine with the thinnest neighbor
                   neibor = i + 1
                   if ((dz(c,i-1)+dz(c,i)) < (dz(c,i+1)+dz(c,i))) neibor = i-1
                end if

                ! Node l and j are combined and stored as node j.
                if (neibor > i) then
                   j = neibor
                   l = i
                else
                   j = i
                   l = neibor
                end if

                call Combo (dz(c,j), h2osoi_liq(c,j), h2osoi_ice(c,j), &
                   t_soisno(c,j), dz(c,l), h2osoi_liq(c,l), h2osoi_ice(c,l), t_soisno(c,l) )

                ! Now shift all elements above this down one.
                if (j-1 > snl(c)+1) then
                   do k = j-1, snl(c)+2, -1
                      t_soisno(c,k) = t_soisno(c,k-1)
                      h2osoi_ice(c,k) = h2osoi_ice(c,k-1)
                      h2osoi_liq(c,k) = h2osoi_liq(c,k-1)
                      dz(c,k) = dz(c,k-1)
                   end do
                end if

                ! Decrease the number of snow layers
                snl(c) = snl(c) + 1
                if (snl(c) >= -1) EXIT

             else

                ! The layer thickness is greater than the prescribed minimum value
                mssi(c) = mssi(c) + 1

             end if
          end do

       end if

    end do

    ! Reset the node depth and the depth of layer interface

    do j = 0, -nlevsnow+1, -1
      !dir$ concurrent
      !cdir nodep
      do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c) + 1) then
             z(c,j) = zi(c,j) - 0.5*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do

  end subroutine CombineSnowLayers

  subroutine DivideSnowLayers(lbc, ubc,                             & !i
                             num_snowc, filter_snowc,  & !i&o
                             snl,dz,zi,t_soisno,                   & !i&o
                             h2osoi_ice,h2osoi_liq,                & !i&o
                             z)  !o


    !============================================================================
    ! !DESCRIPTION:
    ! Subdivides snow layers if they exceed their prescribed maximum thickness.
    ! !CALLED FROM:
    ! subroutine Hydrology2 in module Hydrology2Mod
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    ! 2/28/02, Peter Thornton: Migrated to new data structures.
    !============================================================================
    ! !USES:
    !   use clmtype
    !
    ! !ARGUMENTS:
    implicit none

    !in:
    integer, intent(in)    :: lbc, ubc                    ! column bounds

    !inout:

    integer, intent(inout) :: num_snowc                   ! number of column snow points in column filter
    integer, intent(inout) :: filter_snowc(ubc-lbc+1)     ! column filter for snow points
    integer , intent(inout) :: snl(1)            !number of snow layers
    real(kind_phys), intent(inout) :: dz(1,-nlevsnow+1:nlevsoil)           !layer depth (m)
    real(kind_phys), intent(inout) :: zi(1,-nlevsnow+0:nlevsoil)           !interface level below a "z" level (m)
    real(kind_phys), intent(inout) :: t_soisno(1,-nlevsnow+1:nlevsoil)     !soil temperature (Kelvin)
    real(kind_phys), intent(inout) :: h2osoi_ice(1,-nlevsnow+1:nlevsoil)   !ice lens (kg/m2)
    real(kind_phys), intent(inout) :: h2osoi_liq(1,-nlevsnow+1:nlevsoil)   !liquid water (kg/m2)

    !out: 

    real(kind_phys), intent(out) :: z(1,-nlevsnow+1:nlevsoil)            !layer thickness (m)



    ! OTHER LOCAL VARIABLES:

    integer  :: j, c, fc               ! indices
    real(kind_phys) :: drr                    ! thickness of the combined [m]
    integer  :: msno                   ! number of snow layer 1 (top) to msno (bottom)
    real(kind_phys) :: dzsno(lbc:ubc,nlevsnow) ! Snow layer thickness [m]
    real(kind_phys) :: swice(lbc:ubc,nlevsnow) ! Partial volume of ice [m3/m3]
    real(kind_phys) :: swliq(lbc:ubc,nlevsnow) ! Partial volume of liquid water [m3/m3]
    real(kind_phys) :: tsno(lbc:ubc ,nlevsnow) ! Nodel temperature [K]
    real(kind_phys) :: zwice                  ! temporary
    real(kind_phys) :: zwliq                  ! temporary
    real(kind_phys) :: propor                 ! temporary
    !-----------------------------------------------------------------------

    ! Begin calculation - note that the following column loops are only invoked
    ! for snow-covered columns

    do j = 1,nlevsnow
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j <= abs(snl(c))) then
             dzsno(c,j) = dz(c,j+snl(c))
             swice(c,j) = h2osoi_ice(c,j+snl(c))
             swliq(c,j) = h2osoi_liq(c,j+snl(c))
             tsno(c,j)  = t_soisno(c,j+snl(c))
          end if
       end do
    end do

    !dir$ concurrent
    !cdir nodep
    do fc = 1, num_snowc
       c = filter_snowc(fc)

       msno = abs(snl(c))

       if (msno == 1) then
          ! Specify a new snow layer
          if (dzsno(c,1) > 0.03) then
             msno = 2
             dzsno(c,1) = dzsno(c,1)/2.
             swice(c,1) = swice(c,1)/2.
             swliq(c,1) = swliq(c,1)/2.
             dzsno(c,2) = dzsno(c,1)
             swice(c,2) = swice(c,1)
             swliq(c,2) = swliq(c,1)
             tsno(c,2)  = tsno(c,1)
          end if
       end if

       if (msno > 1) then
          if (dzsno(c,1) > 0.02) then
             drr = dzsno(c,1) - 0.02
             propor = drr/dzsno(c,1)
             zwice = propor*swice(c,1)
             zwliq = propor*swliq(c,1)
             propor = 0.02/dzsno(c,1)
             swice(c,1) = propor*swice(c,1)
             swliq(c,1) = propor*swliq(c,1)
             dzsno(c,1) = 0.02

             call Combo (dzsno(c,2), swliq(c,2), swice(c,2), tsno(c,2), drr, &
                  zwliq, zwice, tsno(c,1))

             ! Subdivide a new layer
             if (msno <= 2 .and. dzsno(c,2) > 0.07) then
                msno = 3
                dzsno(c,2) = dzsno(c,2)/2.
                swice(c,2) = swice(c,2)/2.
                swliq(c,2) = swliq(c,2)/2.
                dzsno(c,3) = dzsno(c,2)
                swice(c,3) = swice(c,2)
                swliq(c,3) = swliq(c,2)
                tsno(c,3)  = tsno(c,2)
             end if
          end if
       end if

       if (msno > 2) then
          if (dzsno(c,2) > 0.05) then
             drr = dzsno(c,2) - 0.05
             propor = drr/dzsno(c,2)
             zwice = propor*swice(c,2)
             zwliq = propor*swliq(c,2)
             propor = 0.05/dzsno(c,2)
             swice(c,2) = propor*swice(c,2)
             swliq(c,2) = propor*swliq(c,2)
             dzsno(c,2) = 0.05

             call Combo (dzsno(c,3), swliq(c,3), swice(c,3), tsno(c,3), drr, &
                  zwliq, zwice, tsno(c,2))

             ! Subdivided a new layer
             if (msno <= 3 .and. dzsno(c,3) > 0.18) then
                msno =  4
                dzsno(c,3) = dzsno(c,3)/2.
                swice(c,3) = swice(c,3)/2.
                swliq(c,3) = swliq(c,3)/2.
                dzsno(c,4) = dzsno(c,3)
                swice(c,4) = swice(c,3)
                swliq(c,4) = swliq(c,3)
                tsno(c,4)  = tsno(c,3)
             end if
          end if
       end if

       if (msno > 3) then
          if (dzsno(c,3) > 0.11) then
             drr = dzsno(c,3) - 0.11
             propor = drr/dzsno(c,3)
             zwice = propor*swice(c,3)
             zwliq = propor*swliq(c,3)
             propor = 0.11/dzsno(c,3)
             swice(c,3) = propor*swice(c,3)
             swliq(c,3) = propor*swliq(c,3)
             dzsno(c,3) = 0.11

             call Combo (dzsno(c,4), swliq(c,4), swice(c,4), tsno(c,4), drr, &
                  zwliq, zwice, tsno(c,3))

             ! Subdivided a new layer
             if (msno <= 4 .and. dzsno(c,4) > 0.41) then
                msno = 5
                dzsno(c,4) = dzsno(c,4)/2.
                swice(c,4) = swice(c,4)/2.
                swliq(c,4) = swliq(c,4)/2.
                dzsno(c,5) = dzsno(c,4)
                swice(c,5) = swice(c,4)
                swliq(c,5) = swliq(c,4)
                tsno(c,5)  = tsno(c,4)
             end if
          end if
       end if

       if (msno > 4) then
          if (dzsno(c,4) > 0.23) then
             drr = dzsno(c,4) - 0.23
             propor = drr/dzsno(c,4)
             zwice = propor*swice(c,4)
             zwliq = propor*swliq(c,4)
             propor = 0.23/dzsno(c,4)
             swice(c,4) = propor*swice(c,4)
             swliq(c,4) = propor*swliq(c,4)
             dzsno(c,4) = 0.23

             call Combo (dzsno(c,5), swliq(c,5), swice(c,5), tsno(c,5), drr, &
                  zwliq, zwice, tsno(c,4))
          end if
       end if

       snl(c) = -msno

    end do

    do j = -nlevsnow+1,0
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             dz(c,j)         = dzsno(c,j-snl(c))
             h2osoi_ice(c,j) = swice(c,j-snl(c))
             h2osoi_liq(c,j) = swliq(c,j-snl(c))
             t_soisno(c,j)   = tsno(c,j-snl(c))
          end if
       end do
    end do

    do j = 0, -nlevsnow+1, -1
      !dir$ concurrent
      !cdir nodep
       do fc = 1, num_snowc
          c = filter_snowc(fc)
          if (j >= snl(c)+1) then
             z(c,j)    = zi(c,j) - 0.5*dz(c,j)
             zi(c,j-1) = zi(c,j) - dz(c,j)
          end if
       end do
    end do

  end subroutine DivideSnowLayers

  subroutine Combo(dz,  wliq,  wice, t, dz2, wliq2, wice2, t2)
    !
    ! !DESCRIPTION:
    ! Combines two elements and returns the following combined
    ! variables: dz, t, wliq, wice.
    ! The combined temperature is based on the equation:
    ! the sum of the enthalpies of the two elements =
    ! that of the combined element.
    !
    ! !USES:
    !
    ! !ARGUMENTS:
    implicit none
    real(kind_phys), intent(in)    :: dz2   ! nodal thickness of 2 elements being combined [m]
    real(kind_phys), intent(in)    :: wliq2 ! liquid water of element 2 [kg/m2]
    real(kind_phys), intent(in)    :: wice2 ! ice of element 2 [kg/m2]
    real(kind_phys), intent(in)    :: t2    ! nodal temperature of element 2 [K]
    real(kind_phys), intent(inout) :: dz    ! nodal thickness of 1 elements being combined [m]
    real(kind_phys), intent(inout) :: wliq  ! liquid water of element 1
    real(kind_phys), intent(inout) :: wice  ! ice of element 1 [kg/m2]
    real(kind_phys), intent(inout) :: t     ! nodel temperature of elment 1 [K]
    !
    ! !CALLED FROM:
    ! subroutine CombineSnowLayers in this module
    ! subroutine DivideSnowLayers in this module
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    !
    !EOP
    !
    ! !LOCAL VARIABLES:
    !
    real(kind_phys) :: dzc   ! Total thickness of nodes 1 and 2 (dzc=dz+dz2).
    real(kind_phys) :: wliqc ! Combined liquid water [kg/m2]
    real(kind_phys) :: wicec ! Combined ice [kg/m2]
    real(kind_phys) :: tc    ! Combined node temperature [K]
    real(kind_phys) :: h     ! enthalpy of element 1 [J/m2]
    real(kind_phys) :: h2    ! enthalpy of element 2 [J/m2]
    real(kind_phys) :: hc    ! temporary
    !-----------------------------------------------------------------------

    dzc = dz+dz2
    wicec = (wice+wice2)
    wliqc = (wliq+wliq2)
    h = (cpice*wice+cpliq*wliq) * (t-tfrz)+hfus*wliq
    h2= (cpice*wice2+cpliq*wliq2) * (t2-tfrz)+hfus*wliq2

    hc = h + h2
    if(hc < 0.)then
       tc = tfrz + hc/(cpice*wicec + cpliq*wliqc)
    else if (hc.le.hfus*wliqc) then
       tc = tfrz
    else
       tc = tfrz + (hc - hfus*wliqc) / (cpice*wicec + cpliq*wliqc)
    end if

    dz = dzc
    wice = wicec
    wliq = wliqc
    t = tc

  end subroutine Combo

  subroutine BuildSnowFilter(lbc, ubc, num_nolakec, filter_nolakec,snl, & !i
                             num_snowc, filter_snowc, &                   !o
                             num_nosnowc, filter_nosnowc)                 !o
    !
    ! !DESCRIPTION:
    ! Constructs snow filter for use in vectorized loops for snow hydrology.
    !
    ! !USES:
    !    use clmtype
    !
    ! !ARGUMENTS:
    implicit none
    integer, intent(in)  :: lbc, ubc                    ! column bounds
    integer, intent(in)  :: num_nolakec                 ! number of column non-lake points in column filter
    integer, intent(in)  :: filter_nolakec(ubc-lbc+1)   ! column filter for non-lake points
    integer, intent(in)  :: snl(1)                        ! number of snow layers
    integer, intent(out) :: num_snowc                   ! number of column snow points in column filter
    integer, intent(out) :: filter_snowc(ubc-lbc+1)     ! column filter for snow points
    integer, intent(out) :: num_nosnowc                 ! number of column non-snow points in column filter
    integer, intent(out) :: filter_nosnowc(ubc-lbc+1)   ! column filter for non-snow points
    !
    ! !CALLED FROM:
    ! subroutine Hydrology2 in Hydrology2Mod
    ! subroutine CombineSnowLayers in this module
    !
    ! !REVISION HISTORY:
    ! 2003 July 31: Forrest Hoffman
    !
    ! !LOCAL VARIABLES:
    ! local pointers to implicit in arguments
    !
    !EOP
    !
    ! !OTHER LOCAL VARIABLES:
    integer  :: fc, c
    !-----------------------------------------------------------------------


    ! Build snow/no-snow filters for other subroutines

    num_snowc = 0
    num_nosnowc = 0
    do fc = 1, num_nolakec
       c = filter_nolakec(fc)
       if (snl(c) < 0) then
          num_snowc = num_snowc + 1
          filter_snowc(num_snowc) = c
       else
          num_nosnowc = num_nosnowc + 1
          filter_nosnowc(num_nosnowc) = c
       end if
    end do

  end subroutine BuildSnowFilter



subroutine FrictionVelocity(pgridcell,forc_hgt,forc_hgt_u,        & !i 
                             forc_hgt_t,forc_hgt_q,                  & !i 
                             lbp, ubp, fn, filterp,                  & !i 
                             displa, z0m, z0h, z0q,                  & !i 
                             obu, iter, ur, um,                      & !i 
                             ustar,temp1, temp2, temp12m, temp22m,   & !o 
                             u10,fv,                                 & !o 
                             fm)  !i&o 

  !=============================================================================
  ! !DESCRIPTION:
  ! Calculation of the friction velocity, relation for potential
  ! temperature and humidity profiles of surface boundary layer.
  ! The scheme is based on the work of Zeng et al. (1998):
  ! Intercomparison of bulk aerodynamic algorithms for the computation
  ! of sea surface fluxes using TOGA CORE and TAO data. J. Climate,
  ! Vol. 11, 2628-2644.
  !
  ! !REVISION HISTORY:
  ! 15 September 1999: Yongjiu Dai; Initial code
  ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
  ! 12/19/01, Peter Thornton
  ! Added arguments to eliminate passing clm derived type into this function.
  ! Created by Mariana Vertenstein
  !============================================================================
  ! !USES:
  ! use clmtype
  !!use clm_atmlnd, only : clm_a2l
  !
  ! !ARGUMENTS:
  implicit none

  !in:

   integer , intent(in) :: pgridcell(1)   ! pft's gridcell index
   real(kind_phys), intent(in) :: forc_hgt(1)    ! atmospheric reference height (m)
   real(kind_phys), intent(in) :: forc_hgt_u(1)  ! observational height of wind [m]
   real(kind_phys), intent(in) :: forc_hgt_t(1)  ! observational height of temperature [m]
   real(kind_phys), intent(in) :: forc_hgt_q(1)  ! observational height of humidity [m]
   integer , intent(in)  :: lbp, ubp         ! pft array bounds
   integer , intent(in)  :: fn               ! number of filtered pft elements
   integer , intent(in)  :: filterp(fn)      ! pft filter
   real(kind_phys), intent(in)  :: displa(lbp:ubp)  ! displacement height (m)
   real(kind_phys), intent(in)  :: z0m(lbp:ubp)     ! roughness length over vegetation, momentum [m]
   real(kind_phys), intent(in)  :: z0h(lbp:ubp)     ! roughness length over vegetation, sensible heat [m]
   real(kind_phys), intent(in)  :: z0q(lbp:ubp)     ! roughness length over vegetation, latent heat [m]
   real(kind_phys), intent(in)  :: obu(lbp:ubp)     ! monin-obukhov length (m)
   integer,  intent(in)  :: iter             ! iteration number
   real(kind_phys), intent(in)  :: ur(lbp:ubp)      ! wind speed at reference height [m/s]
   real(kind_phys), intent(in)  :: um(lbp:ubp)      ! wind speed including the stablity effect [m/s]

   !out:

   real(kind_phys), intent(out) :: ustar(lbp:ubp)   ! friction velocity [m/s]
   real(kind_phys), intent(out) :: temp1(lbp:ubp)   ! relation for potential temperature profile
   real(kind_phys), intent(out) :: temp12m(lbp:ubp) ! relation for potential temperature profile applied at 2-m
   real(kind_phys), intent(out) :: temp2(lbp:ubp)   ! relation for specific humidity profile
   real(kind_phys), intent(out) :: temp22m(lbp:ubp) ! relation for specific humidity profile applied at 2-m
   real(kind_phys), intent(out) :: u10(1)         ! 10-m wind (m/s) (for dust model)
   real(kind_phys), intent(out) :: fv(1)          ! friction velocity (m/s) (for dust model)

   !inout:
   real(kind_phys), intent(inout) :: fm(lbp:ubp)    ! needed for DGVM only to diagnose 10m wind

   ! OTHER LOCAL VARIABLES:

   real(kind_phys), parameter :: zetam = 1.574_kind_phys ! transition point of flux-gradient relation (wind profile)
   real(kind_phys), parameter :: zetat = 0.465_kind_phys ! transition point of flux-gradient relation (temp. profile)
   integer :: f                         ! pft-filter index
   integer :: p                         ! pft index
   integer :: g                         ! gridcell index
   real(kind_phys):: zldis(lbp:ubp)            ! reference height "minus" zero displacement heght [m]
   real(kind_phys):: zeta(lbp:ubp)             ! dimensionless height used in Monin-Obukhov theory
#if (defined DGVM) || (defined DUST)
   real(kind_phys) :: tmp1,tmp2,tmp3,tmp4      ! Used to diagnose the 10 meter wind
   real(kind_phys) :: fmnew                    ! Used to diagnose the 10 meter wind
   real(kind_phys) :: fm10                     ! Used to diagnose the 10 meter wind
   real(kind_phys) :: zeta10                   ! Used to diagnose the 10 meter wind
#endif
   !------------------------------------------------------------------------------


   ! Adjustment factors for unstable (moz < 0) or stable (moz > 0) conditions.

#if (!defined PERGRO)

   !dir$ concurrent
   !cdir nodep
   do f = 1, fn
      p = filterp(f)
      g = pgridcell(p)

      ! Wind profile

      zldis(p) = forc_hgt_u(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetam) then
         ustar(p) = vkc*um(p)/(log(-zetam*obu(p)/z0m(p))&
              - StabilityFunc1(-zetam) &
              + StabilityFunc1(z0m(p)/obu(p)) &
              + 1.14_kind_phys*((-zeta(p))**0.333_kind_phys-(zetam)**0.333_kind_phys))
      else if (zeta(p) < 0._kind_phys) then
         ustar(p) = vkc*um(p)/(log(zldis(p)/z0m(p))&
              - StabilityFunc1(zeta(p))&
              + StabilityFunc1(z0m(p)/obu(p)))
      else if (zeta(p) <=  1._kind_phys) then
         ustar(p) = vkc*um(p)/(log(zldis(p)/z0m(p)) + 5._kind_phys*zeta(p) -5._kind_phys*z0m(p)/obu(p))
      else
         ustar(p) = vkc*um(p)/(log(obu(p)/z0m(p))+5._kind_phys-5._kind_phys*z0m(p)/obu(p) &
              +(5._kind_phys*log(zeta(p))+zeta(p)-1._kind_phys))
      end if

      ! Temperature profile

      zldis(p) = forc_hgt_t(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp1(p) = vkc/(log(-zetat*obu(p)/z0h(p))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(p)/obu(p)) &
              + 0.8_kind_phys*((zetat)**(-0.333_kind_phys)-(-zeta(p))**(-0.333_kind_phys)))
      else if (zeta(p) < 0._kind_phys) then
         temp1(p) = vkc/(log(zldis(p)/z0h(p)) &
              - StabilityFunc2(zeta(p)) &
              + StabilityFunc2(z0h(p)/obu(p)))
      else if (zeta(p) <=  1._kind_phys) then
         temp1(p) = vkc/(log(zldis(p)/z0h(p)) + 5._kind_phys*zeta(p) - 5._kind_phys*z0h(p)/obu(p))
      else
         temp1(p) = vkc/(log(obu(p)/z0h(p)) + 5._kind_phys - 5._kind_phys*z0h(p)/obu(p) &
              + (5._kind_phys*log(zeta(p))+zeta(p)-1._kind_phys))
      end if

      ! Humidity profile

      if (forc_hgt_q(g) == forc_hgt_t(g) .and. z0q(p) == z0h(p)) then
         temp2(p) = temp1(p)
      else
         zldis(p) = forc_hgt_q(g)-displa(p)
         zeta(p) = zldis(p)/obu(p)
         if (zeta(p) < -zetat) then
            temp2(p) = vkc/(log(-zetat*obu(p)/z0q(p)) &
                 - StabilityFunc2(-zetat) &
                 + StabilityFunc2(z0q(p)/obu(p)) &
                 + 0.8_kind_phys*((zetat)**(-0.333_kind_phys)-(-zeta(p))**(-0.333_kind_phys)))
         else if (zeta(p) < 0._kind_phys) then
            temp2(p) = vkc/(log(zldis(p)/z0q(p)) &
                 - StabilityFunc2(zeta(p)) &
                 + StabilityFunc2(z0q(p)/obu(p)))
         else if (zeta(p) <=  1._kind_phys) then
            temp2(p) = vkc/(log(zldis(p)/z0q(p)) + 5._kind_phys*zeta(p)-5._kind_phys*z0q(p)/obu(p))
         else
            temp2(p) = vkc/(log(obu(p)/z0q(p)) + 5._kind_phys - 5._kind_phys*z0q(p)/obu(p) &
                 + (5._kind_phys*log(zeta(p))+zeta(p)-1._kind_phys))
         end if
      endif

      ! Temperature profile applied at 2-m

      zldis(p) = 2.0_kind_phys + z0h(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp12m(p) = vkc/(log(-zetat*obu(p)/z0h(p))&
              - StabilityFunc2(-zetat) &
              + StabilityFunc2(z0h(p)/obu(p)) &
              + 0.8_kind_phys*((zetat)**(-0.333_kind_phys)-(-zeta(p))**(-0.333_kind_phys)))
      else if (zeta(p) < 0._kind_phys) then
         temp12m(p) = vkc/(log(zldis(p)/z0h(p)) &
              - StabilityFunc2(zeta(p))  &
              + StabilityFunc2(z0h(p)/obu(p)))
      else if (zeta(p) <=  1._kind_phys) then
         temp12m(p) = vkc/(log(zldis(p)/z0h(p)) + 5._kind_phys*zeta(p) - 5._kind_phys*z0h(p)/obu(p))
      else
         temp12m(p) = vkc/(log(obu(p)/z0h(p)) + 5._kind_phys - 5._kind_phys*z0h(p)/obu(p) &
              + (5._kind_phys*log(zeta(p))+zeta(p)-1._kind_phys))
      end if

      ! Humidity profile applied at 2-m

      if (z0q(p) == z0h(p)) then
         temp22m(p) = temp12m(p)
      else
         zldis(p) = 2.0_kind_phys + z0q(p)
         zeta(p) = zldis(p)/obu(p)
         if (zeta(p) < -zetat) then
            temp22m(p) = vkc/(log(-zetat*obu(p)/z0q(p)) - &
                 StabilityFunc2(-zetat) + StabilityFunc2(z0q(p)/obu(p)) &
                 + 0.8_kind_phys*((zetat)**(-0.333_kind_phys)-(-zeta(p))**(-0.333_kind_phys)))
         else if (zeta(p) < 0._kind_phys) then
            temp22m(p) = vkc/(log(zldis(p)/z0q(p)) - &
                 StabilityFunc2(zeta(p))+StabilityFunc2(z0q(p)/obu(p)))
         else if (zeta(p) <=  1._kind_phys) then
            temp22m(p) = vkc/(log(zldis(p)/z0q(p)) + 5._kind_phys*zeta(p)-5._kind_phys*z0q(p)/obu(p))
         else
            temp22m(p) = vkc/(log(obu(p)/z0q(p)) + 5._kind_phys - 5._kind_phys*z0q(p)/obu(p) &
                 + (5._kind_phys*log(zeta(p))+zeta(p)-1._kind_phys))
         end if
      end if

#if (defined DGVM) || (defined DUST)
      ! diagnose 10-m wind for dust model (dstmbl.F)
      ! Notes from C. Zender's dst.F:
      ! According to Bon96 p. 62, the displacement height d (here displa) is
      ! 0.0 <= d <= 0.34 m in dust source regions (i.e., regions w/o trees).
      ! Therefore d <= 0.034*z1 and may safely be neglected.
      ! Code from LSM routine SurfaceTemperature was used to obtain u10

      zldis(p) = forc_hgt_u(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (min(zeta(p), 1._kind_phys) < 0._kind_phys) then
         tmp1 = (1._kind_phys - 16._kind_phys*min(zeta(p),1._kind_phys))**0.25_kind_phys
         tmp2 = log((1._kind_phys+tmp1*tmp1)/2._kind_phys)
         tmp3 = log((1._kind_phys+tmp1)/2._kind_phys)
         fmnew = 2._kind_phys*tmp3 + tmp2 - 2._kind_phys*atan(tmp1) + 1.5707963_kind_phys
      else
         fmnew = -5._kind_phys*min(zeta(p),1._kind_phys)
      endif
      if (iter == 1) then
         fm(p) = fmnew
      else
         fm(p) = 0.5_kind_phys * (fm(p)+fmnew)
      end if
      zeta10 = min(10._kind_phys/obu(p), 1._kind_phys)
      if (zeta(p) == 0._kind_phys) zeta10 = 0._kind_phys
      if (zeta10 < 0._kind_phys) then
         tmp1 = (1.0_kind_phys - 16.0_kind_phys * zeta10)**0.25_kind_phys
         tmp2 = log((1.0_kind_phys + tmp1*tmp1)/2.0_kind_phys)
         tmp3 = log((1.0_kind_phys + tmp1)/2.0_kind_phys)
         fm10 = 2.0_kind_phys*tmp3 + tmp2 - 2.0_kind_phys*atan(tmp1) + 1.5707963_kind_phys
      else                ! not stable
         fm10 = -5.0_kind_phys * zeta10
      end if
      tmp4 = log(forc_hgt(g) / 10._kind_phys)
      u10(p) = ur(p) - ustar(p)/vkc * (tmp4 - fm(p) + fm10)
      fv(p)  = ustar(p)
#endif

   end do
#endif


#if (defined PERGRO)

   !===============================================================================
   ! The following only applies when PERGRO is defined
   !===============================================================================

   !dir$ concurrent
   !cdir nodep
   do f = 1, fn
      p = filterp(f)
      g = pgridcell(p)

      zldis(p) = forc_hgt_u(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetam) then           ! zeta < -1
         ustar(p) = vkc * um(p) / log(-zetam*obu(p)/z0m(p))
      else if (zeta(p) < 0._kind_phys) then         ! -1 <= zeta < 0
         ustar(p) = vkc * um(p) / log(zldis(p)/z0m(p))
      else if (zeta(p) <= 1._kind_phys) then        !  0 <= ztea <= 1
         ustar(p)=vkc * um(p)/log(zldis(p)/z0m(p))
      else                             !  1 < zeta, phi=5+zeta
         ustar(p)=vkc * um(p)/log(obu(p)/z0m(p))
      endif

      zldis(p) = forc_hgt_t(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp1(p)=vkc/log(-zetat*obu(p)/z0h(p))
      else if (zeta(p) < 0._kind_phys) then
         temp1(p)=vkc/log(zldis(p)/z0h(p))
      else if (zeta(p) <= 1._kind_phys) then
         temp1(p)=vkc/log(zldis(p)/z0h(p))
      else
         temp1(p)=vkc/log(obu(p)/z0h(p))
      end if

      zldis(p) = forc_hgt_q(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp2(p)=vkc/log(-zetat*obu(p)/z0q(p))
      else if (zeta(p) < 0._kind_phys) then
         temp2(p)=vkc/log(zldis(p)/z0q(p))
      else if (zeta(p) <= 1._kind_phys) then
         temp2(p)=vkc/log(zldis(p)/z0q(p))
      else
         temp2(p)=vkc/log(obu(p)/z0q(p))
      end if

      zldis(p) = 2.0_kind_phys + z0h(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp12m(p)=vkc/log(-zetat*obu(p)/z0h(p))
      else if (zeta(p) < 0._kind_phys) then
         temp12m(p)=vkc/log(zldis(p)/z0h(p))
      else if (zeta(p) <= 1._kind_phys) then
         temp12m(p)=vkc/log(zldis(p)/z0h(p))
      else
         temp12m(p)=vkc/log(obu(p)/z0h(p))
      end if

      zldis(p) = 2.0_kind_phys + z0q(p)
      zeta(p) = zldis(p)/obu(p)
      if (zeta(p) < -zetat) then
         temp22m(p)=vkc/log(-zetat*obu(p)/z0q(p))
      else if (zeta(p) < 0._kind_phys) then
         temp22m(p)=vkc/log(zldis(p)/z0q(p))
      else if (zeta(p) <= 1._kind_phys) then
         temp22m(p)=vkc/log(zldis(p)/z0q(p))
      else
         temp22m(p)=vkc/log(obu(p)/z0q(p))
      end if
#if (defined DGVM) || (defined DUST)
      ! diagnose 10-m wind for dust model (dstmbl.F)
      ! Notes from C. Zender's dst.F:
      ! According to Bon96 p. 62, the displacement height d (here displa) is
      ! 0.0 <= d <= 0.34 m in dust source regions (i.e., regions w/o trees).
      ! Therefore d <= 0.034*z1 and may safely be neglected.
      ! Code from LSM routine SurfaceTemperature was used to obtain u10

      zldis(p) = forc_hgt_u(g)-displa(p)
      zeta(p) = zldis(p)/obu(p)
      if (min(zeta(p), 1._kind_phys) < 0._kind_phys) then
         tmp1 = (1._kind_phys - 16._kind_phys*min(zeta(p),1._kind_phys))**0.25_kind_phys
         tmp2 = log((1._kind_phys+tmp1*tmp1)/2._kind_phys)
         tmp3 = log((1._kind_phys+tmp1)/2._kind_phys)
         fmnew = 2._kind_phys*tmp3 + tmp2 - 2._kind_phys*atan(tmp1) + 1.5707963_kind_phys
      else
         fmnew = -5._kind_phys*min(zeta(p),1._kind_phys)
      endif
      if (iter == 1) then
         fm(p) = fmnew
      else
         fm(p) = 0.5_kind_phys * (fm(p)+fmnew)
      end if
      zeta10 = min(10._kind_phys/obu(p), 1._kind_phys)
      if (zeta(p) == 0._kind_phys) zeta10 = 0._kind_phys
      if (zeta10 < 0._kind_phys) then
         tmp1 = (1.0_kind_phys - 16.0_kind_phys * zeta10)**0.25_kind_phys
         tmp2 = log((1.0_kind_phys + tmp1*tmp1)/2.0_kind_phys)
         tmp3 = log((1.0_kind_phys + tmp1)/2.0_kind_phys)
         fm10 = 2.0_kind_phys*tmp3 + tmp2 - 2.0_kind_phys*atan(tmp1) + 1.5707963_kind_phys
      else                ! not stable
         fm10 = -5.0_kind_phys * zeta10
      end if
      tmp4 = log(forc_hgt(g) / 10._kind_phys)
      u10(p) = ur(p) - ustar(p)/vkc * (tmp4 - fm(p) + fm10)
      fv(p)  = ustar(p)
#endif
   end do

#endif

   end subroutine FrictionVelocity

   ! !IROUTINE: StabilityFunc
   !
   ! !INTERFACE:
   real(kind_phys) function StabilityFunc1(zeta)
     !
     ! !DESCRIPTION:
     ! Stability function for rib < 0.
     !
     ! !USES:
     !      use shr_const_mod, only: SHR_CONST_PI
     !Zack Subin, 7/8/08
     !
     ! !ARGUMENTS:
     implicit none
      real(kind_phys), intent(in) :: zeta  ! dimensionless height used in Monin-Obukhov theory
      !
      ! !CALLED FROM:
      ! subroutine FrictionVelocity in this module
      !
      ! !REVISION HISTORY:
      ! 15 September 1999: Yongjiu Dai; Initial code
      ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
      !
      !EOP
      !
      ! !LOCAL VARIABLES:
      real(kind_phys) :: chik, chik2
      !------------------------------------------------------------------------------

      chik2 = sqrt(1._kind_phys-16._kind_phys*zeta)
      chik = sqrt(chik2)
      StabilityFunc1 = 2._kind_phys*log((1._kind_phys+chik)*0.5_kind_phys) &
           !Changed to pie, Zack Subin, 7/9/08
           + log((1._kind_phys+chik2)*0.5_kind_phys)-2._kind_phys*atan(chik)+pie*0.5_kind_phys

    end function StabilityFunc1

    !------------------------------------------------------------------------------
    !BOP
    !
    ! !IROUTINE: StabilityFunc2
    !
    ! !INTERFACE:
   real(kind_phys) function StabilityFunc2(zeta)
     !
     ! !DESCRIPTION:
     ! Stability function for rib < 0.
     !
     ! !USES:
     !Removed by Zack Subin, 7/9/08
     !     use shr_const_mod, only: SHR_CONST_PI
     !
     ! !ARGUMENTS:
     implicit none
     real(kind_phys), intent(in) :: zeta  ! dimensionless height used in Monin-Obukhov theory
     !
     ! !CALLED FROM:
     ! subroutine FrictionVelocity in this module
     !
     ! !REVISION HISTORY:
     ! 15 September 1999: Yongjiu Dai; Initial code
     ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
     !
     !EOP
     !
     ! !LOCAL VARIABLES:
     real(kind_phys) :: chik2
     !------------------------------------------------------------------------------

     chik2 = sqrt(1._kind_phys-16._kind_phys*zeta)
     StabilityFunc2 = 2._kind_phys*log((1._kind_phys+chik2)*0.5_kind_phys)

   end function StabilityFunc2

   !-----------------------------------------------------------------------
   !BOP
   !
   ! !IROUTINE: MoninObukIni
   !
   ! !INTERFACE:
   subroutine MoninObukIni (ur, thv, dthv, zldis, z0m, um, obu)
     !
     ! !DESCRIPTION:
     ! Initialization of the Monin-Obukhov length.
     ! The scheme is based on the work of Zeng et al. (1998):
     ! Intercomparison of bulk aerodynamic algorithms for the computation
     ! of sea surface fluxes using TOGA CORE and TAO data. J. Climate,
     ! Vol. 11, 2628-2644.
     !
     ! !USES:
     !
     ! !ARGUMENTS:
     implicit none
    real(kind_phys), intent(in)  :: ur    ! wind speed at reference height [m/s]
    real(kind_phys), intent(in)  :: thv   ! virtual potential temperature (kelvin)
    real(kind_phys), intent(in)  :: dthv  ! diff of vir. poten. temp. between ref. height and surface
    real(kind_phys), intent(in)  :: zldis ! reference height "minus" zero displacement heght [m]
    real(kind_phys), intent(in)  :: z0m   ! roughness length, momentum [m]
    real(kind_phys), intent(out) :: um    ! wind speed including the stability effect [m/s]
    real(kind_phys), intent(out) :: obu   ! monin-obukhov length (m)
    !
    ! !CALLED FROM:
    ! subroutine BareGroundFluxes in module BareGroundFluxesMod.F90
    ! subroutine BiogeophysicsLake in module BiogeophysicsLakeMod.F90
    ! subroutine CanopyFluxes in module CanopyFluxesMod.F90
    !
    ! !REVISION HISTORY:
    ! 15 September 1999: Yongjiu Dai; Initial code
    ! 15 December 1999:  Paul Houser and Jon Radakovich; F90 Revision
    !
    !EOP
    !
    ! !LOCAL VARIABLES:
    !
    real(kind_phys) :: wc    ! convective velocity [m/s]
    real(kind_phys) :: rib   ! bulk Richardson number
    real(kind_phys) :: zeta  ! dimensionless height used in Monin-Obukhov theory
    real(kind_phys) :: ustar ! friction velocity [m/s]
    !-----------------------------------------------------------------------

    ! Initial values of u* and convective velocity

    ustar=0.06_kind_phys
    wc=0.5_kind_phys
    if (dthv >= 0._kind_phys) then
       um=max(ur,0.1_kind_phys)
    else
       um=sqrt(ur*ur+wc*wc)
    endif

    rib=grav*zldis*dthv/(thv*um*um)
#if (defined PERGRO)
    rib = 0._kind_phys
#endif

    if (rib >= 0._kind_phys) then      ! neutral or stable
       zeta = rib*log(zldis/z0m)/(1._kind_phys-5._kind_phys*min(rib,0.19_kind_phys))
       zeta = min(2._kind_phys,max(zeta,0.01_kind_phys ))
    else                     ! unstable
       zeta=rib*log(zldis/z0m)
       zeta = max(-100._kind_phys,min(zeta,-0.01_kind_phys ))
    endif

    obu=zldis/zeta

  end subroutine MoninObukIni

! Due to a CCPP bug, this has to be called from the _run method.
 SUBROUTINE lakeini(IVGTYP,         ISLTYP,          gt0,             SNOW,           & !i
                    lake_min_elev,  restart,         lakedepth_default,               &
                    lakedepth2d,    savedtke12d,     snowdp2d,        h2osno2d,       & !o
                    snl2d,          t_grnd2d,        t_lake3d,        lake_icefrac3d, &
                    z_lake3d,       dz_lake3d,       t_soisno3d,      h2osoi_ice3d,   &
                    h2osoi_liq3d,   h2osoi_vol3d,    z3d,             dz3d,           &
                    zi3d,           watsat3d,        csol3d,          tkmg3d,         &
                    iswater,        xice,            xice_threshold,  xland,   tsk,   &
                    use_clm_lake,     use_lakedepth,   con_g,           con_rd,         &
                    tkdry3d,        tksatu3d,        im,              prsi,           &
                    lake_rho0,      lake_ht,         oro,             wet,            &
                    xidx,           yidx,            clm_lake_initialized,            &
                    me,             master,          errmsg,          errflg)

   !==============================================================================
   ! This subroutine was first edited by Hongping Gu for coupling
   ! 07/20/2010
   !==============================================================================

  implicit none

  INTEGER, INTENT(OUT) :: errflg
  CHARACTER(*), INTENT(OUT) :: errmsg

  INTEGER , INTENT (IN)    :: im, me, master, iswater
  REAL(KIND_PHYS),     INTENT(IN)  :: xice_threshold, con_g, con_rd
  REAL(KIND_PHYS), DIMENSION( :  ), INTENT(IN)::   XICE
  REAL(KIND_PHYS), DIMENSION( :  ), INTENT(IN)::      TSK, ORO
  INTEGER, DIMENSION( : )  ,INTENT(IN)  :: XLAND
  INTEGER, DIMENSION( : )  ,INTENT(INOUT)  :: clm_lake_initialized

  logical, dimension(:), intent(in) :: use_clm_lake, wet
  !INTEGER , INTENT (IN) :: lakeflag
  !INTEGER , INTENT (INOUT) :: lake_depth_flag
  LOGICAL, INTENT (IN) ::   use_lakedepth

  LOGICAL , INTENT(IN)      ::     restart
  INTEGER, DIMENSION( : ), INTENT(IN)       :: IVGTYP,ISLTYP
  REAL(KIND_PHYS),    DIMENSION( : ), INTENT(IN)    :: SNOW
  REAL(kind_phys),    DIMENSION(:,:), INTENT(IN)       :: gt0, prsi
  real(kind_phys),    intent(in)                                      :: lakedepth_default,lake_min_elev

  REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: lake_ht
  REAL(KIND_PHYS),           DIMENSION( : )         ,INTENT(INOUT)  :: lake_rho0
  real(kind_phys),    dimension(: ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real(kind_phys),    dimension(: ),intent(out)                        :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
                                                                              
  real(kind_phys),    dimension( :, : ),INTENT(out)                    :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real(kind_phys),    dimension( :,-nlevsnow+1: ),INTENT(out)   :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real(kind_phys),    dimension( :,: ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real(kind_phys),    dimension( :,-nlevsnow+0: ),INTENT(out)   :: zi3d            
  integer, intent(in) :: xidx(:),yidx(:)

  !LOGICAL, DIMENSION( : ),intent(out)                      :: lake
  !REAL(KIND_PHYS), OPTIONAL,    DIMENSION( : ), INTENT(IN)    ::  lake_depth ! no separate variable for this in CCPP

  real,   dimension( 1:im,1:nlevsoil )               :: bsw3d,    &
                                                        bsw23d,   &
                                                        psisat3d, &
                                                        vwcsat3d, &
                                                        watdry3d, &
                                                        watopt3d, &
                                                        hksat3d,  &
                                                        sucsat3d, &
                                                        clay3d,   &
                                                        sand3d   
  integer  :: n,i,j,k,ib,lev,bottom      ! indices
  real(kind_phys),dimension(1:im )    :: bd2d               ! bulk density of dry soil material [kg/m^3]
  real(kind_phys),dimension(1:im )    :: tkm2d              ! mineral conductivity
  real(kind_phys),dimension(1:im )    :: xksat2d            ! maximum hydraulic conductivity of soil [mm/s]
  real(kind_phys),dimension(1:im )    :: depthratio2d       ! ratio of lake depth to standard deep lake depth 
  real(kind_phys),dimension(1:im )    :: clay2d             ! temporary
  real(kind_phys),dimension(1:im )    :: sand2d             ! temporary

  real(kind_phys),parameter       :: scalez  = 0.025_kind_phys   ! Soil layer thickness discretization (m)
  logical,parameter        :: arbinit = .true.
  real(kind_phys),parameter           :: defval  = -999.0
  integer                  :: isl
  integer                  :: numb_lak    ! for debug
  character*256 :: message
  real(kind_phys) :: ht, rho0

  integer, parameter :: xcheck=38
  integer, parameter :: ycheck=92
  integer, parameter :: pretend_iswater=17

#ifdef LAKE_DEBUG
  if(me==0) then
    write(0,*) 'clm_lake_init'
  endif
#endif

  errmsg = ''
  errflg = 0

  IF ( pretend_iswater<0 ) THEN
308 format('ERROR: pretend_iswater = ',I0,' < 0')
    write(errmsg,308) pretend_iswater
    errflg = pretend_iswater
  ENDIF

  !IF ( RESTART ) RETURN  <--- should be handled by clm_lake_initialized

  init_const: if(sum(clm_lake_initialized(1:im))==0 .and. any(use_clm_lake)) then
#ifndef EXTRALAKELAYERS   
   !  dzlak(1) = 0.1_kind_phys
   !  dzlak(2) = 1._kind_phys
   !  dzlak(3) = 2._kind_phys
   !  dzlak(4) = 3._kind_phys
   !  dzlak(5) = 4._kind_phys
   !  dzlak(6) = 5._kind_phys
   !  dzlak(7) = 7._kind_phys
   !  dzlak(8) = 7._kind_phys
   !  dzlak(9) = 10.45_kind_phys
   !  dzlak(10)= 10.45_kind_phys
   !
   !  zlak(1) =  0.05_kind_phys
   !  zlak(2) =  0.6_kind_phys
   !  zlak(3) =  2.1_kind_phys
   !  zlak(4) =  4.6_kind_phys
   !  zlak(5) =  8.1_kind_phys
   !  zlak(6) = 12.6_kind_phys
   !  zlak(7) = 18.6_kind_phys
   !  zlak(8) = 25.6_kind_phys
   !  zlak(9) = 34.325_kind_phys
   !  zlak(10)= 44.775_kind_phys
  dzlak(1) = 0.1_kind_phys
  dzlak(2) = 0.1_kind_phys
  dzlak(3) = 0.1_kind_phys
  dzlak(4) = 0.1_kind_phys
  dzlak(5) = 0.1_kind_phys
  dzlak(6) = 0.1_kind_phys
  dzlak(7) = 0.1_kind_phys
  dzlak(8) = 0.1_kind_phys
  dzlak(9) = 0.1_kind_phys
  dzlak(10)= 0.1_kind_phys
 
  zlak(1) =  0.05_kind_phys
  zlak(2) =  0.15_kind_phys
  zlak(3) =  0.25_kind_phys
  zlak(4) =  0.35_kind_phys
  zlak(5) =  0.45_kind_phys
  zlak(6) = 0.55_kind_phys
  zlak(7) = 0.65_kind_phys
  zlak(8) = 0.75_kind_phys
  zlak(9) = 0.85_kind_phys
  zlak(10)= 0.95_kind_phys
#else
  dzlak(1) =0.1_kind_phys
  dzlak(2) =0.25_kind_phys
  dzlak(3) =0.25_kind_phys
  dzlak(4) =0.25_kind_phys
  dzlak(5) =0.25_kind_phys
  dzlak(6) =0.5_kind_phys
  dzlak(7) =0.5_kind_phys
  dzlak(8) =0.5_kind_phys
  dzlak(9) =0.5_kind_phys
  dzlak(10) =0.75_kind_phys
  dzlak(11) =0.75_kind_phys
  dzlak(12) =0.75_kind_phys
  dzlak(13) =0.75_kind_phys
  dzlak(14) =2_kind_phys
  dzlak(15) =2_kind_phys
  dzlak(16) =2.5_kind_phys
  dzlak(17) =2.5_kind_phys
  dzlak(18) =3.5_kind_phys
  dzlak(19) =3.5_kind_phys
  dzlak(20) =3.5_kind_phys
  dzlak(21) =3.5_kind_phys
  dzlak(22) =5.225_kind_phys
  dzlak(23) =5.225_kind_phys
  dzlak(24) =5.225_kind_phys
  dzlak(25) =5.225_kind_phys

  zlak(1) = dzlak(1)/2._kind_phys
  do k = 2,nlevlake
     zlak(k) = zlak(k-1) + (dzlak(k-1)+dzlak(k))/2._kind_phys
  end do
#endif

   ! "0" refers to soil surface and "nlevsoil" refers to the bottom of model soil

   do j = 1, nlevsoil
      zsoi(j) = scalez*(exp(0.5_kind_phys*(j-0.5_kind_phys))-1._kind_phys)    !node depths
   enddo

   dzsoi(1) = 0.5_kind_phys*(zsoi(1)+zsoi(2))             !thickness b/n two interfaces
   do j = 2,nlevsoil-1
      dzsoi(j)= 0.5_kind_phys*(zsoi(j+1)-zsoi(j-1))
   enddo
   dzsoi(nlevsoil) = zsoi(nlevsoil)-zsoi(nlevsoil-1)

   zisoi(0) = 0._kind_phys
   do j = 1, nlevsoil-1
      zisoi(j) = 0.5_kind_phys*(zsoi(j)+zsoi(j+1))         !interface depths
   enddo
   zisoi(nlevsoil) = zsoi(nlevsoil) + 0.5_kind_phys*dzsoi(nlevsoil)
  endif init_const

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  DO i=1,im
    if(.not. use_clm_lake(i) .or. clm_lake_initialized(i)>0) then
      cycle
    endif

    snowdp2d(i)         = snow(i)*0.005               ! SNOW in kg/m^2 and snowdp in m
    h2osno2d(i)         = snow(i) ! mm 

    lakedepth2d(i)             = defval
    snl2d(i)                   = defval
    do k = -nlevsnow+1,nlevsoil
        h2osoi_liq3d(i,k)      = defval
        h2osoi_ice3d(i,k)      = defval
	t_soisno3d(i,k)        = defval
        z3d(i,k)               = defval 
        dz3d(i,k)              = defval                           
    enddo
    do k = 1,nlevlake 
	t_lake3d(i,k)          = defval
        lake_icefrac3d(i,k)    = defval
        z_lake3d(i,k)          = defval
        dz_lake3d(i,k)         = defval
    enddo
    
    if(wet(i)) then
      lake_rho0(i) = -999
    else
      lake_rho0(i) = prsi(i,1)/(con_rd*gt0(i,1))
    endif
    lake_ht(i) = oro(i) ! -999
    if((wet(i) .or. oro(i)>lake_min_elev) .and. xice(i).gt.xice_threshold) then
      lake_icefrac3d(i,1) = xice(i)
    endif
    
    !	t_soisno3d(i,:)      = tsk(i)
    !        t_lake3d(i,:)        = tsk(i)
    !        t_grnd2d(i)          = tsk(i)
    
    z3d(i,:)             = 0.0
    dz3d(i,:)            = 0.0
    zi3d(i,:)            = 0.0
    h2osoi_liq3d(i,:)    = 0.0
    h2osoi_ice3d(i,:)    = 0.0
    lake_icefrac3d(i,:)  = 0.0
    h2osoi_vol3d(i,:)    = 0.0
    snl2d(i)             = 0.0
    if ( use_lakedepth ) then

      if (lakedepth2d(i) > 0.0) then 
        ! lakedepth2d(i)   = lake_depth(i)
      else
        if ( lakedepth_default  > 0.0 ) then
          lakedepth2d(i)   = lakedepth_default
        else 
          lakedepth2d(i)   = spval
        endif
      endif

    else
      if ( lakedepth_default  > 0.0 ) then
        lakedepth2d(i)   = lakedepth_default
      else 
        lakedepth2d(i)   = spval
      endif
    endif

  ENDDO

! FIXME: REMOVE THIS
!
!   ! judge whether the grid is lake grid
!    numb_lak = 0
!    lake_rho0 = -333
!    lake_ht = -333
!    if(me==master) then
!      print *,'x,y at 1 = ',xidx(1),yidx(1)
!    endif
!    skip: if(1==2) then
!        do i=1,im
!         if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
! 138         format('x=',I0,' y=',I0,' xice=',F9.3,' ivegtype=',I0,' pretend_iswater=',I0)
! 139         format('use_clm_lake=',L6,' gt0=',F9.3,' oro=',F9.3,' lake_min_elev=',F9.3)
!             write(0,138) xidx(i),yidx(i),xice(i),ivgtyp(i),pretend_iswater
!             write(0,139) use_clm_lake(i),gt0(i,1),oro(i),lake_min_elev
!         endif
!         ! FIXME: IMPLEMENT THIS SOMEHOW
!         IF (lakefrac(i)==0) THEN
!           if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!             write(0,*) 'not use_clm_lake(i)'
!           endif
!             rho0 = prsi(i,1)/(con_rd*gt0(i,1))
!             ht = oro(i) ! rho0/con_g
!             lake_rho0(i) = rho0
!             lake_ht(i) = ht
!             if(ht>=lake_min_elev) then 
!               if ( xice(i).gt.xice_threshold) then   !mchen
!                 ivgtyp(i) = pretend_iswater
!                 xland(i) = 0. 
!                 lake_icefrac3d(i,1) = xice(i)
!             !        xice(i)=0.0
!                endif
!             endif

!             if(ivgtyp(i)==pretend_iswater.and.ht>=lake_min_elev) then 
!               if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                 write(0,*) 'is water above lake_min_elev'
!               endif
!               lakefrac(i) = 1
!               numb_lak   = numb_lak + 1
!               if ( xice(i).gt.xice_threshold) then   !mchen
!                 if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                   write(0,*) 'is ice too'
!                 endif
!                 ivgtyp(i) = pretend_iswater
!                 xland(i) = 0. 
!                 lake_icefrac3d(i,1) = xice(i)
!                 !xice(i)=0.0
!               else
!                if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                  write(0,*) 'is also ice'
!                endif
!               endif
!             else
!                if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                  write(0,*) 'should not set to lake'
!                endif
!             end if
!         ELSE
!           if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!             write(0,*) 'already use_clm_lake so is lake'
!           endif
!             lake_rho0(i) = -999
!             lake_ht(i) = oro(i) ! -999
!             numb_lak   = numb_lak + 1
!             if ( xice(i).gt.xice_threshold) then   !mchen
!                 if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                   write(0,*)'is ice too'
!                 endif
!                ivgtyp(i) = pretend_iswater
!                xland(i) = 0. 
!                lake_icefrac3d(i,1) = xice(i)
!                !xice(i)=0.0
!             else
!                 if(xidx(i)==xcheck .and. yidx(i)==ycheck) then
!                   write(0,*)'is not ice though'
!                 endif
!             endif
!         ENDIF   ! end if lakeflag=0
!        end do
!     !print *,"the total number of lake grid is :", numb_lak
!        ! initialize lake grid 
!      endif skip

  !!!!!!!!!!!!!!!!!!begin to initialize lake variables!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  DO i = 1,im
 
    if(.not. use_clm_lake(i) .or. clm_lake_initialized(i)>0) then
      cycle
    endif

    ! Soil hydraulic and thermal properties
    isl = ISLTYP(i)   
    if (isl == 0  ) isl = 14
    if (isl == 14 ) isl = isl + 1 
    do k = 1,nlevsoil
      sand3d(i,k)  = sand(isl)
      clay3d(i,k)  = clay(isl)
    enddo

    do k = 1,nlevsoil
      clay2d(i) = clay3d(i,k)
      sand2d(i) = sand3d(i,k)
      watsat3d(i,k) = 0.489_kind_phys - 0.00126_kind_phys*sand2d(i)
      bd2d(i)    = (1._kind_phys-watsat3d(i,k))*2.7e3_kind_phys
      xksat2d(i) = 0.0070556_kind_phys *( 10._kind_phys**(-0.884_kind_phys+0.0153_kind_phys*sand2d(i)) ) ! mm/s
      tkm2d(i) = (8.80_kind_phys*sand2d(i)+2.92_kind_phys*clay2d(i))/(sand2d(i)+clay2d(i))          ! W/(m K)

      bsw3d(i,k) = 2.91_kind_phys + 0.159_kind_phys*clay2d(i)
      bsw23d(i,k) = -(3.10_kind_phys + 0.157_kind_phys*clay2d(i) - 0.003_kind_phys*sand2d(i))
      psisat3d(i,k) = -(exp((1.54_kind_phys - 0.0095_kind_phys*sand2d(i) + 0.0063_kind_phys*(100.0_kind_phys-sand2d(i)  &
           -clay2d(i)))*log(10.0_kind_phys))*9.8e-5_kind_phys)
      vwcsat3d(i,k) = (50.5_kind_phys - 0.142_kind_phys*sand2d(i) - 0.037_kind_phys*clay2d(i))/100.0_kind_phys
      hksat3d(i,k) = xksat2d(i)
      sucsat3d(i,k) = 10._kind_phys * ( 10._kind_phys**(1.88_kind_phys-0.0131_kind_phys*sand2d(i)) )
      tkmg3d(i,k) = tkm2d(i) ** (1._kind_phys- watsat3d(i,k))
      tksatu3d(i,k) = tkmg3d(i,k)*0.57_kind_phys**watsat3d(i,k)
      tkdry3d(i,k) = (0.135_kind_phys*bd2d(i) + 64.7_kind_phys) / (2.7e3_kind_phys - 0.947_kind_phys*bd2d(i))
      csol3d(i,k) = (2.128_kind_phys*sand2d(i)+2.385_kind_phys*clay2d(i)) / (sand2d(i)+clay2d(i))*1.e6_kind_phys  ! J/(m3 K)
      watdry3d(i,k) = watsat3d(i,k) * (316230._kind_phys/sucsat3d(i,k)) ** (-1._kind_phys/bsw3d(i,k))
      watopt3d(i,k) = watsat3d(i,k) * (158490._kind_phys/sucsat3d(i,k)) ** (-1._kind_phys/bsw3d(i,k))
    end do
    if (lakedepth2d(i) == spval) then
      lakedepth2d(i) = zlak(nlevlake) + 0.5_kind_phys*dzlak(nlevlake)
      z_lake3d(i,1:nlevlake) = zlak(1:nlevlake)
      dz_lake3d(i,1:nlevlake) = dzlak(1:nlevlake)
    else
      depthratio2d(i) = lakedepth2d(i) / (zlak(nlevlake) + 0.5_kind_phys*dzlak(nlevlake)) 
      z_lake3d(i,1) = zlak(1)
      dz_lake3d(i,1) = dzlak(1)
      dz_lake3d(i,2:nlevlake) = dzlak(2:nlevlake)*depthratio2d(i)
      z_lake3d(i,2:nlevlake) = zlak(2:nlevlake)*depthratio2d(i) + dz_lake3d(i,1)*(1._kind_phys - depthratio2d(i))
    end if
    ! initial t_lake3d here
    t_soisno3d(i,1)      = tsk(i)
    t_lake3d(i,1)        = tsk(i)
    t_grnd2d(i)          = 277.0
    do k = 2, nlevlake
      if(z_lake3d(i,k).le.depth_c) then 
        t_soisno3d(i,k)=tsk(i)+(277.0-tsk(i))/depth_c*z_lake3d(i,k)
        t_lake3d(i,k)=tsk(i)+(277.0-tsk(i))/depth_c*z_lake3d(i,k)
      else
	t_soisno3d(i,k)      = 277.0
        t_lake3d(i,k)        = 277.0
      end if
    enddo
    !end initial t_lake3d here
    z3d(i,1:nlevsoil) = zsoi(1:nlevsoil)
    zi3d(i,0:nlevsoil) = zisoi(0:nlevsoil)
    dz3d(i,1:nlevsoil) = dzsoi(1:nlevsoil)
    savedtke12d(i) = tkwat ! Initialize for first timestep.


    if (snowdp2d(i) < 0.01_kind_phys) then
      snl2d(i) = 0
      dz3d(i,-nlevsnow+1:0) = 0._kind_phys
      z3d (i,-nlevsnow+1:0) = 0._kind_phys
      zi3d(i,-nlevsnow+0:0) = 0._kind_phys
    else
      if ((snowdp2d(i) >= 0.01_kind_phys) .and. (snowdp2d(i) <= 0.03_kind_phys)) then
        snl2d(i) = -1
        dz3d(i,0)  = snowdp2d(i)
      else if ((snowdp2d(i) > 0.03_kind_phys) .and. (snowdp2d(i) <= 0.04_kind_phys)) then
        snl2d(i) = -2
        dz3d(i,-1) = snowdp2d(i)/2._kind_phys
        dz3d(i, 0) = dz3d(i,-1)
      else if ((snowdp2d(i) > 0.04_kind_phys) .and. (snowdp2d(i) <= 0.07_kind_phys)) then
        snl2d(i) = -2
        dz3d(i,-1) = 0.02_kind_phys
        dz3d(i, 0) = snowdp2d(i) - dz3d(i,-1)
      else if ((snowdp2d(i) > 0.07_kind_phys) .and. (snowdp2d(i) <= 0.12_kind_phys)) then
        snl2d(i) = -3
        dz3d(i,-2) = 0.02_kind_phys
        dz3d(i,-1) = (snowdp2d(i) - 0.02_kind_phys)/2._kind_phys
        dz3d(i, 0) = dz3d(i,-1)
      else if ((snowdp2d(i) > 0.12_kind_phys) .and. (snowdp2d(i) <= 0.18_kind_phys)) then
        snl2d(i) = -3
        dz3d(i,-2) = 0.02_kind_phys
        dz3d(i,-1) = 0.05_kind_phys
        dz3d(i, 0) = snowdp2d(i) - dz3d(i,-2) - dz3d(i,-1)
      else if ((snowdp2d(i) > 0.18_kind_phys) .and. (snowdp2d(i) <= 0.29_kind_phys)) then
        snl2d(i) = -4
        dz3d(i,-3) = 0.02_kind_phys
        dz3d(i,-2) = 0.05_kind_phys
        dz3d(i,-1) = (snowdp2d(i) - dz3d(i,-3) - dz3d(i,-2))/2._kind_phys
        dz3d(i, 0) = dz3d(i,-1)
      else if ((snowdp2d(i) > 0.29_kind_phys) .and. (snowdp2d(i) <= 0.41_kind_phys)) then
        snl2d(i) = -4
        dz3d(i,-3) = 0.02_kind_phys
        dz3d(i,-2) = 0.05_kind_phys
        dz3d(i,-1) = 0.11_kind_phys
        dz3d(i, 0) = snowdp2d(i) - dz3d(i,-3) - dz3d(i,-2) - dz3d(i,-1)
      else if ((snowdp2d(i) > 0.41_kind_phys) .and. (snowdp2d(i) <= 0.64_kind_phys)) then
        snl2d(i) = -5
        dz3d(i,-4) = 0.02_kind_phys
        dz3d(i,-3) = 0.05_kind_phys
        dz3d(i,-2) = 0.11_kind_phys
        dz3d(i,-1) = (snowdp2d(i) - dz3d(i,-4) - dz3d(i,-3) - dz3d(i,-2))/2._kind_phys
        dz3d(i, 0) = dz3d(i,-1)
      else if (snowdp2d(i) > 0.64_kind_phys) then
        snl2d(i) = -5
        dz3d(i,-4) = 0.02_kind_phys
        dz3d(i,-3) = 0.05_kind_phys
        dz3d(i,-2) = 0.11_kind_phys
        dz3d(i,-1) = 0.23_kind_phys
        dz3d(i, 0)=snowdp2d(i)-dz3d(i,-4)-dz3d(i,-3)-dz3d(i,-2)-dz3d(i,-1)
      endif
    end if

    do k = 0, snl2d(i)+1, -1
      z3d(i,k)    = zi3d(i,k) - 0.5_kind_phys*dz3d(i,k)
      zi3d(i,k-1) = zi3d(i,k) - dz3d(i,k)
    end do

    ! 3:subroutine makearbinit

    if (snl2d(i) < 0) then
      do k = snl2d(i)+1, 0
        ! Be careful because there may be new snow layers with bad temperatures like 0 even if
        ! coming from init. con. file.
        if(arbinit .or. t_soisno3d(i,k) > 300 .or. t_soisno3d(i,k) < 200) t_soisno3d(i,k) = 250._kind_phys
      enddo
    end if

    do k = 1, nlevsoil
      if(arbinit .or. t_soisno3d(i,k) > 1000 .or. t_soisno3d(i,k) < 0) t_soisno3d(i,k) = t_lake3d(i,nlevlake)
    end do

    do k = 1, nlevlake
      if(arbinit .or. lake_icefrac3d(i,k) > 1._kind_phys .or. lake_icefrac3d(i,k) < 0._kind_phys) then
        if(t_lake3d(i,k) >= tfrz) then
          lake_icefrac3d(i,k) = 0._kind_phys
        else
          lake_icefrac3d(i,k) = 1._kind_phys
        end if
      end if
    end do

    do k = 1,nlevsoil
      if (arbinit .or. h2osoi_vol3d(i,k) > 10._kind_phys .or. h2osoi_vol3d(i,k) < 0._kind_phys) h2osoi_vol3d(i,k) = 1.0_kind_phys
      h2osoi_vol3d(i,k) = min(h2osoi_vol3d(i,k),watsat3d(i,k))

      ! soil layers
      if (t_soisno3d(i,k) <= tfrz) then
        h2osoi_ice3d(i,k)  = dz3d(i,k)*denice*h2osoi_vol3d(i,k)
        h2osoi_liq3d(i,k) = 0._kind_phys
      else
        h2osoi_ice3d(i,k) = 0._kind_phys
        h2osoi_liq3d(i,k) = dz3d(i,k)*denh2o*h2osoi_vol3d(i,k)
      endif
    enddo

    do k = -nlevsnow+1, 0
      if (k > snl2d(i)) then
        h2osoi_ice3d(i,k) = dz3d(i,k)*bdsno
        h2osoi_liq3d(i,k) = 0._kind_phys
      end if
    end do

    clm_lake_initialized(i) = 1
  ENDDO

END SUBROUTINE lakeini

END MODULE clm_lake
