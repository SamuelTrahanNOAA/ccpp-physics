!>\file gsd_chem_config_wrapper.F90
!! This file is the GSDChem configuration wrapper with CCPP coupling to FV3
!! Haiqin.Li@noaa.gov 07/2020

 module gsd_chem_config_wrapper

   implicit none

   private

   public :: gsd_chem_config_wrapper_init, gsd_chem_config_wrapper_run, gsd_chem_config_wrapper_finalize

contains

!> \brief Brief description of the subroutine
!!
      subroutine gsd_chem_config_wrapper_init(ntso2,ntsulf,ntdms,ntmsa,ntco, &
           ntpp25,ntbc1,ntbc2,ntoc1,ntoc2,ntdust1,ntdust2,ntdust3,ntdust4,   &
           ntdust5,ntss1,ntss2,ntss3,ntss4,ntss5,ntpp10,chem_opt,errmsg,errflg)
        use gsd_chem_config, config_chem_opt=>chem_opt
        implicit none
        integer, intent(in) :: ntso2,ntsulf,ntdms,ntmsa,ntco,ntpp25,ntbc1, &
             ntbc2,ntoc1,ntoc2,ntdust1,ntdust2,ntdust3,ntdust4,ntdust5,    &
             ntss1,ntss2,ntss3,ntss4,ntss5,ntpp10,chem_opt
        character(len=*), intent(out) :: errmsg
        integer,          intent(out) :: errflg
        
        errmsg = ''
        errflg = 0

        print *,'gsd_chem_config_wrapper_init'

        config_chem_opt = chem_opt

        num_chem=20

        if(chem_opt == CHEM_OPT_GOCART_CO) then
           num_chem = num_chem+1

           num_ebu = 8
           num_ebu_in = 8
           num_emis_ant = 8

           p_ebu_co = 8
           p_ebu_in_co = 8
        else
           num_ebu = 7
           num_ebu_in = 7
           num_emis_ant = 7

           ! CHEM_OPT_GOCART_RACM
           p_ebu_co = 5
           p_ebu_in_co = 5
        endif

        call set_and_check(p_so2,ntso2,'The so2 tracer is mandatory.')
        call set_and_check(p_sulf,ntsulf,'The sulf tracer is mandatory.')
        call set_and_check(p_dms,ntdms,'The dms tracer is mandatory.')
        call set_and_check(p_msa,ntmsa,'The msa tracer is mandatory.')
        if(chem_opt == CHEM_OPT_GOCART_CO) then
           call set_and_check(p_co,ntco,'The co tracer is mandatory when chem_opt==CHEM_OPT_GOCART_CO')
        endif
        call set_and_check(p_p25,ntpp25,'The pp25 tracer is mandatory.')
        call set_and_check(p_bc1,ntbc1,'The bc1 tracer is mandatory.')
        call set_and_check(p_bc2,ntbc2,'The bc2 tracer is mandatory.')
        call set_and_check(p_oc1,ntoc1,'The oc1 tracer is mandatory.')
        call set_and_check(p_oc2,ntoc2,'The oc2 tracer is mandatory.')
        call set_and_check(p_dust_1,ntdust1,'The dust1 tracer is mandatory.')
        call set_and_check(p_dust_2,ntdust2,'The dust2 tracer is mandatory.')
        call set_and_check(p_dust_3,ntdust3,'The dust3 tracer is mandatory.')
        call set_and_check(p_dust_4,ntdust4,'The dust4 tracer is mandatory.')
        call set_and_check(p_dust_5,ntdust5,'The dust5 tracer is mandatory.')
        call set_and_check(p_seas_1,ntss1,'The ss1 tracer is mandatory.')
        call set_and_check(p_seas_2,ntss2,'The ss2 tracer is mandatory.')
        call set_and_check(p_seas_3,ntss3,'The ss3 tracer is mandatory.')
        call set_and_check(p_seas_4,ntss4,'The ss4 tracer is mandatory.')
        call set_and_check(p_seas_5,ntss5,'The ss5 tracer is mandatory.')
        call set_and_check(p_p10,ntpp10,'The pp10 tracer is mandatory.')

      contains
        subroutine set_and_check(p_index,nt_index,mandatory)
          implicit none
          integer, intent(in) :: nt_index
          integer, intent(out) :: p_index
          character(len=*), intent(in), optional :: mandatory
          
          if(nt_index<1) then
             if(present(mandatory)) then
                errmsg = trim(mandatory)
                errflg = 1
             endif
             return
          else if(nt_index/=ntso2 .and. nt_index<=ntso2) then
             errmsg = 'All other tracers must be after so2.'
             errflg = 1
             return
          endif

          p_index = nt_index-ntso2+1

        end subroutine set_and_check
      end subroutine gsd_chem_config_wrapper_init

!> \brief Brief description of the subroutine
!!
!! \section arg_table_gsd_chem_config_wrapper_finalize Argument Table
!!
      subroutine gsd_chem_config_wrapper_finalize()
      end subroutine gsd_chem_config_wrapper_finalize

!> \defgroup gsd_chem_config_group GSD Chem config wrapper Module
!! This is the gsd chemistry
!>\defgroup gsd_chem_config_wrapper GSD Chem config wrapper Module  
!> \ingroup gsd_chem_config_group
!! This is the GSD Chem config wrapper Module
!! \section arg_table_gsd_chem_config_wrapper_run Argument Table
!! \htmlinclude gsd_chem_config_wrapper_run.html
!!
!>\section gsd_chem_config_wrapper GSD Chemistry Scheme General Algorithm
!> @{
      subroutine gsd_chem_config_wrapper_run()
      end subroutine gsd_chem_config_wrapper_run
!> @}
    end module gsd_chem_config_wrapper
