!>\file h2o_def.f 
!! This file contains array definition in H2O scheme.

!>\ingroup mod_GFS_phys_time_vary
!! This module defines arrays in H2O scheme.
      module h2o_def

!> \section arg_table_h2o_def
!! \htmlinclude h2o_def.html
!!

      use machine , only : kind_phys
      implicit none
      private
      integer, public :: levh2o=-1, h2o_coeff=-1
      real(kind=kind_phys), allocatable, public :: h2o_pres(:)
      end module h2o_def
