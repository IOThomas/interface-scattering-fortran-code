!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! MODULE:  Module name
!
!> @author
!> Author Name}
!
! DESCRIPTION: 
!>  Short module description
!
! REVISION HISTORY:
! dd Mmm yyyy - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module constants
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter, public :: real12 = real128

    real(real12), parameter, public :: zero = 0.0_real12, one = 1.0_real12, two = 2.0_real12
    complex(real12), parameter, public :: czero=(zero, zero)

    real(real12), parameter, public:: pi = two*acos(zero)

end module