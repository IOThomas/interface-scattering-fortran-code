!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! SUBMODULE:  Module name
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

submodule (io_variables) io_variable_proc
    implicit none
contains

    module procedure assign_name
        this%name = name
    end procedure assign_name

    module procedure assign_units
        this%units = units
    end procedure assign_units

end submodule