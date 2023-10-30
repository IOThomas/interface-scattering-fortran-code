!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! MODULE:  io_variables
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

module io_variables
    use constants, only: real12
    implicit none

    type, abstract :: io_variable
        character(len=20) :: name, units
    contains
        private
        procedure, public :: set_name => assign_name
        procedure, public :: set_units => assign_units
    end type

    type, extends(io_variable) :: io_integer
        integer :: value
    contains
        private
        procedure, public :: set_value => assign_value_integer
        procedure, public :: write_out_fi
        procedure, public :: write_out_ui
        generic :: write(formatted) => write_out_fi
        generic :: write(unformatted) => write_out_ui
    end type

    type, extends(io_variable) :: io_real
        real(real12) :: value
    contains
        private
        procedure, public :: set_value => assign_value_real
        procedure, public :: write_out_fr
        procedure, public :: write_out_ur
        generic :: write(formatted) => write_out_fr
        generic :: write(unformatted) => write_out_ur
    end type

    type, extends(io_variable) :: io_complex
        complex(real12) :: value
    contains
        private
        procedure, public :: set_value => assign_value_complex
        procedure, public :: write_out_fc
        procedure, public :: write_out_uc
        generic :: write(formatted) => write_out_fc
        generic :: write(unformatted) => write_out_uc
    end type

    type, extends(io_variable) :: io_logical
        logical :: value
    contains
        private
        procedure, public :: set_value => assign_value_logical
        procedure, public :: write_out_fl
        procedure, public :: write_out_ul
        generic :: write(formatted) => write_out_fl
        generic :: write(unformatted) => write_out_ul
    end type

    type, extends(io_variable) :: io_string
        character(len=50) :: value
    contains
        private
        procedure, public :: set_value => assign_value_string
        procedure, public :: write_out_fs
        procedure, public :: write_out_us
        generic :: write(formatted) => write_out_fs
        generic :: write(unformatted) => write_out_us
    end type

    interface 
        module subroutine assign_name(this, name)
            class(io_variable) :: this
            character(len=*),intent(in) :: name
        end subroutine
        module subroutine assign_units(this, units)
            class(io_variable) :: this
            character(len=*),intent(in) :: units
        end subroutine
        module subroutine assign_value_integer(this, value)
            class(io_integer) :: this
            integer, intent(in) :: value
        end subroutine
        module subroutine assign_value_real(this, value)
            class(io_real) :: this
            real(real12), intent(in) :: value
        end subroutine
        module subroutine assign_value_complex(this, value)
            class(io_complex) :: this
            complex(real12), intent(in) :: value
        end subroutine
        module subroutine assign_value_logical(this, value)
            class(io_logical) :: this
            logical, intent(in) :: value
        end subroutine
        module subroutine assign_value_string(this, value)
            class(io_string) :: this
            character(len=*), intent(in) :: value
        end subroutine
        module subroutine write_out_fi(dtv, unit, iotype, vlist, iostat, iomag)
            class(io_integer), intent(in) :: dtv
            integer, intent(in) :: unit
            character(len=*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_ui(dtv, unit, iostat, iomag)
            class(io_integer), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_fr(dtv, unit, iotype, vlist, iostat, iomag)
            class(io_real), intent(in) :: dtv
            integer, intent(in) :: unit
            character(len=*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_ur(dtv, unit, iostat, iomag)
            class(io_real), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_fc(dtv, unit, iotype, vlist, iostat, iomag)
            class(io_complex), intent(in) :: dtv
            integer, intent(in) :: unit
            character(len=*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_uc(dtv, unit, iostat, iomag)
            class(io_complex), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_fl(dtv, unit, iotype, vlist, iostat, iomag)
            class(io_logical), intent(in) :: dtv
            integer, intent(in) :: unit
            character(len=*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_ul(dtv, unit, iostat, iomag)
            class(io_logical), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_fs(dtv, unit, iotype, vlist, iostat, iomag)
            class(io_string), intent(in) :: dtv
            integer, intent(in) :: unit
            character(len=*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
        module subroutine write_out_us(dtv, unit, iostat, iomag)
            class(io_string), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomag
        end subroutine
    end interface

end module io_variables