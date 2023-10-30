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

submodule (io_variables) io_logical
    implicit none
contains

    module procedure assign_value_logical
        this%value = value
    end 

    module procedure write_out_fl
        character(len=30) :: format
        
        if (dtv%units=="") then 
            write(format, '(a, i2, a,i2, a)') '(a', vlist(1), ',a,l',vlist(2),')'
            write(unit, fmt=format, iostat=iostat) dtv%name,': ',dtv%value
        else if (dtv%name=="") then
            write(format, '(a, i2, a,i2, a)') '(l', vlist(2), ',a, a',vlist(3),')'
            write(unit, fmt=format, iostat=iostat) dtv%value, ' ', dtv%units
        else if ((dtv%name=="").and.(dtv%units=="")) then
            write(format, '(a, i2,a)') '(l',vlist(2),')'
            write(unit, fmt=format, iostat=iostat) dtv%value
        else 
            write(format, '(a,i2,a,i2, a,i2,a)') '(a', vlist(1),'a,l',vlist(2),',a, a', vlist(3),')'
            write(unit, fmt=format, iostat=iostat) dtv%name, ': ',dtv%value, ' ', dtv%units
        end if
    end

    module procedure write_out_ul
        write(unit, fmt=*, iostat=iostat) dtv%name, ':', dtv%value, ' ',dtv%units
    end

end submodule